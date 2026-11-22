#!/usr/bin/env python3
"""Live check of hilda -r against real providers. Spends tokens.

Tasks, run per provider:

  sum    The top agent must delegate two line counts and reply with their
         sum, 42. Checks the answer, that two sub-agents ran, and whether
         both agent calls came in one reply (concurrent).
  broad  Summarise four files, each with a codename, with and without -r.
         Compares tokens, turns, wall time and correctness. The files are
         tiny, so this measures the fixed cost of a sub-agent.
  large  Find a codename buried in each of four files of about 6,000
         estimated tokens, with and without -r, under -M read-only and
         --context-budget 12000. One agent cannot hold all four files; a
         sub-agent can hold its one. Not part of "all": a run can use
         several hundred thousand tokens. Use --budget-tokens 400000 with it.

Providers are skipped when their key variable is unset. hilda reaches
Anthropic and Gemini through their OpenAI-compatible endpoints.

Usage:
  scripts/live_recurse.py                      # every provider with a key, both tasks
  scripts/live_recurse.py anthropic gemini     # these providers
  scripts/live_recurse.py --task sum --model openai=gpt-6-luna
  scripts/live_recurse.py openai --extra openai="--reasoning none"
  scripts/live_recurse.py openai --task large --budget-tokens 400000
  scripts/live_recurse.py --list               # show providers, models and keys; run nothing

Raw --stream-json output of every run is kept under --out.
"""

import argparse
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

# name: (key variable, hilda arguments, default model). Model ids are
# defaults only; override with --model NAME=ID or HILDA_LIVE_<NAME>_MODEL.
PROVIDERS = {
    "openrouter": ("OPENROUTER_API_KEY", ["-P", "openrouter"], "anthropic/claude-opus-5-5"),
    "openai": ("OPENAI_API_KEY", ["-P", "openai", "--base-url", "https://api.openai.com/v1"], "gpt-6-luna"),
    "anthropic": (
        "ANTHROPIC_API_KEY",
        ["-P", "openai", "--base-url", "https://api.anthropic.com/v1", "--api-key-env", "ANTHROPIC_API_KEY"],
        "claude-opus-5-5",
    ),
    "gemini": (
        "GEMINI_API_KEY",
        ["-P", "openai", "--base-url", "https://generativelanguage.googleapis.com/v1beta/openai", "--api-key-env", "GEMINI_API_KEY"],
        "gemini-3.8-flash",
    ),
}

SUM_PROMPT = (
    "Use the agent tool to delegate two tasks, both in the same reply: one sub-agent counts "
    "the lines of a.txt, the other counts the lines of b.txt. Do not count them yourself. "
    "When both answer, reply with only the sum as a number."
)

CODENAMES = {"alpha.py": "amber", "beta.py": "basalt", "gamma.py": "cobalt", "delta.py": "dune"}
BROAD_PROMPT = (
    "Summarise each of the four Python files in this directory (alpha.py, beta.py, gamma.py, "
    "delta.py) in one line each. Include each file's codename, which is in its docstring."
)
BROAD_DELEGATE = " Delegate one sub-agent per file, all in one reply."

LARGE = {"big_a.py": "heron", "big_b.py": "quartz", "big_c.py": "saffron", "big_d.py": "tundra"}
LARGE_CHARS = 24000
LARGE_PROMPT = (
    "Each of big_a.py, big_b.py, big_c.py and big_d.py contains exactly one line of the form "
    "'# Codename: WORD' somewhere in it. Find each file's codename by reading the files. "
    "Reply with four lines, FILE: WORD."
)
LARGE_ARGS = ["-M", "read-only", "--context-budget", "12000"]


def fixtures(root: Path) -> None:
    """Files for both tasks: 17 + 25 lines, and four modules with codenames."""
    root.mkdir(parents=True, exist_ok=True)
    (root / "a.txt").write_text("".join(f"line {i}\n" for i in range(17)))
    (root / "b.txt").write_text("".join(f"row {i}\n" for i in range(25)))
    for i, (name, code) in enumerate(CODENAMES.items()):
        body = "\n".join(f"def f{j}(x):\n    return x * {i + j}\n" for j in range(6))
        (root / name).write_text(f'"""Module {name}. Codename: {code}. Multiplies inputs."""\n\n{body}')
    # Codenames at 40% to 85% of each file, so no single page holds two.
    for i, (name, code) in enumerate(LARGE.items()):
        lines, size, j = [], 0, 0
        while size < LARGE_CHARS:
            line = f"def g{j}(x):\n    return x * {j % 97} + {(j * 7) % 13}\n"
            lines.append(line)
            size += len(line)
            j += 1
        lines.insert(len(lines) * (40 + 15 * i) // 100, f"# Codename: {code}\n")
        (root / name).write_text("".join(lines))


def run_hilda(hilda, provider, model, args, prompt, cwd, state, timeout):
    """One headless --stream-json run. Returns (result dict or None, events, stderr, seconds)."""
    _, base, _ = PROVIDERS[provider]
    cmd = [hilda, *base, "-m", model, "--no-session", "--stream-json", *args, "-p", prompt]
    env = {**os.environ, "XDG_STATE_HOME": str(state)}
    start = time.monotonic()
    try:
        p = subprocess.run(cmd, cwd=cwd, env=env, capture_output=True, text=True, timeout=timeout)
        out, err = p.stdout, p.stderr
    except subprocess.TimeoutExpired as e:
        out = e.stdout.decode() if isinstance(e.stdout, bytes) else (e.stdout or "")
        err = (e.stderr.decode() if isinstance(e.stderr, bytes) else (e.stderr or "")) + f"\n[timeout after {timeout}s]"
    secs = time.monotonic() - start
    events = []
    for line in out.splitlines():
        try:
            events.append(json.loads(line))
        except json.JSONDecodeError:
            pass
    result = events[-1] if events and events[-1].get("type") == "result" else None
    return result, events, err, secs


def tokens(usage):
    return (usage or {}).get("prompt_tokens", 0) + (usage or {}).get("completion_tokens", 0)


def agent_calls(result):
    """Assistant messages of the top agent, as lists of agent call ids."""
    return [
        [c["id"] for c in m.get("tool_calls") or [] if c["function"]["name"] == "agent"]
        for m in (result or {}).get("messages", [])
        if m.get("role") == "assistant"
    ]


def hint(err):
    """A pointer for errors this check is meant to find."""
    if re.search(r"max_tokens|max_completion_tokens", err):
        return "provider rejected max_tokens: see docs/dev/recursion.md, Limits"
    if "stream_options" in err:
        return "provider rejected stream_options"
    return ""


def check_sum(result, events, err):
    """(name, passed, required) for the sum task."""
    calls = agent_calls(result)
    labels = {e["agent"] for e in events if "agent" in e}
    family = (result or {}).get("family") or {}
    agent_results = [
        m["content"] for m in (result or {}).get("messages", [])
        if m.get("role") == "tool" and m["tool_call_id"] in {i for c in calls for i in c}
    ]
    return [
        ("finished", bool(result) and result.get("stop") == "finished", True),
        ("answer is 42", bool(result) and re.search(r"\b42\b", result.get("result", "")) is not None, True),
        ("two agent calls", sum(map(len, calls)) >= 2, True),
        ("no agent errors", bool(agent_results) and not any(r.startswith("error:") for r in agent_results), True),
        ("children used tokens", tokens(family.get("usage")) > tokens((result or {}).get("usage")), True),
        ("sub-agent tool calls seen", len(labels) >= 2, False),
        ("both calls in one reply", any(len(c) >= 2 for c in calls), False),
    ]


def report_checks(checks):
    for name, ok, required in checks:
        print(f"    [{'ok' if ok else 'FAIL' if required else 'no'}] {name}{'' if required else ' (info)'}")
    return all(ok for _, ok, req in checks if req)


def summary(result, secs):
    if not result:
        return f"{secs:.0f}s, no result"
    family = result.get("family") or {}
    usage = family.get("usage") or result.get("usage") or {}
    cost = usage.get("cost")
    return (
        f"{secs:.0f}s, top-agent turns {result.get('turns')}, "
        f"{tokens(usage)} tokens" + (f", ${cost:.4f}" if cost is not None else "")
        + (f", budget {family.get('spent_tokens')}/{family.get('budget_tokens')}" if family else "")
    )


def save(out, name, events, err):
    (out / f"{name}.jsonl").write_text("".join(json.dumps(e) + "\n" for e in events))
    (out / f"{name}.stderr").write_text(err)


def compare(hilda, name, model, extra, recurse, work, state, out, timeout, task, args, prompt, delegate, codes):
    """Run a task with one agent, then with -r; print both and the ratios. True if both found every code."""
    rows = {}
    for label, more, text in (("single", [], prompt), ("-r", recurse, prompt + delegate)):
        r, ev, err, secs = run_hilda(hilda, name, model, args + more + extra, text, work, state, timeout)
        save(out, f"{name}-{task}{'-r' if more else ''}", ev, err)
        found = [c for c in codes if r and c in r.get("result", "").lower()]
        reads = sum(1 for e in ev if e.get("type") == "tool_call" and e.get("name") == "read")
        trims = sum(1 for e in ev if e.get("type") == "context_trimmed")
        rows[label] = (r, secs, found)
        print(f"  {task} {label:6}: {summary(r, secs)}, codenames {len(found)}/{len(codes)}, reads {reads}, trims {trims}")
        if r and r.get("error"):
            print(f"    error: {r['error'][:300]}")
    single, multi = rows["single"][0], rows["-r"][0]
    if single and multi:
        t1 = tokens(single.get("usage"))
        t2 = tokens(((multi.get("family") or {}).get("usage")) or multi.get("usage"))
        if t1:
            print(f"    -r used {t2 / t1:.2f}x the tokens and {rows['-r'][1] / max(rows['single'][1], 0.1):.2f}x the time")
    return all(len(f) == len(codes) and r and r.get("stop") == "finished" for r, _, f in rows.values())


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("providers", nargs="*", metavar="PROVIDER",
                    help=f"any of {', '.join(PROVIDERS)}; default: every one whose key is set")
    ap.add_argument("--task", choices=["sum", "broad", "large", "all"], default="all",
                    help="all runs sum and broad; large runs only when named")
    ap.add_argument("--model", action="append", default=[], metavar="NAME=ID", help="model for a provider")
    ap.add_argument("--extra", action="append", default=[], metavar="NAME=ARGS",
                    help='more hilda arguments for a provider\'s runs, e.g. openai="--reasoning none"')
    ap.add_argument("--budget-tokens", type=int, default=200000)
    ap.add_argument("--max-depth", type=int, default=2)
    ap.add_argument("--timeout", type=int, default=600, help="seconds per run")
    ap.add_argument("--hilda", help="binary; default: cabal list-bin hilda")
    ap.add_argument("--out", type=Path, help="where to keep raw output; default: a new temp directory")
    ap.add_argument("--list", action="store_true", help="show providers and exit")
    a = ap.parse_args()

    models = {n: os.environ.get(f"HILDA_LIVE_{n.upper()}_MODEL", m) for n, (_, _, m) in PROVIDERS.items()}
    for spec in a.model:
        name, _, model = spec.partition("=")
        if name not in PROVIDERS or not model:
            sys.exit(f"--model {spec}: expected NAME=ID with NAME in {', '.join(PROVIDERS)}")
        models[name] = model
    extra = {n: [] for n in PROVIDERS}
    for spec in a.extra:
        name, _, args = spec.partition("=")
        if name not in PROVIDERS:
            sys.exit(f"--extra {spec}: expected NAME=ARGS with NAME in {', '.join(PROVIDERS)}")
        extra[name] += shlex.split(args)

    unknown = [p for p in a.providers if p not in PROVIDERS]
    if unknown:
        sys.exit(f"unknown provider {', '.join(unknown)}: expected {', '.join(PROVIDERS)}")
    chosen = a.providers or list(PROVIDERS)
    if a.list:
        for n in PROVIDERS:
            key = PROVIDERS[n][0]
            print(f"{n:11} {models[n]:32} ${key} {'set' if os.environ.get(key) else 'unset'}")
        return 0

    hilda = a.hilda or subprocess.run(["cabal", "list-bin", "hilda"], capture_output=True, text=True,
                                      cwd=Path(__file__).resolve().parent.parent).stdout.strip()
    if not hilda or not shutil.which(hilda):
        sys.exit("hilda binary not found: run make build, or pass --hilda PATH")

    out = a.out or Path(tempfile.mkdtemp(prefix="hilda-live-"))
    out.mkdir(parents=True, exist_ok=True)
    work = out / "work"
    fixtures(work)
    recurse = ["-r", "--budget-tokens", str(a.budget_tokens), "--max-depth", str(a.max_depth)]
    print(f"hilda: {hilda}\noutput: {out}\n")

    failed = 0
    for name in chosen:
        key = PROVIDERS[name][0]
        if not os.environ.get(key):
            print(f"{name}: skipped, ${key} unset\n")
            continue
        model = models[name]
        state = out / f"state-{name}"
        print(f"{name} ({model})")

        if a.task in ("sum", "all"):
            r, ev, err, secs = run_hilda(hilda, name, model, recurse + extra[name], SUM_PROMPT, work, state, a.timeout)
            save(out, f"{name}-sum", ev, err)
            print(f"  sum: {summary(r, secs)}")
            if r and r.get("error"):
                print(f"    error: {r['error'][:300]}")
            if hint(err + json.dumps(r or {})):
                print(f"    hint: {hint(err + json.dumps(r or {}))}")
            failed += not report_checks(check_sum(r, ev, err))

        if a.task in ("broad", "all"):
            failed += not compare(hilda, name, model, extra[name], recurse, work, state, out, a.timeout,
                                  "broad", [], BROAD_PROMPT, BROAD_DELEGATE, CODENAMES.values())
        if a.task == "large":
            failed += not compare(hilda, name, model, extra[name], recurse, work, state, out, a.timeout,
                                  "large", LARGE_ARGS, LARGE_PROMPT, BROAD_DELEGATE, LARGE.values())
        print()

    print(f"raw output: {out}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
