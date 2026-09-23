# hilda

A coding agent in Haskell. It talks to any OpenAI-compatible chat-completions
server or to OpenRouter, and runs headless or as a REPL.

## Build

Requires GHC 9.10 and cabal (both via ghcup) and `libgmp-dev`.

```sh
make build     # cabal build all
make test      # cabal test
make install   # copies hilda to ~/.local/bin
```

## Providers and models

| Provider | Selected when | Key | Base URL |
|-|-|-|-|
| `openrouter` | `OPENROUTER_API_KEY` is set, or `-P openrouter` | `OPENROUTER_API_KEY` (required) | `https://openrouter.ai/api/v1` |
| `openai` | otherwise, or `-P openai` | `OPENAI_API_KEY` (optional) | `OPENAI_BASE_URL`, else `https://api.openai.com/v1` |

`--base-url` and `--api-key-env VAR` override either. The `openai` provider
covers local servers such as Ollama or llama.cpp; they usually need no key.

Pass `-m NAME` once per provider. hilda saves the last model used with each
provider in `$XDG_STATE_HOME/hilda/models.json` (default
`~/.local/state/hilda/`) and reuses it when `-m` is absent. REPL `/model`
changes are saved too.

## Usage

```sh
hilda -m vendor/model                           # REPL
hilda -p "fix the failing test"                 # one prompt, answer on stdout
git diff | hilda -p -                           # prompt from stdin
hilda -p "list TODOs" --json                    # one JSON object
hilda -p "list TODOs" --stream-json             # one JSON line per event
```

Text mode prints the answer on stdout and tool activity on stderr.
`--stream-json` prints `text`, `tool_call` and `tool_result` lines as they
happen. Its last line is the `result` object that `--json` prints alone.

Exit codes: 0 finished, 1 error, 2 stopped by `--max-turns` (default 50).

## Tools and modes

Tools: `read`, `write`, `edit` (exact, unique string replacement), `bash`
(`bash -c`, default timeout 120 s, whole process group killed on timeout).

| `--mode` | read | write, edit, bash |
|-|-|-|
| `yolo` (default) | run | run |
| `ask` | run | confirm on the terminal; refused without one |
| `read-only` | run | not offered to the model; refused if called |

## System prompt

- `--system TEXT` or `--system-file PATH` replaces the base prompt.
- `AGENTS.md` files load from the git root down to the working directory,
  outermost first. Without a git root, only the working directory is read.
- `--agents PATH` (repeatable) loads the given files instead.
  `--no-agents` loads none.

`/system` in the REPL prints the assembled prompt.

## REPL commands

`/mode`, `/model`, `/tools`, `/system`, `/usage`, `/clear`, `/quit`.
Ctrl-C cancels the current turn. Ctrl-D exits.
