# hilda

The `hilda` command: a coding agent for OpenAI-compatible chat-completions servers and OpenRouter. It runs headless, as a REPL, or as an Agent Client Protocol (ACP) server.

The agent loop is the [`hilda-core`](https://hackage.haskell.org/package/hilda-core) library. Options, providers, modes and the changelog are in the [repository README](https://github.com/shakfu/hilda#readme).

## Install

```sh
cabal install hilda
```

isocline 1.1.0 cannot read Shift+Enter in kitty, WezTerm or Ghostty. With it, or any release without the fix, use Ctrl+J. A build from a [clone](https://github.com/shakfu/hilda) (`make install`) has the fix.
