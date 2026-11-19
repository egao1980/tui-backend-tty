# tui-backend-tty

[`tui-protocol`](https://github.com/egao1980/tui-protocol) backend on the real terminal. Brief: [`cl-stack/docs/capabilities/tui.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/tui.md) ([#274](https://github.com/egao1980/cl-stack/issues/274)).

**Status: contract only.** The tests describe the backend; the GF methods signal "not implemented".

```lisp
(asdf:load-system "tui-backend-tty")

(if (tui-backend-tty:tty-available-p)
    (stack-tui:run-tui (make-instance 'my-app) :backend (tui-backend-tty:make-tty-backend))
    …)   ; stdin/stdout not a terminal: use tui-backend-terminal, or fail
```

| OS | How |
|----|-----|
| Linux, macOS | CFFI into libc: `isatty`, `tcgetattr` / `cfmakeraw` / `tcsetattr` on an opaque termios buffer, `ioctl(TIOCGWINSZ)`, `SIGWINCH` through the implementation's signal API. |
| Windows 10 1809+ | CFFI into kernel32: `GetConsoleMode` / `SetConsoleMode` (VT input, VT processing), UTF-8 code pages, `ReadConsoleInputW`, `GetConsoleScreenBufferInfo`. |

No C shim, no overlays, no grovel: the package is source-only.

- `backend-start` saves the terminal state and enters raw mode. It signals `tty-not-a-terminal` (a `tui-backend-error`) when stdin or stdout is not a terminal.
- `backend-stop` restores the saved state exactly. `run-tui` calls it on every exit, including errors. An exit hook restores it when the process exits without unwinding `run-tui`.
- A window resize wakes `backend-read`. `run-tui` then sees the new `backend-size` and sends a `resize-message`.

## Tests

The tests start a child Lisp on a pty from [`pty-protocol`](https://github.com/egao1980/pty-protocol) ([`pty-backend-native`](https://github.com/egao1980/pty-backend-native)) and read the terminal state with `stty` (Unix) or `GetConsoleMode` (Windows). The crash-exit test needs `sh` and is skipped on Windows.

## License

MIT
