# Revision history for jsaddle-terminal

## 0.1.0.0

* First release.  Runs a jsaddle app inside a tmux pane: when a watching
  IDE (leksah) answers the handshake, jsaddle's Batch/Results JSON is
  tunnelled over the process's own stdin/stdout (OSC 5799 frames out,
  RS-prefixed lines in) and the UI renders in an iframe hosted by the IDE.
  No sockets are opened, so it also works over ssh-reached tmux servers.
* Builds on POSIX (termios raw mode) and Windows (console API).
* The optional demo (flag `demo`, off by default) uses reflex-dom-core 0.9
  and ghcjs-dom 0.10.
