# My collection of Emacs Lisp snippets

## erc-tts.el

Pipes Erc messages to a TTS program.

Use via `M-x erc-tts-start`. The "TTS program" must be a executable that reads lines to speak on STDIN and outputs sound to your system's preferred sound sink.

The TTS process can be stopped using `M-x erc-tts-stop`.

## mpris2.el

Defines an interactive `mpris2-yank-current-track` function that inserts the current [MPRIS2](https://mpris2.readthedocs.io/en/latest/) track & artist name at point.
