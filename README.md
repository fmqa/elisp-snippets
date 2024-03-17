# My collection of Emacs Lisp snippets

## erc-ircfmt.el

[transient.el](https://www.gnu.org/software/emacs/manual/html_mono/transient.html)-based UI for [IRC formatting control codes](https://modern.ircdocs.horse/formatting.html). 

Triggered via `C-c q` in `erc-mode`.

![Foreground selection](.assets/ircfmt-1.png?raw=true "Foreground color selection")
![Background selection](.assets/ircfmt-2.png?raw=true "Background color selection")

Additionally, provides an `erc-normalize` module to normalize text yanked into the input prompt, restoring transforming ERC text faces into IRC formatting codes.

## erc-tts.el

Pipes Erc messages to a TTS program.

Use via `M-x erc-tts-start`. The "TTS program" must be a executable that reads lines to speak on STDIN and outputs sound to your system's preferred sound sink.

The TTS process can be stopped using `M-x erc-tts-stop`.

## mpris2.el

Defines an interactive `mpris2-yank-current-track` function that inserts the current [MPRIS2](https://mpris2.readthedocs.io/en/latest/) track & artist name at point.
