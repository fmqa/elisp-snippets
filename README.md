# My collection of Emacs Lisp snippets

## erc-ircfmt.el

[transient.el](https://www.gnu.org/software/emacs/manual/html_mono/transient.html)-based UI for [IRC formatting control codes](https://modern.ircdocs.horse/formatting.html). 

Triggered via `C-c q` in `erc-mode`.

![Foreground selection](.assets/ircfmt-1.png?raw=true "Foreground color selection")
![Background selection](.assets/ircfmt-2.png?raw=true "Background color selection")

## mpris2.el

Defines an interactive `mpris2-yank-current-track` function that inserts the current [MPRIS2](https://mpris2.readthedocs.io/en/latest/) track & artist name at point.
