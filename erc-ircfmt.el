;; Adds a `C-c q' keybind to `erc-mode' to pick IRC color formatting codes
;; via a transient prefix

(defmacro erc-fmt--define-color-describer (color n)
  `(defun ,(intern (concat "erc-fmt--describe-color-" color)) (&optional fg)
     (propertize ,color 'face (append (list (erc-get-fg-color-face (or fg ,n)))
                                      (when fg (list (erc-get-bg-color-face ,n)))))))

;; Color formatters
(erc-fmt--define-color-describer "white" 0)
(erc-fmt--define-color-describer "black" 1)
(erc-fmt--define-color-describer "blue" 2)
(erc-fmt--define-color-describer "green" 3)
(erc-fmt--define-color-describer "red" 4)
(erc-fmt--define-color-describer "brown" 5)
(erc-fmt--define-color-describer "magenta" 6)
(erc-fmt--define-color-describer "orange" 7)
(erc-fmt--define-color-describer "yellow" 8)
(erc-fmt--define-color-describer "lightgreen" 9)
(erc-fmt--define-color-describer "cyan" 10)
(erc-fmt--define-color-describer "lightcyan" 11)
(erc-fmt--define-color-describer "lightblue" 12)
(erc-fmt--define-color-describer "pink" 13)
(erc-fmt--define-color-describer "grey" 14)
(erc-fmt--define-color-describer "lightgrey" 15)

(defun erc-fmt--describe-italic () (propertize "italic" 'face '(erc-italic-face)))
(defun erc-fmt--describe-bold () (propertize "bold" 'face '(erc-bold-face)))
(defun erc-fmt--describe-underline () (propertize "underline" 'face '(erc-underline-face)))

(defun erc-fmt--color-code (fg &optional bg)
  (if bg (format "\03%d,%d" fg bg) (format "\03%d" fg)))

(defun erc-fmt--enclose-with (open close)
  (let ((from (region-beginning))
        (to (region-end)))
    (save-excursion
      (goto-char to)
      (insert close)
      (goto-char from)
      (insert open))))

(defun erc-fmt--insert-or-enclose (open &optional close)
  (if (region-active-p)
      (erc-fmt--enclose-with open (or close open))
    (insert open)))

(defun erc-fmt--insert-color (fg &optional bg)
  (erc-fmt--insert-or-enclose (erc-fmt--color-code fg bg) "\03"))

(defun erc-fmt-boldify ()
  (interactive)
  (erc-fmt--insert-or-enclose "\02"))

(defun erc-fmt-italicize ()
  (interactive)
  (erc-fmt--insert-or-enclose "\035"))

(defun erc-fmt-underline ()
  (interactive)
  (erc-fmt--insert-or-enclose "\037"))

;; Macro helper for defining background color transient
(defmacro erc-fmt--color-define-prefix (name fg color)
  `(transient-define-prefix ,name ()
     [,(format "Foreground: %s; Background: ?" color)
      ("00" (lambda () (erc-fmt--describe-color-white      ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 0)))
      ("01" (lambda () (erc-fmt--describe-color-black      ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 1)))
      ("02" (lambda () (erc-fmt--describe-color-blue       ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 2)))
      ("03" (lambda () (erc-fmt--describe-color-green      ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 3)))
      ("04" (lambda () (erc-fmt--describe-color-red        ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 4)))
      ("05" (lambda () (erc-fmt--describe-color-brown      ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 5)))
      ("06" (lambda () (erc-fmt--describe-color-magenta    ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 6)))
      ("07" (lambda () (erc-fmt--describe-color-orange     ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 7)))
      ("08" (lambda () (erc-fmt--describe-color-yellow     ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 8)))
      ("09" (lambda () (erc-fmt--describe-color-lightgreen ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 9)))
      ("10" (lambda () (erc-fmt--describe-color-cyan       ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 10)))
      ("11" (lambda () (erc-fmt--describe-color-lightcyan  ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 11)))
      ("12" (lambda () (erc-fmt--describe-color-lightblue  ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 12)))
      ("13" (lambda () (erc-fmt--describe-color-pink       ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 13)))
      ("14" (lambda () (erc-fmt--describe-color-grey       ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 14)))
      ("15" (lambda () (erc-fmt--describe-color-lightgrey  ,fg)) (lambda () (interactive) (erc-fmt--insert-color ,fg 15)))
      ("s" "spoilers" (lambda () (interactive) (erc-fmt--insert-color ,fg ,fg)))
      ("RET" (lambda () (propertize "default" 'face (list (erc-get-fg-color-face ,fg)))) (lambda () (interactive) (erc-fmt--insert-color ,fg)))]))

;; Define background color prefixes
(erc-fmt--color-define-prefix erc-fmt-color-white 0 "white")
(erc-fmt--color-define-prefix erc-fmt-color-black 1 "black")
(erc-fmt--color-define-prefix erc-fmt-color-blue 2 "blue")
(erc-fmt--color-define-prefix erc-fmt-color-green 3 "green")
(erc-fmt--color-define-prefix erc-fmt-color-red 4 "red")
(erc-fmt--color-define-prefix erc-fmt-color-brown 5 "brown")
(erc-fmt--color-define-prefix erc-fmt-color-magenta 6 "magenta")
(erc-fmt--color-define-prefix erc-fmt-color-orange 7 "orange")
(erc-fmt--color-define-prefix erc-fmt-color-yellow 8 "yellow")
(erc-fmt--color-define-prefix erc-fmt-color-lightgreen 9 "lightgreen")
(erc-fmt--color-define-prefix erc-fmt-color-cyan 10 "cyan")
(erc-fmt--color-define-prefix erc-fmt-color-lightcyan 11 "lightcyan")
(erc-fmt--color-define-prefix erc-fmt-color-lightblue 12 "lightblue")
(erc-fmt--color-define-prefix erc-fmt-color-pink 13 "pink")
(erc-fmt--color-define-prefix erc-fmt-color-grey 14 "grey")
(erc-fmt--color-define-prefix erc-fmt-color-lightgrey 15 "lightgrey")

;; Main transient menu
(transient-define-prefix erc-fmt ()
  ["Foreground color"
   ("00" erc-fmt--describe-color-white erc-fmt-color-white)
   ("01" erc-fmt--describe-color-black erc-fmt-color-black)
   ("02" erc-fmt--describe-color-blue erc-fmt-color-blue)
   ("03" erc-fmt--describe-color-green erc-fmt-color-green)
   ("04" erc-fmt--describe-color-red erc-fmt-color-red)
   ("05" erc-fmt--describe-color-brown erc-fmt-color-brown)
   ("06" erc-fmt--describe-color-magenta erc-fmt-color-magenta)
   ("07" erc-fmt--describe-color-orange erc-fmt-color-orange)
   ("08" erc-fmt--describe-color-yellow erc-fmt-color-yellow)
   ("09" erc-fmt--describe-color-lightgreen erc-fmt-color-lightgreen)
   ("10" erc-fmt--describe-color-cyan erc-fmt-color-cyan)
   ("11" erc-fmt--describe-color-lightcyan erc-fmt-color-lightcyan)
   ("12" erc-fmt--describe-color-lightblue erc-fmt-color-lightblue)
   ("13" erc-fmt--describe-color-pink erc-fmt-color-pink)
   ("14" erc-fmt--describe-color-grey erc-fmt-color-grey)
   ("15" erc-fmt--describe-color-lightgrey erc-fmt-color-lightgrey)]
  ["Effects"
   ("i"  erc-fmt--describe-italic erc-fmt-italicize)
   ("b"  erc-fmt--describe-bold erc-fmt-boldify)
   ("u"  erc-fmt--describe-underline erc-fmt-underline)])

;; Install binding
(define-key erc-mode-map (kbd "C-c q") 'erc-fmt)
