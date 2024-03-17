;; Transient menus for IRC formatting.
;;
;; Adds a `C-c q' keybind to `erc-mode' to pick IRC color formatting codes
;; via a transient prefix.
;;
;; Author: Alcor <alcor@tilde.club>

(defmacro erc-fmt--define-color-describer (color n)
  `(defun ,(intern (concat "erc-fmt--describe-color-" color)) (&optional fg)
     (propertize ,color 'face (append (list (erc-get-fg-color-face (or fg ,n)))
                                      (when fg (list (erc-get-bg-color-face ,n)))))))

;; Color formatters
(erc-fmt--define-color-describer "white"      0)
(erc-fmt--define-color-describer "black"      1)
(erc-fmt--define-color-describer "blue"       2)
(erc-fmt--define-color-describer "green"      3)
(erc-fmt--define-color-describer "red"        4)
(erc-fmt--define-color-describer "brown"      5)
(erc-fmt--define-color-describer "magenta"    6)
(erc-fmt--define-color-describer "orange"     7)
(erc-fmt--define-color-describer "yellow"     8)
(erc-fmt--define-color-describer "lightgreen" 9)
(erc-fmt--define-color-describer "cyan"       10)
(erc-fmt--define-color-describer "lightcyan"  11)
(erc-fmt--define-color-describer "lightblue"  12)
(erc-fmt--define-color-describer "pink"       13)
(erc-fmt--define-color-describer "grey"       14)
(erc-fmt--define-color-describer "lightgrey"  15)

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
(erc-fmt--color-define-prefix erc-fmt-color-white      0 "white")
(erc-fmt--color-define-prefix erc-fmt-color-black      1 "black")
(erc-fmt--color-define-prefix erc-fmt-color-blue       2 "blue")
(erc-fmt--color-define-prefix erc-fmt-color-green      3 "green")
(erc-fmt--color-define-prefix erc-fmt-color-red        4 "red")
(erc-fmt--color-define-prefix erc-fmt-color-brown      5 "brown")
(erc-fmt--color-define-prefix erc-fmt-color-magenta    6 "magenta")
(erc-fmt--color-define-prefix erc-fmt-color-orange     7 "orange")
(erc-fmt--color-define-prefix erc-fmt-color-yellow     8 "yellow")
(erc-fmt--color-define-prefix erc-fmt-color-lightgreen 9 "lightgreen")
(erc-fmt--color-define-prefix erc-fmt-color-cyan       10 "cyan")
(erc-fmt--color-define-prefix erc-fmt-color-lightcyan  11 "lightcyan")
(erc-fmt--color-define-prefix erc-fmt-color-lightblue  12 "lightblue")
(erc-fmt--color-define-prefix erc-fmt-color-pink       13 "pink")
(erc-fmt--color-define-prefix erc-fmt-color-grey       14 "grey")
(erc-fmt--color-define-prefix erc-fmt-color-lightgrey  15 "lightgrey")

;; Main transient menu
(transient-define-prefix erc-fmt ()
  ["Foreground color"
   ("00" erc-fmt--describe-color-white      erc-fmt-color-white)
   ("01" erc-fmt--describe-color-black      erc-fmt-color-black)
   ("02" erc-fmt--describe-color-blue       erc-fmt-color-blue)
   ("03" erc-fmt--describe-color-green      erc-fmt-color-green)
   ("04" erc-fmt--describe-color-red        erc-fmt-color-red)
   ("05" erc-fmt--describe-color-brown      erc-fmt-color-brown)
   ("06" erc-fmt--describe-color-magenta    erc-fmt-color-magenta)
   ("07" erc-fmt--describe-color-orange     erc-fmt-color-orange)
   ("08" erc-fmt--describe-color-yellow     erc-fmt-color-yellow)
   ("09" erc-fmt--describe-color-lightgreen erc-fmt-color-lightgreen)
   ("10" erc-fmt--describe-color-cyan       erc-fmt-color-cyan)
   ("11" erc-fmt--describe-color-lightcyan  erc-fmt-color-lightcyan)
   ("12" erc-fmt--describe-color-lightblue  erc-fmt-color-lightblue)
   ("13" erc-fmt--describe-color-pink       erc-fmt-color-pink)
   ("14" erc-fmt--describe-color-grey       erc-fmt-color-grey)
   ("15" erc-fmt--describe-color-lightgrey  erc-fmt-color-lightgrey)]
  ["Effects"
   ("i"  erc-fmt--describe-italic    erc-fmt-italicize)
   ("b"  erc-fmt--describe-bold      erc-fmt-boldify)
   ("u"  erc-fmt--describe-underline erc-fmt-underline)])

;; Install binding
(define-key erc-mode-map (kbd "C-c q") 'erc-fmt)

;; Erc module to normalize text inserted at the prompt.
;; Author: Alcor <alcor@tilde.club>

(defvar erc-normalize-copy-cc t "Copy control codes")

(defmacro erc-normalize-color-mapper (n pred get-color-face value)
  "Generates face mapper for color codes [0;n] where value is checked
against get-color-face via pred."
  `(cond
    ,@(let ((clauses))
        (while (>= n 0)
          (push `((funcall ,pred (funcall ,get-color-face ,n) ,value) ,n) clauses)
          (setq n (1- n)))
        clauses)))

(defun erc-normalize-get-color (pred get-color-face value)
  "Returns the color code for face VALUE given the inverse function GET-COLOR-FACE
and the given equality predicate."
  (erc-normalize-color-mapper 98 pred get-color-face value))

(defun erc-normalize-text-enclose-cc (start end fg bg bold italic underline)
  "Given formatting flags FG BG BOLD ITALIC UNDERLINE, enclose the string
between START and END with the corresponding IRC formatting codes."
  (let ((open nil)
        (close nil))
    (when bold (push "\02" open) (push "\02" close))
    (when italic (push "\035" open) (push "\035" close))
    (when underline (push "\037" open) (push "\037" close))
    (when (or fg bg)
      (push (if bg (format "\03%d,%d" (or fg 99) bg) (format "\03%d" fg)) open)
      (push "\03" close))
    (when (and open close)
      (save-excursion
        (goto-char end)
        (insert (string-join close))
        (goto-char start)
        (insert (string-join open))))))

(defun erc-normalize-text (value start end)
  "Given a formatted string VALUE between START and END, enclose the
string the appropriate formatting codes."
  (let* ((pred (if (listp value) #'member #'equal))
         (fg (erc-normalize-get-color pred #'erc-get-fg-color-face value))
         (bg (erc-normalize-get-color pred #'erc-get-bg-color-face value))
         (bold (funcall pred 'erc-bold-face value))
         (italic (funcall pred 'erc-italic-face value))
         (underline (funcall pred 'erc-underline-face value)))
    (erc-normalize-text-enclose-cc start end fg bg bold italic underline)))

(defun erc-normalize-cc-substitute (str)
  "Given a formatted string STR, normalize it for sending over IRC,
substituting ERC faces with corresponding control characters if ERC-NORMALIZE-COPY-CC is T."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when erc-normalize-copy-cc
      (let ((match nil))
        (while (setq match (text-property-search-forward 'font-lock-face nil nil))
          (let ((start (prop-match-beginning match))
                (end (prop-match-end match))
                (value (prop-match-value match)))
            (erc-normalize-text value start end)))))
    (set-text-properties (point-min) (point-max) nil)
    (buffer-string)))

(defun erc-normalize-setup ()
  (add-hook 'yank-transform-functions #'erc-normalize-cc-substitute 70 t))

(define-erc-module normalize nil
  "This mode removes properties from text inserted at the prompt,
possibly inserting corresponding control characters."
  ((add-hook 'erc-mode-hook #'erc-normalize-setup)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-normalize-setup)))
  ((remove-hook 'erc-mode-hook #'erc-normalize-setup)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (remove-hook 'yank-transform-functions #'erc-normalize-cc-substitute t)))))
