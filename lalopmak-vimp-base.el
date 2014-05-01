

;;  Colemak Evil: A set of optimized Vim-like key bindings for Emacs.
;;  Copyright (C) 2013 Patrick Brinich-Langlois

;;  lalopmak-vimp: A more geometric fork.
;;  Copyright (C) 2013

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;Commonalities to both lalopmak-vimp and minimalistic-lalopmak-vimp
(require 'lalopmak-vimp-libraries)
(require 'lalopmak-layouts)
(require 'lalopmak-jump)

(defvar lalopmak-vimp-lisp-mode-hook-and-map-symbols '((nil (emacs-lisp-mode-map lisp-mode-map lisp-interaction-mode-map hy-mode-map))
                                                       (clojure-mode-hook (clojure-mode-map)))
  "List with entries of the form (hook (mode-map1 mode-map2 ...)) where hook are lisp-mode hooks and the mode-maps
are those to add a keybinding to upon hook being triggered.")

;; we're using the colemak layout by default
(if (not (boundp 'lalopmak-layout-map))
    (setq lalopmak-layout-map 'colemak-to-colemak))

(defmacro lalopmak-vimp-define-key (keymap key def &optional modes)
  "Defines key given the lalopmak-vimp keymap, in accordance to the lalopmak-layout-map"
  `(define-key ,keymap (key-to-layout ,key lalopmak-layout-map) ,def))

;; remove all keybindings from insert-state keymap
(setcdr vimp-insert-state-map nil)
;; but [escape] should switch back to normal state
(lalopmak-vimp-define-key vimp-insert-state-map [escape] 'vimp-normal-state)

;; make undo more incremental (break into smaller chunks)
(setq vimp-want-fine-undo t)

(defmacro lalopmak-vimp-local-set-key (keymap key)
  "Defines local key given the lalopmak-vimp keymap, in accordance to the lalopmak-layout-map"
  `(local-set-key ,keymap (key-to-layout ,key lalopmak-layout-map)))



;; map multiple states at once (courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674)
(defun set-in-all-vimp-states (key def &optional maps)
  (unless maps
    (setq maps (list vimp-normal-state-map
                     vimp-visual-state-map
                     vimp-insert-state-map
                     vimp-emacs-state-map
		     vimp-motion-state-map)))
  (while maps
    (lalopmak-vimp-define-key (pop maps) key def)))


(defun set-in-all-vimp-states-but-insert (key def)
  (set-in-all-vimp-states key
                          def
                          (list vimp-normal-state-map
                                vimp-visual-state-map
                                vimp-emacs-state-map
                                vimp-motion-state-map)))

(defun set-in-all-vimp-states-but-insert-and-motion (key def)
  (set-in-all-vimp-states key
                          def
                          (list vimp-normal-state-map
                                vimp-visual-state-map
                                vimp-emacs-state-map)))


;; Experiment: make space into a "leader" key

;;default: space does one space, unless remapped in a mode
;; (lalopmak-vimp-define-key vimp-motion-state-map " " (lambda () (interactive) (insert " ")))



;; (lalopmak-vimp-define-key vimp-motion-state-map " (" 'vimp-previous-open-paren)
;; (lalopmak-vimp-define-key vimp-motion-state-map " )" 'vimp-next-close-paren)
;; (lalopmak-vimp-define-key vimp-motion-state-map " {" 'vimp-previous-open-brace)
;; (lalopmak-vimp-define-key vimp-motion-state-map " }" 'vimp-next-close-brace)

(defmacro lalopmak-vimp-define-mode-bindings (hook-and-maps-symbols state-symbols &rest bindings)
  "Given lists of state-symbols and hook-and-maps-symbols, as well as some number of key bindings,
binds them via vimp-define-key for those states in those modes.

hook-and-maps-symbols should be a list of the form:

'((hook1 (map1 map2)) (hook2 (map3 map4))...)

e.g. '((nil (emacs-lisp-mode-map lisp-mode-map lisp-interaction-mode-map))
       (clojure-mode-hook (clojure-mode-map)))

where hook can be nil if the maps already exist and can be added to right away."
  `(mapc (lambda (hook-and-maps-symbol)
           (lexical-let* ((hook-symbol (car hook-and-maps-symbol))
                          (map-symbols (cadr hook-and-maps-symbol))
                          (define-key-func (lambda ()
                                             (mapc (lambda (map-symbol)
                                                     (mapc (lambda (state-symbol)
                                                             (vimp-define-key state-symbol (symbol-value map-symbol) ,@bindings))
                                                           ,state-symbols))
                                                   map-symbols))))
             (if hook-symbol
                 (add-hook hook-symbol define-key-func)
               (funcall define-key-func))))
         ,hook-and-maps-symbols))

(defmacro lalopmak-vimp-define-lisp-motions (&rest bindings)
  "For each lisp mode map represented in lalopmak-vimp-lisp-mode-hook-and-map-symbols,

adds 'motion bindings to that lisp mode map."
  `(lalopmak-vimp-define-mode-bindings lalopmak-vimp-lisp-mode-hook-and-map-symbols '(motion) ,@bindings))

(lalopmak-vimp-define-lisp-motions "  " (lambda () (interactive) (insert " "))  ;;two spaces for a space

                                   " a" "as(" ;;select outside parens
                                   " A" "ar(" ;;select inside parens

                                   " c" "cs(" ;;copy outside parens
                                   " C" "cr(" ;;copy inside parens

                                   " t" "ts(" ;;change all parens
                                   " T" "tr(" ;;change inside parens

                                   " d" "ds(" ;;delete all parens
                                   " D" "dr(" ;;delete inside parens

                                   " \\" "\\s(" ;;indent all parens

                                   " ," ",s(" ;;comment all s-exp
                                   " (" 'paredit-wrap-sexp
                                   " {" 'paredit-wrap-curly
                                   " [" 'paredit-wrap-square
                                   " <" 'paredit-wrap-angled

                                   " r(" 'paredit-open-round
                                   " r{" 'paredit-open-curly
                                   " r[" 'paredit-open-square
                                   " r<" 'paredit-open-angled

                                   " )" 'paredit-close-round
                                   " }" 'paredit-close-curly
                                   " ]" 'paredit-close-square
                                   " >" 'paredit-close-angled

                                   ;;navigation on the inside
                                   " l" 'paredit-backward-up
                                   " n" 'paredit-backward
                                   " k" 'paredit-backward-down

                                   " y" 'paredit-forward-up
                                   " i" 'paredit-forward
                                   " ." 'paredit-forward-down

                                   " j" 'paredit-backward-barf-sexp
                                   " h" 'paredit-backward-slurp-sexp

                                   " ;" 'paredit-forward-barf-sexp
                                   " o" 'paredit-forward-slurp-sexp

                                   ;;navigation on the outside
                                   ;; " j" 'paredit-backward-up
                                   ;; " h" 'paredit-backward
                                   ;; " k" 'paredit-backward-down

                                   ;; " ;" 'paredit-forward-up
                                   ;; " o" 'paredit-forward
                                   ;; " ." 'paredit-forward-down

                                   ;; " n" 'paredit-backward-slurp-sexp
                                   ;; " l" 'paredit-backward-barf-sexp

                                   ;; " y" 'paredit-forward-barf-sexp
                                   ;; " i" 'paredit-forward-slurp-sexp

                                   " e" 'paredit-join-sexps
                                   " u" 'paredit-split-sexp

                                   " q" 'raise-sexp
                                   " w" 'paredit-splice-sexp-killing-backward
                                   " f" 'paredit-splice-sexp
                                   " p" 'paredit-splice-sexp-killing-forward
                                   " g" 'paredit-convolute-sexp) 

;;; Make the return and backspace keys work in normal mode
;; Backspace in normal mode doesn't work in the terminal.
(lalopmak-vimp-define-key vimp-motion-state-map (kbd "RET") (lambda () (interactive) (newline)))
(lalopmak-vimp-define-key vimp-motion-state-map (kbd "<backspace>") 'delete-backward-char)


(lalopmak-vimp-define-key vimp-insert-state-map "\C-o" 'vimp-execute-in-normal-state)
(set-in-all-vimp-states (kbd "C-r") 'isearch-backward)

;; ;;multiple cursors
;; (lalopmak-vimp-define-key vimp-visual-state-map " i" 'mc/mark-next-like-this)
;; (lalopmak-vimp-define-key vimp-visual-state-map " n" 'mc/mark-previous-like-this)
;; (lalopmak-vimp-define-key vimp-visual-state-map " e" 'mc/mark-all-symbols-like-this)

;;expand-region
(set-in-all-vimp-states (kbd "C-f") 'er/expand-region)
(setq expand-region-contract-fast-key "w")

(vimp-define-motion lalopmak-vimp-forward-char (count &optional crosslines noerror)
  "Forward character, adds a space to the end of the line if one doesn't already exist."
  :type exclusive
  (interactive "<c>" (list vimp-cross-lines
                           (vimp-kbd-macro-suppress-motion-error)))
  (save-excursion (forward-char)
                  (when (and (eolp)
                             (not (eq ?\s (char-before))))
                    (insert " ")))
  (vimp-forward-char count crosslines noerror))

(vimp-define-motion lalopmak-vimp-backward-char (count &optional crosslines noerror)
  "Backward character, adds a space to the end of the previous line if one doesn't already exist."
  :type exclusive
  (interactive "<c>" (list vimp-cross-lines
                           (vimp-kbd-macro-suppress-motion-error)))
  (save-excursion (backward-char)
                  (when (and (eolp)
                             (not (eq ?\s (char-before))))
                    (insert " ")))
  (vimp-backward-char count 'crosslines noerror))

(vimp-define-motion lalopmak-vimp-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'vimp-move-WORD #'vimp-move-word)))
    (vimp-move-end count move nil t)))

(vimp-define-motion lalopmak-vimp-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (lalopmak-vimp-forward-word-end count t))

(vimp-define-motion lalopmak-vimp-backward-word-begin (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'vimp-move-WORD #'vimp-move-word)))
    (vimp-move-beginning (- (or count 1)) move)))

(vimp-define-motion lalopmak-vimp-backward-WORD-begin (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (lalopmak-vimp-backward-word-begin count t))

;; (vimp-define-motion lalopmak-vimp-forward-char (count &optional crosslines noerror)
;;   "Forward character, allowing you to fall to the next line"
;;   :type exclusive
;;   (if (and (boundp 'paredit-mode) paredit-mode)
;;       (paredit-forward)
;;     (vimp-forward-char count 'crosslines noerror)))

;; (vimp-define-motion lalopmak-vimp-backward-char (count &optional crosslines noerror)
;;   "Backward character, allowing you to rise to the previous line"
;;   (if (and (boundp 'paredit-mode) paredit-mode)
;;       (paredit-backward)
;;     (vimp-backward-char count 'crosslines noerror)))

;;Makes these compatible with undo-tree
(when (boundp 'undo-tree-visualizer-mode-map)
  (define-key undo-tree-visualizer-mode-map [remap lalopmak-vimp-backward-char]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map [remap lalopmak-vimp-forward-char]
    'undo-tree-visualize-switch-branch-right))


(defmacro create_lalopmak-vimp-if-count-else (then-name else-name docstring metadata then &rest else)
  "Creates vimp-motion lalopmak-vimp-if-count-[then-name]-else-[else-name]
metadata should be a list, e.g. (:type line :repeat abort) or nil"
  (declare (indent 2))
  `(vimp-define-motion ,(intern (format "lalopmak-vimp-if-count-%s-else-%s" then-name else-name)) (count)
     ,docstring
     ,@metadata
     (if count
         ,then
       ,@else)))

(defmacro create_lalopmak-vimp-if-count-goto-line-else (else-name docstring metadata &rest else)
  "Creates vimp-motion lalopmak-vimp-if-count-goto-line-else-[else-name]
which goes to line-number count if it exists, otherwise executes else.

metadata should be a list, e.g. (:type line :repeat abort) or nil"
  (declare (indent defun))
  `(create_lalopmak-vimp-if-count-else "goto-line"
       ,else-name
     ,docstring
     ,metadata
     (vimp-goto-line count)
     ,@else))

;;creates lalopmak-vimp-if-count-goto-line-else-open-below
(create_lalopmak-vimp-if-count-goto-line-else "open-below"
  "if count goes to line, otherwise opens below"
  nil
  (vimp-open-below 1))

;;creates lalopmak-vimp-if-count-goto-line-else-ace-jump-line-mode
(create_lalopmak-vimp-if-count-goto-line-else "ace-jump-line-mode"
  "if count goes to line, otherwise ace-jump line"
  (:type line :repeat abort)
  (lalopmak-vimp-ace-jump-line-mode))

(defun lalopmak-vimp-write (beg end &optional type filename bang)
  (if  (and (boundp 'edit-server-edit-mode) edit-server-edit-mode)
      (edit-server-save)
    (vimp-write beg end type filename bang)))

;; (defadvice vimp-write (around check-edit-server
;;                               (beg end &optional type filename bang))
;;   (if (edit-server-edit-mode-running)
;;       (edit-server-save)
;;     ad-do-it))

(defun lalopmak-vimp-execute-process (processName &rest processArgs)
  "Executes a process with given args, all strings.  Does not wait for PROCESSNAME to terminate; returns nil."
  (let ((process (or (executable-find processName)
                     (error (concat "Unable to find " processName)))))
    (apply 'start-process
           process
           nil
           process
           processArgs)))

;;;;;;;;;;;;;;;;;; Custom : commands ;;;;;;;;;;;;;;;;;;;;;;;


(vimp-ex-define-cmd "f[ile]" 'ido-find-file)
(vimp-ex-define-cmd "b[uffer]" 'ido-switch-buffer)

;;hooks for quitting/saving commands
(vimp-ex-define-cmd "w[rite]" 'vimp-write)

;;git
(vimp-ex-define-cmd "git" 'magit-status)
 
(vimp-ex-define-cmd "ccmode" 'centered-cursor-mode) 


;;comment
(vimp-ex-define-cmd "com[ment]" 'vimpnc-comment-operator)

;;M-:
(vimp-ex-define-cmd "eval" 'eval-expression)
(vimp-ex-define-cmd "ev" "eval")
(vimp-ex-define-cmd "ielm" 'ielm-window)
(vimp-ex-define-cmd "interactive-eval" "ielm")
(vimp-ex-define-cmd "repl" 'inferior-lisp)

;;Terminal
(vimp-ex-define-cmd "terminal" 'sole-terminal-window)
(vimp-ex-define-cmd "newterminal" 'new-terminal-window)


;;C-h k
(vimp-ex-define-cmd "key" 'describe-key)

;;C-h f
(vimp-ex-define-cmd "fun[ction]" 'describe-function)

;;M-x apropos
(vimp-ex-define-cmd "funlist" 'apropos)

;;C-h v
(vimp-ex-define-cmd "var[iable]" 'describe-variable)

;;C-h m
(vimp-ex-define-cmd "help" 'describe-mode)

;;calculator
(vimp-ex-define-cmd "ca[lculator]" 'calc)
(vimp-ex-define-cmd "ec[alculator]" 'calc-embedded)

;;registers
(vimp-ex-define-cmd "increment" 'increment-register)
;; (vimp-ex-define-cmd "registers" (kbd "C-x r"))
;; (vimp-ex-define-cmd "showregisters" 'vimp-show-registers)

(vimp-ex-define-cmd "li[nes]" 'list-matching-lines) 

(vimp-ex-define-cmd "gr[ep]" 'grep-find) 
(vimp-ex-define-cmd "rgr[ep]" 'rgrep) 

(defun dired-in-current-directory (&optional wdired)
  "Opens dired in current directory.

If wdired true, opens wdired as well."
  (interactive)
  (unless (equal major-mode 'dired-mode)
    (dired default-directory))
  (when wdired
    (wdired-change-to-wdired-mode) 
    (vimp-normal-state))) 

(defun wdired-in-current-directory ()
  "Opens wdired in current directory"
  (interactive)
  (dired-in-current-directory t))

(vimp-ex-define-cmd "di[red]" 'dired-in-current-directory)
(vimp-ex-define-cmd "wd[ired]" 'wdired-in-current-directory) 

(defun clip-abs-path ()
  "Put the current absolute path on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (dired-current-directory)
                    (buffer-file-name))))
    (when filename
        (x-select-text filename)))) 


(vimp-ex-define-cmd "abs" 'clip-abs-path)

(defun lalopmak-vimp-copy-register (source destination)
  "Copies content of source register to destination register"
  (interactive "*cSource Register: \ncDestination Register:")
  (vimp-set-register destination (vimp-get-register source 'noerror)))

(vimp-ex-define-cmd "rc" 'lalopmak-vimp-copy-register)

(defun lalopmak-vimp-clone-split-buffer ()
  (interactive)
  (let ((cloned-buffer (call-interactively 'clone-indirect-buffer)))
    nil)) 

(vimp-ex-define-cmd "csplit" 'lalopmak-vimp-clone-split-buffer)
;;open external program

;; (vimp-define-operator vimp-yank (beg end type register yank-handler)
;;   "Saves the characters in motion into the kill-ring."
;;   :move-point nil
;;   :repeat nil
;;   (interactive "<R><x><y>")
;;   (cond
;;    ((and (fboundp 'cua--global-mark-active)
;;          (fboundp 'cua-copy-region-to-global-mark)
;;          (cua--global-mark-active))
;;     (cua-copy-region-to-global-mark beg end))
;;    ((eq type 'block)
;;     (vimp-yank-rectangle beg end register yank-handler))
;;    ((eq type 'line)
;;     (vimp-yank-lines beg end register yank-handler))
;;    (t
;;     (vimp-yank-characters beg end register yank-handler))))

(vimp-ex-define-cmd "cmd" 'shell-command)

;;Directory-dependent external processes (e.g. file managers, shells)
(defmacro lalopmak-vimp-directory-process (process &optional dir-str format-dir-str func-name cmd)
  "Defines a function that runs PROCESS. Returns nil; does not wait for PROCESS to terminate.

If DIR-STR is nil, defaults to PROCESS.

If FORMAT-DIR-STR is nil, the function is given (concat DIR-STR DIRECTORY) as its argument.
If non-nil, it is given (format DIR-STR DIRECTORY).

If not given, FUNC-NAME defaults to lalopmak-vimp-PROCESS.

Finally, stores this function into vimp-ex CMD, which defaults to PROCESS.

All arguments are strings.

Example usage: (lalopmak-vimp-directory-process \"gnome-terminal\"
                 \"--working-directory=\"
                 nil
                 \"shell\")

Ranger in gnome-terminal: (lalopmak-vimp-directory-process \"gnome-terminal\"
                            \"--command=ranger \\\"%s\\\"\"
                            t
                            \"lalopmak-vimp-ranger\"
                            \"ranger\") "
  (declare (indent 1))
  (let* ((str-fun (if format-dir-str `format `concat))
         (dir-str (or dir-str ""))
         (func-symbol (intern (or func-name
                                  (concat "lalopmak-vimp-"
                                          process))))
         (docstring (concat "Executes process "
                            process
                            " with sole argument "
                            (funcall str-fun dir-str "DIR")
                            ".  If not given, DIR defaults to buffer directory or ~/.  Does not block.")))
    `(progn
       (defun ,func-symbol (&optional dir)
         ,docstring
         (interactive)
         (lalopmak-vimp-execute-process ,process
                                        (,str-fun ,dir-str
                                                  (or dir
                                                      (buffer-directory)
                                                      (file-truename "~/")))))

       (vimp-ex-define-cmd ,(or cmd process) ',func-symbol))))

;;M-x speck-mode (spell checking)

(vimp-ex-define-cmd "spell" 'speck-mode)

;;multiple cursors
(vimp-ex-define-cmd "cursor" (lambda ()
                               (interactive)
                               (when (boundp 'multiple-cursors-mode)
                                 (if (vimp-visual-state-p)
                                     (mc/edit-lines)
                                   ;; (mc/add-cursor-on-click)
                                   ))))

;; M-x keyfreq mode (key frequency analysis)

(vimp-ex-define-cmd "keyfreq" 'keyfreq-show)

;;Ya-snippets

;; inserts yasnippet "around" the visual mode selection, where applicable.
;; works with yas-wrap-around-region, or by inserting `yas/selected-text`
;; (with those quotations) at select point in snippet
(lalopmak-vimp-define-key vimp-visual-state-map (kbd "<tab>") 'yas-insert-snippet)

;;workaround for Issue #254
(add-hook 'yas-before-expand-snippet-hook
          #'(lambda()
              (when (vimp-visual-state-p)
                (let ((p (point))
                      (m (mark)))
                  (vimp-insert-state)
                  (goto-char p)
                  (set-mark m)))))

(defun vimp-snippet (name)
  (interactive "sSnippet shortcut:")
  (vimp-insert 1)
  (insert (concat " " name))
  (yas-expand))

(vimp-ex-define-cmd "snippet" 'vimp-snippet)

;;Frame sizes

;; (vimp-ex-define-cmd "fit" 'fit-frame-to-buffer)


(vimp-define-motion lalopmak-vimp-stretch (count)
  "Stretches the frame count times"
  (stretch-frame count))

(vimp-define-motion lalopmak-vimp-unstretch (count)
  "Unstretches the frame count times"
  (unstretch-frame count))

(vimp-ex-define-cmd "stretch" 'lalopmak-vimp-stretch)
(vimp-ex-define-cmd "unstretch" 'lalopmak-vimp-unstretch)
(vimp-ex-define-cmd "wide" 'make-frame-wide)

(vimp-define-motion lalopmak-vimp-grow (count)
  "Growes the frame count times"
  (grow-frame count))

(vimp-define-motion lalopmak-vimp-shrink (count)
  "Shrinkes the frame count times"
  (shrink-frame count))

(vimp-ex-define-cmd "gro[w]" 'lalopmak-vimp-grow)
(vimp-ex-define-cmd "shrink" 'lalopmak-vimp-shrink)
(vimp-ex-define-cmd "tall" 'make-frame-tall)

(vimp-ex-define-cmd "small" 'set-frame-to-default-size)
(vimp-ex-define-cmd "large" 'maximize-frame-except-some-width)
(vimp-ex-define-cmd "fullscreen" 'maximize-frame)

(vimp-ex-define-cmd "corner" 'frame-to-top-left-corner)


(vimp-define-operator lalopmak-vimp-strikethrough (beg end type)
  "Strikes through text.

If strikethroughs make up at least half the region (most commonly when the
entire region has been struck through) unstrikes region."
  (let (char)
    (if (eq type 'block)
        (vimp-apply-on-block #'lalopmak-vimp-strikethrough beg end nil)
      (let ((strikethrough-char #x336))
        (if (>= (do-within-positions beg end (count-char-in-buffer strikethrough-char))
                (/ (- end beg) 2))
            (do-within-positions beg end
                                 (replace-string (make-string 1 strikethrough-char)
                                                 ""))
          (save-excursion
            (while (< beg end)
              (goto-char (1+ beg))
              (setq char (following-char))
              (if (eq char strikethrough-char)
                  (setq beg (1+ beg))
                (setq beg (+ 2 beg))
                (setq end (1+ end))
                (insert-char strikethrough-char 1)))))))))


(set-in-all-vimp-states-but-insert "gs" 'lalopmak-vimp-strikethrough)

;; (ad-activate-all)  ;activates all advice

;;FRAGILE
;;Redefines visual updates so as to update the primary, rather than the clipboard, with the selection
;;This also allows you to select a region, copy from outside, then paste into the region
(defun vimp-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (vimp-visual-state-p)
                   (fboundp 'x-select-text)
                   (or (not (boundp 'ns-initialized))
                       (with-no-warnings ns-initialized))
                   (not (eq vimp-visual-selection 'block)))
          (when (display-graphic-p) ;;(equal (window-system) x)??
            (x-set-selection 'PRIMARY (buffer-substring-no-properties
                                       vimp-visual-beginning
                                       vimp-visual-end)))
          (setq x-last-selected-text-primary))))))

(setq vimp-search-module 'vimp-search)
 
;;Generic functions

(defun transpose-symbols (arg)
  "Interchange symbols around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other symbols (backward if ARG negative).
If ARG is zero, the symbols around or after point and around or after mark
are interchanged."
  (interactive "p")
  (transpose-subr 'forward-symbol arg))


(provide 'lalopmak-vimp-base)
