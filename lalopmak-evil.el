;;  Colemak Evil: A set of optimized Vim-like key bindings for Emacs.
;;  Copyright (C) 2013 Patrick Brinich-Langlois

;;  lalopmak-evil: A more geometric fork.
;;  Copyright (C) 2013

;;  lalopmak-evil: Some additions by Tim
;;  Copyright (C) 2014

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see .


(require 'lalopmak-evil-base)
(require 'lalopmak-camel-case)


;;;;;;;;;;;;;;;;; Bindings ;;;;;;;;;;;;;;;;;;;


;;; Up/down/left/right
(set-in-all-evil-states-but-insert "u" 'evil-previous-line)
(set-in-all-evil-states-but-insert "e" 'evil-next-line)
(set-in-all-evil-states-but-insert "n" 'evil-backward-char)
(set-in-all-evil-states-but-insert "i" 'evil-forward-char)
;;; Beginning/end of line (home/end)
(set-in-all-evil-states-but-insert "y" 'evil-insert)
(set-in-all-evil-states-but-insert "\M-y" 'evil-append)
(set-in-all-evil-states-but-insert "Y" 'evil-insert-line)
(set-in-all-evil-states-but-insert "\M-Y" 'evil-append-line)


(set-in-all-evil-states-but-insert "\M-r" 'evil-jump-backward)
(set-in-all-evil-states-but-insert "\M-s" 'evil-jump-forward)


(set-in-all-evil-states-but-insert "\M-h" 'helm-swoop-without-pre-input)
(define-key evil-normal-state-map "m" 'helm-timi)
(define-key evil-insert-state-map "\M-m" 'helm-proj)
(global-set-key "\M-m" 'helm-proj)


(evil-define-motion lalopmak-evil-scroll-page-up (count)
    "Scrolls page up 10 lines"
    (previous-line 10))

(evil-define-motion lalopmak-evil-scroll-page-down (count)
    "Scrolls down 10 lines"
    (next-line 10))

;;; Prev/next buffer
(set-in-all-evil-states-but-insert "\M-u" 'lalopmak-evil-scroll-page-up)
(set-in-all-evil-states-but-insert "\M-e" 'lalopmak-evil-scroll-page-down)


;;; WORD forward/backward
(set-in-all-evil-states-but-insert "\M-i" 'evil-forward-little-word-end)
(set-in-all-evil-states-but-insert "\M-I" 'forward-symbol)
(set-in-all-evil-states-but-insert "\M-n" 'evil-backward-little-word-begin)
(set-in-all-evil-states-but-insert "\M-N" (lambda () (interactive) (forward-symbol -1)))

(set-in-all-evil-states-but-insert "I" 'lalopmak-evil-narrowed-ace-jump-char-to-mode)
(set-in-all-evil-states-but-insert "R" 'evil-insert-line)
(set-in-all-evil-states-but-insert ";" 'evil-repeat-find-char)

;;Ace jump
(set-in-all-evil-states-but-insert "\M-o" 'lalopmak-evil-narrowed-ace-jump-char-mode)
(lalopmak-evil-define-key evil-normal-state-map "o" 'lalopmak-evil-ace-jump-line-mode)
(setq ace-jump-word-mode-use-query-char t)
(set-in-all-evil-states-but-insert "l" 'ace-jump-word-mode)


(set-in-all-evil-states-but-insert "f" 'helm-semantic-or-imenu)
(set-in-all-evil-states-but-insert "\M-f" 'helm-etags-select)

(winner-mode)
(global-set-key "\M-m" 'window-configuration-to-register)
(global-set-key "\M-j" 'jump-to-register)


(set-in-all-evil-states-but-insert "\M-." 'evil-jump-backward)
(set-in-all-evil-states-but-insert "\M-," 'evil-jump-forward)


(set-in-all-evil-states-but-insert (kbd "<RET>") 'previous-buffer)
(set-in-all-evil-states-but-insert (kbd "S-<RET>") 'next-buffer)
(set-in-all-evil-states-but-insert "ร" 'next-buffer)

                                        ; Marks
(lalopmak-evil-define-key evil-normal-state-map "M" 'evil-set-marker)
(set-in-all-evil-states-but-insert "j" 'evil-goto-mark)

;; Dired up and down
(define-key dired-mode-map "e" 'next-line)
(define-key dired-mode-map "u" 'previous-line)

;; smartparens
(evil-global-set-key 'insert (kbd "M-i") 'sp-forward-slurp-sexp)
(evil-global-set-key 'insert (kbd "M-n") 'sp-forward-barf-sexp)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(add-to-list 'evil-emacs-state-modes 'undo-tree-visualizer-mode)
(define-key undo-tree-visualizer-mode-map (kbd "u") 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "e") 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map (kbd "n") 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-mode-map (kbd "i") 'undo-tree-visualize-switch-branch-right)


;; Minibuffer
(define-key evil-ex-completion-map "\M-e" 'next-complete-history-element)
(define-key evil-ex-completion-map  "\M-u" 'previous-complete-history-element)
(define-key evil-ex-completion-map "\M-i" 'forward-word)
(define-key evil-ex-completion-map  "\M-n" 'backward-word)


;; Send an email from anywher;; e
;; (set-in-all-evil-states-but-insert "\C-\M-m" 'mu4e-compose-new)
;; (global-set-key "\C-\M-m" 'mu4e-compose-new)

; ido
(defun ido-my-keys ()
    (define-key ido-completion-map "\M-i" 'ido-next-match)
    (define-key ido-completion-map "\M-n" 'ido-prev-match)
    (define-key ido-completion-map (kbd "<SPC>") 'ido-restrict-to-matches)
    )

(add-hook 'ido-setup-hook 'ido-my-keys)

; org-mode
(evil-define-key 'normal org-mode-map (kbd "C-M-i") 'org-demote-subtree)
(evil-define-key 'normal org-mode-map (kbd "C-M-n") 'org-promote-subtree)

(evil-define-key 'normal python-mode-map (kbd "M-.")
    (lambda ()
        (interactive)
        (progn
        (jedi:goto-definition)
        (recenter-top-bottom 0))
        ))


(set-in-all-evil-states-but-insert (kbd "<tab>") 'indent-for-tab-command)


(eval-after-load "org-mode"
	'(progn
       (org-defkey org-columns-map "n" 'backward-char)
       (org-defkey org-columns-map "i"
                   (lambda () (interactive) (goto-char (1+ (point)))))
       (org-defkey org-columns-map "e"
                   (lambda () (interactive)
                     (let ((col (current-column)))
                       (beginning-of-line 2)
                       (while (and (org-invisible-p2) (not (eobp)))
                         (beginning-of-line 2))
                       (move-to-column col)
                       (if (eq major-mode 'org-agenda-mode)
                           (org-agenda-do-context-action)))))
       (org-defkey org-columns-map "u"
                   (lambda () (interactive)
                     (let ((col (current-column)))
                       (beginning-of-line 0)
                       (while (and (org-invisible-p2) (not (bobp)))
                         (beginning-of-line 0))
                       (move-to-column col)
                       (if (eq major-mode 'org-agenda-mode)
                           (org-agenda-do-context-action)))))
       (org-defkey org-columns-map "y" 'org-columns-edit-value)
))

; jedi
(evil-define-key 'normal python-mode-map (kbd "M-.") 'jedi:goto-definition)


(abbrev-mode t)  ;; Note: *disables* abbrev-mode (?!)

;; jabber
(evil-define-key 'normal jabber-roster-mode-map (kbd "RET")
    (lambda () (interactive) (jabber-chat-with-jid-at-point)))
(evil-define-key 'normal jabber-roster-mode-map (kbd "o")
    (lambda () (interactive) (jabber-roster-toggle-offline-display)))


(evil-global-set-key 'visual (kbd "d") 'evil-delete)

;;directional object maps
(lalopmak-evil-define-key evil-inner-text-objects-map "n" 'evil-inner-word)
(lalopmak-evil-define-key evil-outer-text-objects-map "i" 'evil-a-word)
(lalopmak-evil-define-key evil-inner-text-objects-map "e" 'evil-inner-WORD)
(lalopmak-evil-define-key evil-outer-text-objects-map "u" 'evil-a-WORD)
(lalopmak-evil-define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
(lalopmak-evil-define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
(lalopmak-evil-define-key evil-inner-text-objects-map "p" 'evil-inner-paragraph)
(lalopmak-evil-define-key evil-outer-text-objects-map "p" 'evil-a-paragraph)

;; Not sure what this is, think it's an emacs definition.  Originally assigned to o
(lalopmak-evil-define-key evil-inner-text-objects-map "n" 'evil-inner-symbol)
(lalopmak-evil-define-key evil-outer-text-objects-map "n" 'evil-a-symbol)

;; Execute command: map : to ;
(lalopmak-evil-define-key evil-motion-state-map ";" 'evil-ex);;; End of word forward/backward

;;;        Cut/copy/paste
(set-in-all-evil-states-but-insert "x" 'evil-delete-char)
(set-in-all-evil-states-but-insert "D" 'evil-delete-line)  ; delete to end of line; use dd to delete whole line
(lalopmak-evil-define-key evil-normal-state-map "c" 'evil-yank)
(lalopmak-evil-define-key evil-visual-state-map "c" 'evil-yank)
(set-in-all-evil-states-but-insert "C" (lambda () (interactive) (evil-yank (point) (point-at-eol))))


;; "Original" paste v/undo p
;; =====
(set-in-all-evil-states-but-insert "V" 'evil-paste-before)
(set-in-all-evil-states-but-insert "v" 'evil-paste-after)
(set-in-all-evil-states-but-insert "\C-v" 'evil-paste-pop)
(set-in-all-evil-states-but-insert "\M-v" 'evil-paste-pop-next)

;;; Undo/redo
(lalopmak-evil-define-key evil-normal-state-map "p" 'undo)
(when (fboundp 'undo-tree-undo)
    (lalopmak-evil-define-key evil-normal-state-map "p" 'undo-tree-undo)
    (lalopmak-evil-define-key evil-normal-state-map "\C-p" 'undo-tree-redo))
;; =====


;; get back last visual selection
(lalopmak-evil-define-key evil-normal-state-map "ga" 'evil-visual-restore)

;;; Cursor position jumplist
;; (set-in-all-evil-states-but-insert "N" 'evil-jump-backward)
;; (set-in-all-evil-states-but-insert "I" 'evil-jump-forward)


;;; Move cursor to top/bottom of screen
;; next/prior are page up/down
(set-in-all-evil-states (kbd "C-<next>") 'evil-window-bottom)
(set-in-all-evil-states (kbd "C-<prior>") 'evil-window-top)


;;; Make insert/add work also in visual line mode like in visual block mode
;; not sure what this means

;;; Visual mode
(set-in-all-evil-states-but-insert "a" 'evil-visual-char)
(set-in-all-evil-states-but-insert "A" 'evil-visual-line)
(set-in-all-evil-states-but-insert "\C-a" 'mark-whole-buffer)
(lalopmak-evil-define-key evil-motion-state-map "\M-a" 'evil-visual-block)
(lalopmak-evil-define-key evil-motion-state-map "f" 'evil-find-char)
(lalopmak-evil-define-key evil-operator-state-map "i" 'evil-find-char-to)
(lalopmak-evil-define-key evil-motion-state-map "i" 'evil-find-char-to)
(lalopmak-evil-define-key evil-motion-state-map "c" 'evil-yank-line)
(lalopmak-evil-define-key evil-operator-state-map "c" 'evil-yank-line)
(lalopmak-evil-define-key evil-visual-state-map "\C-\M-u" 'undo)
(lalopmak-evil-define-key evil-visual-state-map "\C-\M-e" 'redo)






;; ;;switching sides in visual mode
;; (define-key evil-visual-state-map " a" 'exchange-point-and-mark)
;; (define-key yevil-visual-state-map " A" 'evil-visual-exchange-corners)

;; ;;space-prefixed motions
;; (define-key evil-motion-state-map " " nil)
;; (define-key evil-motion-state-map "  " (lambda () (interactive) (insert " ")))
;; (define-key evil-motion-state-map " /" 'evil-scroll-line-to-bottom)
;; (define-key evil-motion-state-map " \\" 'evil-scroll-line-to-center)
;; (define-key evil-motion-state-map " !" 'evil-scroll-line-to-top)


(set-in-all-evil-states-but-insert "J" 'evil-join)


;;not motion for compatiblilty with undo-tree
(set-in-all-evil-states-but-insert-and-motion "q" 'evil-replace)
(set-in-all-evil-states-but-insert-and-motion "Q" 'evil-replace-state)

;;; Scroll in place
(lalopmak-evil-define-key evil-motion-state-map (kbd "C-<up>") 'evil-scroll-line-up)
(lalopmak-evil-define-key evil-motion-state-map (kbd "C-<down>") 'evil-scroll-line-down)

;;; Live line reordering
;; not implemented

;;; Restore mappings
;;; Free mappings: ,/+/H

;;; Macros
;; (lalopmak-evil-define-key evil-normal-state-map "Q" '(lambda ()
;; 					 (interactive)
;; 					 (evil-execute-macro 1 last-kbd-macro)))

;; (cond (window-system  ; ensure not running in a terminal
;;        (lalopmak-evil-local-set-key (kbd "<return>") 'newline)
;;        (lalopmak-evil-local-set-key "\C-m" 'evil-record-macro)))
(lalopmak-evil-define-key evil-normal-state-map "@" 'evil-execute-macro)
(lalopmak-evil-define-key evil-normal-state-map "b" 'evil-record-macro)
;; (lalopmak-evil-define-key evil-normal-state-map "\"" 'evil-execute-macro)


(define-key evil-motion-state-map (kbd "C-'") 'evil-goto-mark-line)
;;; Duplicate line
;; not implemented
;; Use "CV" instead

;;; Misc overridden keys must be prefixed with g
;; not implemented


(evil-define-operator lalopmak-evil-all-case (beg end type)
    "Converts to all case, or, if already all case, converts to all lower case."
    (let ((region (buffer-substring beg end)))
        (if (equal (upcase region)
                   region)
                (evil-downcase beg end type)
            (evil-upcase beg end type))))


(lalopmak-evil-define-key evil-visual-state-map "m" 'lalopmak-evil-all-case)

;;; Search
(lalopmak-evil-define-key evil-motion-state-map "k" 'evil-search-next)
(lalopmak-evil-define-key evil-motion-state-map "K" 'evil-search-previous)

;;; Window handling
;; C-w (not C-r as in Shai's mappings) prefixes window commands

(lalopmak-evil-define-key evil-window-map "n" 'evil-window-left)
(lalopmak-evil-define-key evil-window-map "N" 'evil-window-move-far-left)
(lalopmak-evil-define-key evil-window-map "e" 'evil-window-down)
(lalopmak-evil-define-key evil-window-map "E" 'evil-window-move-very-bottom)
(lalopmak-evil-define-key evil-window-map "u" 'evil-window-up)
(lalopmak-evil-define-key evil-window-map "U" 'evil-window-move-very-top)
(lalopmak-evil-define-key evil-window-map "i" 'evil-window-right)
(lalopmak-evil-define-key evil-window-map "I" 'evil-window-move-far-right)
(lalopmak-evil-define-key evil-window-map "k" 'evil-window-new)


(set-in-all-evil-states-but-insert (kbd "\\")  'evil-indent)
;;Unassigns previous object pending states
(define-key evil-visual-state-map "a" nil)
(define-key evil-visual-state-map "\C-\M-y" 'evil-insert)

(define-key evil-visual-state-map "o" 'lalopmak-evil-ace-jump-line-mode)
(define-key evil-motion-state-map "o" 'lalopmak-evil-ace-jump-line-mode)

(define-key evil-operator-state-map "a" nil)
(define-key evil-operator-state-map "y" nil)

;; Insert / inner object pending state
(set-in-all-evil-states-but-insert "R" 'evil-insert-line)
(lalopmak-evil-define-key evil-operator-state-map "y" evil-inner-text-objects-map)
(lalopmak-evil-define-key evil-visual-state-map "y" evil-inner-text-objects-map)

;;Append / outer object pending state
;; (set-in-all-evil-states-but-insert "s" 'evil-append)
;; (set-in-all-evil-states-but-insert "S" 'evil-append-line)
(lalopmak-evil-define-key evil-operator-state-map "a" evil-outer-text-objects-map)
(lalopmak-evil-define-key evil-visual-state-map "a" evil-outer-text-objects-map)

;; (lalopmak-evil-define-key evil-visual-state-map "S" 'evil-append)

;;Change
(set-in-all-evil-states-but-insert "T" 'evil-change-line)
(set-in-all-evil-states-but-insert "t" 'evil-change)

;; (set-in-all-evil-states-but-insert "p" 'evil-substitute)   ;tentative assignment
;; (set-in-all-evil-states-but-insert "\C-p" 'evil-change-whole-line)

;;conflicts with undo
;; (set-in-all-evil-states-but-insert "\C-t" 'evil-jump-backward)
;; (set-in-all-evil-states-but-insert "\C-p" 'evil-jump-forward)

;;old find char/reverse for use in macros
;; (set-in-all-evil-states-but-insert "\M-f" 'evil-find-char)
(set-in-all-evil-states-but-insert "\M-w" 'evil-find-char-backward)
(set-in-all-evil-states-but-insert "\M-t" 'evil-repeat-find-char)

                                        ;switch to buffer
(lalopmak-evil-define-key evil-motion-state-map "b" 'switch-to-buffer)
(lalopmak-evil-define-key evil-motion-state-map "\M-b" 'ido-write-file)
(lalopmak-evil-define-key evil-motion-state-map "\C-b" 'fiplr-find-file)
(lalopmak-evil-define-key evil-motion-state-map "B" 'find-file)

;;switches transpose words to transpose symbols
(lalopmak-evil-define-key evil-insert-state-map "\C-t" 'transpose-symbols)
(lalopmak-evil-define-key evil-normal-state-map "\C-t" 'transpose-symbols)

;;;;;;;;;;;;PASTING;;;;;;;;;;;;;;;;;;
(evil-define-motion lalopmak-evil-paste-below (count)
    "Pastes in the line below."
    (evil-open-below 1)
    ;; (newline count) ;;TODO count indicates number of lines until the paste
    (evil-paste-after 1))

(evil-define-motion lalopmak-evil-paste-below-then-normal (count)
    "Pastes in the line below then normal mode."
    (lalopmak-evil-paste-below count)
    (evil-normal-state))

(evil-define-motion lalopmak-evil-paste-above (count)
    "Pastes in the line above."
    (evil-open-above 1)
    ;; (newline count) ;;TODO count indicates number of lines until the paste
    (evil-paste-after 1))

(evil-define-motion lalopmak-evil-paste-above-then-normal (count)
    "Pastes in the line above then normal mode."
    (lalopmak-evil-paste-above count)
    (evil-normal-state))

(evil-define-motion lalopmak-evil-paste-at-bol (count)
    "Pastes at beginning of line."
    (back-to-indentation)
    (evil-paste-before 1))

(evil-define-motion lalopmak-evil-paste-at-eol (count)
    "Pastes at end of line."
    (evil-end-of-line)
    (evil-paste-after 1))

;;o to open in line above/below, or [number]o to go to line [number]
(set-in-all-evil-states-but-insert "z" 'evil-open-below)
(set-in-all-evil-states-but-insert "Z" 'evil-open-above)


(lalopmak-evil-define-key evil-motion-state-map "0" 'evil-beginning-of-line)


;;necessary or we get errors when trying to map "EE", "UE", etc
(lalopmak-evil-define-key evil-motion-state-map "E" nil)
(lalopmak-evil-define-key evil-motion-state-map "U" nil)

(lalopmak-evil-define-key evil-motion-state-map "EU" 'evil-forward-section-begin)
(lalopmak-evil-define-key evil-motion-state-map "EE" 'evil-forward-section-end)
(lalopmak-evil-define-key evil-motion-state-map "UU" 'evil-backward-section-begin)
(lalopmak-evil-define-key evil-motion-state-map "UE" 'evil-backward-section-end)
(lalopmak-evil-define-key evil-motion-state-map "U(" 'evil-previous-open-paren)
(lalopmak-evil-define-key evil-motion-state-map "U)" 'evil-previous-open-paren)
(lalopmak-evil-define-key evil-motion-state-map "E(" 'evil-next-close-paren)
(lalopmak-evil-define-key evil-motion-state-map "E)" 'evil-next-close-paren)
(lalopmak-evil-define-key evil-motion-state-map "U{" 'evil-previous-open-brace)
(lalopmak-evil-define-key evil-motion-state-map "U}" 'evil-previous-open-brace)
(lalopmak-evil-define-key evil-motion-state-map "E}" 'evil-next-close-brace)
(lalopmak-evil-define-key evil-motion-state-map "E}" 'evil-next-close-brace)

;;tentative assignment; for the key in top middle
(lalopmak-evil-define-key evil-motion-state-map "!" 'evil-jump-item)

;; Makes ; an alias for :
(set-in-all-evil-states-but-insert ";" 'evil-ex)


;;hooks for hints
(evil-ex-define-cmd "hints" 'lalopmak-evil-hints)
(evil-ex-define-cmd "ars" "hints")

(evil-ex-define-cmd "mnemonic" 'lalopmak-evil-mnemonic-hints)


;;experiment
(setq evil-cross-lines t)

(provide 'lalopmak-evil)
