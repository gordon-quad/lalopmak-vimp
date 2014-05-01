
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


(require 'lalopmak-vimp-base)

;;; Up/down/left/right
(set-in-all-vimp-states-but-insert "h" 'lalopmak-vimp-backward-char)
(set-in-all-vimp-states-but-insert "l" 'lalopmak-vimp-forward-char)

;; (set-in-all-vimp-states-but-insert "e" 'vimp-previous-line)
;; (set-in-all-vimp-states-but-insert "\C-e" 'vimp-scroll-up)
;; (set-in-all-vimp-states-but-insert "n" 'vimp-next-line)
;; (set-in-all-vimp-states-but-insert "\C-n" 'vimp-scroll-down)

(lalopmak-vimp-define-key vimp-motion-state-map "k" 'vimp-search-next)
(lalopmak-vimp-define-key vimp-motion-state-map "K" 'vimp-search-previous)


(lalopmak-vimp-define-key vimp-motion-state-map "b" 'lalopmak-vimp-backward-word-begin)
(lalopmak-vimp-define-key vimp-motion-state-map "B" 'lalopmak-vimp-backward-WORD-begin)

(lalopmak-vimp-define-key vimp-motion-state-map "e" 'lalopmak-vimp-forward-word-end)
(lalopmak-vimp-define-key vimp-motion-state-map "E" 'lalopmak-vimp-forward-WORD-end)

;;Ace jump
(set-in-all-vimp-states-but-insert "f" 'lalopmak-vimp-narrowed-ace-jump-char-mode)
(set-in-all-vimp-states-but-insert "F" 'lalopmak-vimp-narrowed-ace-jump-char-to-mode)
(set-in-all-vimp-states-but-insert "t" 'lalopmak-vimp-ace-jump-char-mode)
(set-in-all-vimp-states-but-insert "T" 'lalopmak-vimp-ace-jump-char-to-mode)
;; (set-in-all-vimp-states "\C-f" 'vimp-ace-jump-char-mode)

(when (fboundp 'undo-tree-undo)
  (lalopmak-vimp-define-key vimp-normal-state-map "u" 'undo-tree-undo)
  (lalopmak-vimp-define-key vimp-normal-state-map "U" 'undo-tree-redo))

(lalopmak-vimp-define-key vimp-motion-state-map (kbd "C-b") 'lalopmak-vimp-vimp-scroll-page-up)
(lalopmak-vimp-define-key vimp-motion-state-map (kbd "C-d") 'lalopmak-vimp-vimp-scroll-down)

;;Line jump
;; (set-in-all-vimp-states-but-insert "j" 'lalopmak-vimp-if-count-goto-line-else-ace-jump-line-mode) ;temporary assignment

(set-in-all-vimp-states-but-insert "x" 'ido-switch-buffer)
(set-in-all-vimp-states-but-insert "X" 'ido-find-file)

(set-in-all-vimp-states-but-insert ";" 'vimp-ex)


;; (set-in-all-vimp-states-but-insert "," 'ido-switch-buffer)

(provide 'lalopmak-vimp-mnemonic)
