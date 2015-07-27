;; Little Word Stuff (from tarao's plugins)

(defun maybe-define-category (cat doc &optional table)
  (unless (category-docstring cat table) (define-category cat doc table)))

(let (uc lc defs (table (standard-category-table)))
  (map-char-table
   #'(lambda (key value)
       (when (natnump value)
         (let (from to)
           (if (consp key)
               (setq from (car key) to (cdr key))
             (setq from (setq to key)))
           (while (<= from to)
             (cond ((/= from (downcase from))
                    (add-to-list 'uc from))
                   ((/= from (upcase from))
                    (add-to-list 'lc from)))
             (setq from (1+ from))))))
   (standard-case-table))
  (setq defs `(("Uppercase" ?U ,uc)
               ("Lowercase" ?u ,lc)
               ("Underscore" ?_ (?_))))
  (dolist (elt defs)
    (maybe-define-category (cadr elt) (car elt) table)
    (dolist (ch (car (cddr elt)))
      (modify-category-entry ch (cadr elt) table))))

(defgroup vimp-little-word nil
  "CamelCase and snake_case word movement support."
  :prefix "vimp-little-word-"
  :group 'vimp)

(defcustom vimp-little-word-separating-categories
  (append vimp-cjk-word-separating-categories '((?u . ?U) (?_ . ?u) (?_ . ?U)))
  "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '((character . character))
  :group 'vimp-little-word)

(defcustom vimp-little-word-combining-categories
  (append vimp-cjk-word-combining-categories '())
  "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '((character . character))
  :group 'vimp-little-word)

(defmacro vimp-with-little-word (&rest body)
  (declare (indent defun) (debug t))
  `(let ((vimp-cjk-word-separating-categories
          vimp-little-word-separating-categories)
         (vimp-cjk-word-combining-categories
          vimp-little-word-combining-categories))
     ,@body))

(vimp-define-motion vimp-forward-little-word-begin (count)
  "Move the cursor to the beginning of the COUNT-th next little word."
  :type exclusive
  (vimp-with-little-word (vimp-forward-word-begin count)))

(vimp-define-motion vimp-forward-little-word-end (count)
  "Move the cursor to the end of the COUNT-th next little word."
  :type inclusive
  (vimp-with-little-word (vimp-forward-word-end count)))

(vimp-define-motion vimp-backward-little-word-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous little word."
  :type exclusive
  (vimp-with-little-word (vimp-backward-word-begin count)))

 (vimp-define-motion vimp-backward-little-word-end (count)
  "Move the cursor to the end of the COUNT-th previous little word."
  :type inclusive
  (vimp-with-little-word (vimp-backward-word-end count)))

(vimp-define-text-object vimp-a-little-word (count &optional beg end type)
  "Select a little word."
  (vimp-select-an-object 'vimp-little-word beg end type count))

(vimp-define-text-object vimp-inner-little-word (count &optional beg end type)
  "Select inner little word."
  (vimp-select-inner-object 'vimp-little-word beg end type count))

(defun forward-vimp-little-word (&optional count)
    "Move by little words."
    "Forward by little words."
    (vimp-with-little-word (vimp-move-word count))
  (vimp-with-little-word (forward-vimp-word count)))

(provide 'lalopmak-camel-case)
