;;; quick-search.el --- Package that makes searching parts of a buffer easier  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  Tomas Fabrizio Orsi <torsi@fi.uba.ar>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The purpose of this package is to make searching things quicker, and easier providing a useful little prompt that opens your desired web browser with your selected search engine.
;; Each engine can be customized to be shown on a per mode basis (or even globally)

;;; Code:

;; Modes part

;;; Taken from: https://stackoverflow.com/a/1511827
(defun quick-search/which-active-modes ()
  "Returns all the modes (both major and minor) that are enabled in the current buffer."
  (let ((active-modes))
    ;;Adds all minor modes to the active-modes variable
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)

    ;;Finally, we add the major mode
    (add-to-list 'active-modes major-mode)

    active-modes
    ))


;; Search engine part

(defvar *quick-search/search-engines* nil)
(setq *quick-search/search-engines* nil)

(defun quick-search/add-search-engine (name search-query modes)
  "Creates a 'Search engine object' that the quick-search function will take into account"
  (let (
        (new-engine (list name search-query modes))
        )
    (add-to-list '*quick-search/search-engines* new-engine)
    ))

(defun quick-search/get-engine-name (engine)
  (nth 0 engine))

(defun quick-search/get-engine-search (engine)
  (nth 1 engine))

(defun quick-search/get-engine-modes (engine)
  (nth 2 engine))

(defun quick-search/check-if-engine-is-valid (engine modes)
  "Given an engine and a list of active modes, returns whether it is valid to show the engine or not."
  (let (
        (engine-modes (quick-search/get-engine-modes engine))
        )
    (if (not engine-modes) 
        't ;;If the engine has "nil" as modes, then that means it has to be turned on globally
      (seq-intersection engine-modes modes)
    )))

(defun quick-search/get-valid-engines (engines modes)
  "Given a list of engines and active modes, returns a sequence of all the valid engines"
  (seq-filter (lambda (engine)
	  (quick-search/check-if-engine-is-valid engine modes))
	engines)
  )

(defun quick-search/get-engine-from-name (engine-name engines)
  "Get the 'Engine Object', given the name"
    (seq-find (lambda (engine) (equal engine-name (quick-search/get-engine-name engine))) engines)
    )


;; Browser part
(defvar *quick-search/preferred-browser* nil)
(setq *quick-search/preferred-browser* nil)

(defun quick-search/set-preferred-browser (name arguments)
  "Sets the preferred-browser variable. This variable is used to call the shell command that will open the browser itself"
  (setq *preferred-browser* (cons name arguments)))

(defun quick-search/get-browser-name (browser)
  (car *preferred-browser*))

(defun quick-search/get-browser-argument (browser)
  (cdr *preferred-browser*))

(defun quick-search/search-on-browser (engine what)
  "Search 'what' on preferred engine.

If nothing is entered interactively, the current
buffer selection is used as the 'what'."
  (let*
      (
       (browser-name (quick-search/get-browser-name *preferred-browser*))
       (browser-argument (quick-search/get-browser-argument *preferred-browser*))
       (engine-search (quick-search/get-engine-search engine))
       (full_query_url (concat engine-search "'" what "'"))
       )
    (async-shell-command (concat browser-name " " browser-argument full_query_url) nil nil)
    ))

;; Quick search part
(defun quick-search (where what)
  "Opens the quicksearch menu so that you can pick and choose what and where to search"
  (interactive
   (let* (
         (completion-ignore-case  t)
         (valid-engines (quick-search/get-valid-engines *quick-search/search-engines* (quick-search/which-active-modes)))
         (engine-names (mapcar (lambda (engine)
			    (quick-search/get-engine-name engine))
			 valid-engines))
         )
     (list
      (quick-search/get-engine-from-name
       (completing-read "Which search engine: " engine-names nil t) valid-engines)
      (read-string "What to search (default: buffer selection): ")
      )
     ))
  (let (
        ;; The default search value is what is being selected with the buffer.
        ;; It is quoted so that it is only evaluated if it is used
        (default-search '(buffer-substring (region-beginning) (region-end)))
        )
    (quick-search/search-on-browser where (if (equal what "") (eval default-search) what))
    )
  )

(provide 'quick-search)
;;; quick-search.el ends here
