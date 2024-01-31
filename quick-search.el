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
(defun which-active-modes ()
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

(defvar *search-engines* nil)
(setq *search-engines* nil)

(defun add-search-engine (name search-query modes)
  "Creates a 'Search engine object' that the quick-search function will take into account"
  (let (
        (new-engine (list name search-query modes))
        )
    (add-to-list '*search-engines* new-engine)
    ))

(defun get-engine-name (engine)
  (nth 0 engine))

(defun get-engine-search (engine)
  (nth 1 engine))

(defun get-engine-modes (engine)
  (nth 2 engine))

(defun check-if-engine-is-valid (engine modes)
  (let (
        (engine-modes (get-engine-modes engine))
        )
    (if (not engine-modes) 
        't ;;If the engine has "nil" as modes, then that means it has to be turned on globally
      (seq-intersection engine-modes modes)
    )))

(defun get-valid-engines (engines modes)
  (seq-filter (lambda (engine)
	  (check-if-engine-is-valid engine modes))
	engines)
  )

(defun get-engine-from-name (engine-name engines)
  "Get the 'Engine Object', given the name"
    (seq-find (lambda (engine) (equal engine-name (get-engine-name engine))) engines)
    )







;; Browser part
(defvar *prefered-browser* nil)
(setq *prefered-browser** nil)

(defun set-prefered-browser (name arguments)
  "Sets the prefered-browser variable. This variable is used to call the shell command that will open the browser itself"
  (setq *prefered-browser* (cons name arguments)))

(defun get-browser-name (browser)
  "Sets the prefered-browser variable. This variable is used to call the shell command that will open the browser itself"
  (car *prefered-browser*))

(defun get-browser-argument (browser)
  "Sets the prefered-browser variable. This variable is used to call the shell command that will open the browser itself"
  (cdr *prefered-browser*))

(defun search-on-browser (engine what)
  "Search TERM on preferred engine on engine.

If no search TERM is entered interactively, the current
buffer selection is used as the TERM."
  (let*
      (
       (browser-name (get-browser-name *prefered-browser*))
       (browser-argument (get-browser-argument *prefered-browser*))
       (engine-search (get-engine-search engine))
       (full_query_url (concat engine-search "'" what "'"))
       )
    (shell-command (concat browser-name " " browser-argument full_query_url) nil nil)
    ))

;; Quick search part
(defun quick-search (where what)
  "Opens the quicksearch menu so that you can pick and choose what and where to search"
  (interactive
   (let* (
         (completion-ignore-case  t)
         (valid-engines (get-valid-engines *search-engines* (which-active-modes)))
         (engine-names (mapcar (lambda (engine)
			    (get-engine-name engine))
			 valid-engines))
         )
     (list
      (get-engine-from-name
       (completing-read "Which search engine: " engine-names nil t) valid-engines)
      (read-string "What: ")
      )
     ))
  (search-on-browser where what)
  )


(provide 'quick-search)
;;; quick-search.el ends here
