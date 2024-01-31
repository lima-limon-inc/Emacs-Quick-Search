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

;;; Source: https://heemayl.net/posts/invoke-search-engine-on-browser-with-search-term-from-emacs/
(defun search-on-browser (term)
  "Search TERM on preferred engine on browser.

If no search TERM is entered interactively, the current
buffer selection is used as the TERM."

  (interactive "sSearch term (default to selection): ")

  (when (eq term "")
    (setq term (buffer-substring (region-beginning) (region-end))))

  (setq term (replace-regexp-in-string " +" "+" term))

  (unless (boundp 'search-engine-query-url)
    (setq search-engine-query-url "https://duckduckgo.com/?q="))

  (unless (boundp 'browser-command)
    (setq browser-command "firefox"))

  (let ((full_query_url (concat search-engine-query-url "'" term "'")))
    (shell-command (concat browser-command " '" full_query_url "'") nil nil)))
