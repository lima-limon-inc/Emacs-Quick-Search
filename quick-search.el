; Source: https://heemayl.net/posts/invoke-search-engine-on-browser-with-search-term-from-emacs/
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
