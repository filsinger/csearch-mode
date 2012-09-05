;; csearch-mode.el
;;
;; url: https://github.com/filsinger/csearch-mode/
;;
;; author: Jason Filsinger (https://github.com/filsinger)
;;
;; version 0.2.0
;;
;; note: On OS X you might need to specify the path to the csearch executable.
;;       The osx GUI usually doesnt contain the propper search path
;;       for executable files (because it's not run from the shell).
;;
;;       e.g.
;;       (setq csearch/csearch-program "/usr/local/bin/csearch")
;;       (setq csearch/cindex-program "/usr/local/bin/cindex")
;;
;;
;; csearch-mode can automatically set the 'csearch/index-file when a '.cseachindex' file
;; is found in upward in the directory tree.  This can be done by adding the following to
;; your .emacs config:
;;
;;   (add-hook 'find-file-hook 'csearch/find-file-hook-function)
;;


(defgroup csearch-mode nil
  "*csearch"
  :group 'csearch-mode)

(defcustom csearch/csearch-program "csearch"
  "Path to the csearch executable"
  :type '(file :must-match t)
  :group 'csearch-mode)

(defcustom csearch/cindex-program "cindex"
  "Path to the csearch executable"
  :type '(file :must-match t)
  :group 'csearch-mode)

(defcustom csearch/index-file nil
  "Codesearch index file.  This value is assigned to the environment variable CSEARCHINDEX before invoking csearch.  Default value is `~/.csearchindex'"
  :type 'file
  :group 'csearch-mode)

(defcustom csearch/auto-find-index t
  "Automatically seach the directory tree upwards for '.csearchindex' when opening a file"
  :type 'boolean
  :group 'csearch-mode)

(defvar csearch/seach-history nil
  "Search history for csearch")

(defvar csearch/match-face 'match
  "Face name to use for csearch matches")

(defvar csearch/error-face 'compilation-error
  "Face name to use for csearch matches")

(defvar csearch/font-lock-keywords
  `(("^.+\\(command not found\\): .+$" 0 csearch/error-face)

	("^.+open \\(.+\\):.?+\\(no such file or directory\\)" 0 csearch/error-face)
	)
  "Additional things to highlight in the csearch output.
This gets tacked on the end of generated expressions.")

;;;###autoload
(defun csearch/index-set (index-path)
  "Set the current csearch index"
  (interactive "GIndex file: ")
  (let ((new-index-path (expand-file-name (if (file-directory-p index-path) (format "%s/.csearchindex" index-path) index-path))))
	(unless (file-directory-p (file-name-directory index-path)) (error (format "Not a valid .csearchindex: %s" index-path)))
	(setenv "CSEARCHINDEX" new-index-path) ))

;;;###autoload
(defun csearch/index-get ()
  "Get the current csearch index path"
  (or (getenv "CSEARCHINDEX") (file-truename "~/.csearchindex")) )

;;;###autoload
(defmacro csearch/with-index-file (index-file &rest body)
  "Execute the forms in BODY with the index-file temporarly set as the current csearch index."
  (declare (indent 1) (debug t))
  `(let ((prev-index-file (csearch/index-get)))
	  (progn (when ,index-file (csearch/index-set (file-truename ,index-file)))
			 ,@body
			 (csearch/index-set prev-index-file))))

;;;###autoload
(defun csearch/find-file-upward (file &optional startdir)
  "Search the directory tree upwards for a specific file"
  (let ((dirname (expand-file-name (if startdir startdir (file-name-directory (buffer-file-name)))))
		(found nil))
	(while (not (or found (string= dirname "/")))
	  (setq found (if (file-exists-p (format "%s%s" dirname file )) dirname nil))
	  (setq dirname (expand-file-name "../" dirname)) )
	(expand-file-name file found) ))

;;;###autoload
(defun csearch/insert-sorted-lines (ARG &optional REVERSE)
  ""
  (save-excursion
	(let ((entries-start (point)))
	  (insert ARG)
	  (sort-lines nil entries-start (point)))))

;;;###autoload
(defun csearch/csearch (regexp &optional case-insensitive index-file)
  "Run the csearch tool and search for the provided REGEXP

If CASE-INSENSITIVE is provided then csearch will perform a
case-insensitive search.  If INDEX-FILE is provided then
csearch will use the INDEX-FILE for it's search index.
"
  (with-current-buffer (switch-to-buffer-other-window "*csearch-list*")
  	(setq buffer-read-only nil)
  	(erase-buffer)
	(let ((csearch-command (concat
							(if csearch/csearch-program (file-truename csearch/csearch-program) "csearch")
							" "
							(when case-insensitive "-i ")
							"-n "
							regexp)) )
	  (insert "Type RET on an entry to open file\n\n")
	  (insert (concat "" csearch-command "\n"))
	  (insert (concat "Codesearch matches for regexp `" regexp "`:\n\n"))
	  (csearch/with-index-file index-file
		(csearch/insert-sorted-lines (shell-command-to-string csearch-command)) ) )
	(goto-char (point-min))
	(forward-line 3)
	(while (re-search-forward regexp nil t)
	  (replace-match (propertize (match-string 0) 'face nil 'font-lock-face csearch/match-face)))

	(goto-char (point-min))
	(forward-line 5)
	(font-lock-add-keywords 'grep-mode csearch/font-lock-keywords)
	(grep-mode)
	(setq buffer-read-only t)))


;;;###autoload
(defun csearch/index-reset (&optional index-file)
  "Delete all of the current csearch index"
  (interactive)
  (csearch/with-index-file index-file
	(shell-command-to-string (format "%s --reset" (if csearch/cindex-program csearch/cindex-program "cindex")))))

;;;###autoload
(defun csearch/index-add (path &optional index-file synchronous)
  "Add a path to the csearch index"
  (interactive "GPath:")
  (with-temp-message "generating cindex..."
	(let ((cindex-command (format "%s %s %s"
								  (shell-quote-argument (if csearch/cindex-program csearch/cindex-program "cindex"))
								  (expand-file-name path)
								  (if synchronous "" "&") )))
	  (csearch/with-index-file index-file (shell-command cindex-command "*cindex*") ) )))


;;;###autoload
(defun csearch/index-list (&optional index-file)
  "Returns a list of paths that were used to generate the current cindex"
  (let (cindex-list)
	(with-temp-buffer
	  (csearch/with-index-file index-file
		(shell-command (format "%s --list" (shell-quote-argument (if csearch/cindex-program csearch/cindex-program "cindex"))) (current-buffer)))
	  (save-excursion
		(goto-char (point-min))
		(while (re-search-forward "^\\([c-z/]+?:*[a-z/\\]+\\)$" nil t)
		  (add-to-list 'cindex-list (match-string 1))))
	  ) cindex-list) )

;;;###autoload
(defun csearch/index-add-list (file-list &optional index-file)
  "Add a list of paths to the csearch index"
  (let ((index-path ""))
	(loop for path in file-list do (setq index-path (format "%s %s" index-path (shell-quote-argument path))) )
	(csearch/with-index-file index-file
	  (shell-command (format "%s --reset%s &"
							 (shell-quote-argument (if csearch/cindex-program csearch/cindex-program "cindex"))
							 index-path)
					 "*cindex*") )))

;;;###autoload
(defun csearch/index-regenerate (&optional index-file)
  "Regenerate the current csearch index using the current index paths"
  (interactive)
  (csearch/index-add-list (csearch/index-list index-file) index-file)  )


;;;###autoload
(defun csearch/read-string-region-or-prompt-string (prompt &optional history non-interactive)
  "Similar to read-string except it sets the default value to the selected region or the thing-at-point ('symbol)"
  (let ((default-search-string (if (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)) (thing-at-point 'symbol) )))
		  (if non-interactive
			  (substring-no-properties default-search-string)
			  (read-string (format "%s%s: " prompt (if default-search-string (format " [default: %s]" default-search-string) "") ) nil 'history default-search-string) )))

;;;###autoload
(defun csearch/search (regexp)
  "Display a list of all symbols in the csearch index that REGEXP matches"
  (interactive (list (csearch/read-string-region-or-prompt-string "Symbol (word or regexp)" 'csearch/search-history)) )
  (csearch/csearch regexp nil csearch/index-file))

;;;###autoload
(defun csearch/find-file-hook-function ()
  ""
  (when csearch/auto-find-index
	(let ((tree-index (csearch/find-file-upward ".csearchindex")))
	  (set (make-local-variable 'csearch/index-file) tree-index))))

(provide 'csearch-mode)
