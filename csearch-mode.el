;; csearch-mode.el
;;
;; url: https://github.com/filsinger/csearch-mode/
;;
;; author: Jason Filsinger
;;
;; version 0.1.0
;;
;; note: On OS X you might need to specify the path to the csearch executable.
;;       The osx GUI usually doesnt contain the propper search path
;;       for executable files (because it's not run from the shell).
;;
;;       e.g.
;;       (setq csearch/program "/usr/local/bin/csearch")
;;

(defgroup csearch-mode nil
  "*csearch"
  :group 'csearch-mode)

(defcustom csearch/program "csearch"
  "Path to the csearch executable"
  :type `string
  :group `csearch-mode)

(defcustom csearch/index-file nil
  "codesearch index file.  This value is assigned to the environment variable CSEARCHINDEX before invoking csearch.  Default value is `~/.csearchindex'"
  :type 'string
  :group `csearch-mode)

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

(defun csearch/insert-sorted-lines (ARG &optional REVERSE)
  ""
  (save-excursion
	(let ((entries-start (point)))
	  (insert ARG)
	  (sort-lines nil entries-start (point)))))

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
							(if csearch/program csearch/program "csearch")
							" "
							(when case-insensitive "-i ")
							"-n "
							regexp))
		  (prev-index-file (getenv "CSEARCHINDEX")))
	  (setenv "CSEARCHINDEX" index-file)
	  (insert "Type RET on an entry to open file\n\n")
	  (insert (concat "" csearch-command "\n"))
	  (insert (concat "Codesearch matches for regexp `" regexp "`:\n\n"))
	  (csearch/insert-sorted-lines (shell-command-to-string csearch-command))
	  (setenv "CSEARCHINDEX" prev-index-file))

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
(defun csearch/apropos (regexp)
  "Display a list of all symbols in the csearch index that REGEXP matches"
  (interactive "MSymbol (word or regexp): ")
  (csearch/csearch regexp nil csearch/index-file))

(provide 'csearch-mode)
