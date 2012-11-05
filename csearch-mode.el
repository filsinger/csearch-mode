;; csearch-mode.el
;;
;; url: https://github.com/filsinger/csearch-mode/
;;
;; author: Jason Filsinger (https://github.com/filsinger)
;;
;; version 0.4.2
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
;; csearch-mode can automatically re-index files on save by adding the following
;; to your .emacs config:
;;
;;   (add-hook 'after-save-hook (lambda () (csearch/index-add (buffer-file-name) nil t)))
;;


(require 'grep)

;;;###autoload
(defgroup csearch-mode nil
  "*csearch"
  :group 'csearch-mode)

;;;###autoload
(defcustom csearch/csearch-program "csearch"
  "Path to the csearch executable"
  :type '(file :must-match t)
  :group 'csearch-mode)

;;;###autoload
(defcustom csearch/cindex-program "cindex"
  "Path to the csearch executable"
  :type '(file :must-match t)
  :group 'csearch-mode)

;;;###autoload
(defcustom csearch/index-file nil
  "Codesearch index file.  This value is assigned to the environment variable CSEARCHINDEX before invoking csearch.  Default value is `~/.csearchindex'"
  :type 'file
  :group 'csearch-mode)

;;;###autoload
(defcustom csearch/result-line-offset 1
  "Amount to offset the csearch result line numbers."
  :type 'number
  :group 'csearch-mode)

(defcustom csearch/result-regexp "^\\([a-zA-Z]*:*/*.+?\\):\\([0-9]+\\):\\(.+\\)$"
  "csearch restult match regexp"
  :type 'regexp
  :group 'csearch-mode)

(defcustom csearch/match-face 'match
  "Face name to use for csearch matches"
  :type 'face
  :group 'csearch-mode)

(defcustom csearch/error-face 'compilation-error
  "Face name to use for csearch matches"
  :type 'face
  :group 'csearch-mode)

(defcustom csearch/hit-face compilation-info-face
  "Face name to use for grep hits."
  :type 'face
  :group 'csearch-mode)

(defcustom csearch/context-face 'shadow
  "Face name to use for grep context lines."
  :type 'face
  :group 'csearch-mode)

;;;###autoload
(defcustom csearch/ignore-regexp-list
  '(".xcodeproj/"
	"/.git/")
  "*Alist of all ignore"
  :group 'list
  :group 'csearch-mode)

(defvar csearch/search-history nil
  "Search history for csearch")

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
  (or (getenv "CSEARCHINDEX")
      (if (file-exists-p (file-truename "~/.csearchindex"))
          (file-truename "~/.csearchindex")
        (if (getenv "HOMEPATH")
            (file-truename (format "%s/.csearchindex" (getenv "HOMEPATH")))
          nil))))

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
(defun csearch/filter ()
  "Handle match highlighting escape sequences inserted by the grep process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
		;;delete the files in the ignore list
		(let ((kill-whole-line t))
		  (while (re-search-forward (regexp-opt csearch/ignore-regexp-list) end 1)
			(goto-char (match-beginning 0))
			(goto-char (point-at-bol))
			(kill-line)))
        ;; Highlight grep matches and delete marking sequences.
        (goto-char beg)
        (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[[0-9]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face csearch/match-face)
                         t t))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))
        ;; offset the line numbers by 1 (csearch uses the 0 as the first line number, emacs uses 1
        (when csearch/result-line-offset
          (goto-char beg)
          (while (re-search-forward csearch/result-regexp end 1)
            (replace-match (number-to-string (+ (string-to-number (match-string 2)) csearch/result-line-offset)) t t nil 2 ) )
		  )))))

;;;###autoload
(define-compilation-mode csearch-mode "csearch"
  "Sets `csearch-last-buffer' and `compilation-window-height'."
  (setq csearch-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-face) csearch/hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist) grep-regexp-alist)
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-process-setup-function) 'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) grep-error-screen-columns)
  (add-hook 'compilation-filter-hook 'csearch/filter nil t)
  )

;;;###autoload
(defun csearch/csearch (regexp &optional case-insensitive index-file)
  "Run the csearch tool and search for the provided REGEXP

If CASE-INSENSITIVE is provided then csearch will perform a
case-insensitive search.  If INDEX-FILE is provided then
csearch will use the INDEX-FILE for it's search index.
"
  (compilation-start
   (format "%s %s %s" (if csearch/csearch-program csearch/csearch-program "csearch") (if case-insensitive "-in " "-n") regexp)
   'csearch-mode))


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
              (read-string (format "%s%s: " prompt (if default-search-string (format " [default: %s]" default-search-string) "") ) nil history default-search-string) )))

;;;###autoload
(defun csearch/search (regexp)
  "Display a list of all symbols in the csearch index that REGEXP matches"
  (interactive (list (csearch/read-string-region-or-prompt-string "Symbol (word or regexp)" 'csearch/search-history)) )
  (csearch/csearch regexp nil csearch/index-file))

;;;###autoload
(defun csearch/find-file-hook-function ()
  "This function can be called from the find-file-hook to automatically
seach the directory tree upwards for a \".csearchindex\" file."
  (let ((tree-index (csearch/find-file-upward ".csearchindex")))
    (set (make-local-variable 'csearch/index-file) tree-index)))

(provide 'csearch-mode)
