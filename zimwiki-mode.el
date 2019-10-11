;;; zimwiki-mode.el --- Zim Deskopt Wiki edit mode          -*- lexical-binding: t; -*-

;; URL: https://github.com/WillForan/zimwiki-mode
;; Author: Will Foran <willforan+zimwiki-mode@gmail.com>
;; Keywords: outlines
;; Package-Requires: ((emacs "25") (helm-ag "0.58") (helm-projectile "0.14.0") (dokuwiki-mode "0.1.1"))
;; Version: 0.0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;; Edit zim wiki txt files within Emacs
;
; ffap code from:
;   https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a/


;;; Code:


;; configure other packages
(require 'ffap)
;(require 'org-mode)
;(require 'neotree)

(defgroup zimwiki nil
  "Major mode for a zim wiki notebook (dokuwiki derivitive)."
  :group 'text
  :prefix "zimwiki-"
  :tag "ZimWiki"
:link '(url-link "http://zim-wiki.org"))

(defcustom zimwiki-root (expand-file-name "~/notes/PersonalWiki")
  "The root folder for the zim wiki notebook."
  :group 'zimwiki
  :type 'string
 )
(defcustom zimwiki-journal-datestr "Calendar/%Y/Week_%02V.txt"
  "Path as time format to journal pages."
  :group 'zimwiki
  :type 'string
  )
(defcustom zimwiki-now-disp "[d: %Y-%m-%d]"
  "How to insert date/time."
  :group 'zimwiki
  :type 'string
  )


(defun zimwiki-now-page ()
  "What is the path to the page for this time."
  (let ((datestr (format-time-string zimwiki-journal-datestr)))
	(concat zimwiki-root "/" datestr)))

(defun zimwiki-goto-now ()
  "Go to the journal page for now."
  (interactive)
  (switch-to-buffer (find-file-noselect (zimwiki-now-page)))
  ; TODO: if empyt buffer, add date template? have zimwiki-insert-header, but it wont be date
  (zimwiki-mode)
)

(defun zimwiki-search ()
  "Search zim notebook with ag."
  (interactive)
  (helm-do-ag zimwiki-root)
  (zimwiki-mode)
)

(defun zimwiki-mklink (path &optional text)
  "Make a link from a PATH with optional TEXT: [[path]] or [[path|text]]."
  (let* ((text (if text (concat "|" text) "")))
   (concat "[[" path text "]]")))

(defun zimwiki-link-now ()
  "Link to current day."
  (zimwiki-mklink
     (zimwiki-path2wiki (zimwiki-now-page))
     (format-time-string zimwiki-now-disp)))

;; :a:b to /zimwiki-root/a/b.txt
;; TODO: +a:b $(cwd)/a/b/.txt
;;       [[a:b]] $(cwd)/a/b.txt should just work (?)
;;       deal with spaces
(defun zimwiki-wiki2path (zp)
  "Transform zim link ZP (':a:b') to file path /root/a/b.txt ."
  (let*
      ((zr (concat zimwiki-root "/" ))
       (zp (replace-regexp-in-string "^\\+" "" zp))
       (zp (replace-regexp-in-string "^:+" zr zp))
       (zp (replace-regexp-in-string ":+" "/"  zp))
       ; anything after a pipe
       (zp (replace-regexp-in-string "\\|.*" ""  zp))
       ; remove any [ or ]
       (zp (replace-regexp-in-string "[][]" ""  zp))
       (zp (concat zp ".txt") ))
      zp
   )
)

(defun zimwiki-path2wiki (zp)
  "Transform path ZP ('./a/b.txt') to wike path."
  (let*
      (
       (zp (replace-regexp-in-string "^\\./" "+" zp)) ; relative is +

       ; various ways the file to be linked can have root in it:
       ;  * normal: /home/b/blah
       ;  * home alias: ~/b/blah
       ;  * symlink: ~/b/blah -> /emulated/0/storage/blah
       (zp (replace-regexp-in-string (concat "^" zimwiki-root) ":" zp))
       (zp (replace-regexp-in-string
	    (concat "^" (expand-file-name zimwiki-root)) ":" zp))
       (zp (replace-regexp-in-string
	    (concat "^" (expand-file-name (file-truename zimwiki-root)))
	    ":" zp))
       ; no .txt,  / becomes :, no repeat :
       (zp (replace-regexp-in-string ".txt" "" zp)) ; no extension
       (zp (replace-regexp-in-string "/" ":"  zp)) ; all slashes to :
       (zp (replace-regexp-in-string ":+" ":"  zp)) ; replace extra :'s
       )
    zp))

(defun zimwiki-insert-now-link ()
  "Insert now string in current buffer."
  (interactive)
  (insert (zimwiki-link-now))
)

(defun zimwiki-insert-current-at-now ()
  "Insert current page into now page (and go to now page)."
  (interactive)
  (let ((cur (zimwiki-path2wiki (buffer-file-name))))
    (progn
      (zimwiki-goto-now)
      ; go to end
      (goto-char (point-max)) ; TODO: change for week -- search for week dayname
      (insert "\n")
      (insert (zimwiki-mklink cur)))))


;; at point
(defun zimwiki-ffap (&optional wikipath)
  "Goto file from WIKIPATH."
  (interactive)
  (let* (;(name (or wikipath (word-at-point)))
         (name (or wikipath (ffap-string-at-point 'file)))
         (name (zimwiki-wiki2path name))
         (fname (expand-file-name name)))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
        (find-file-at-point fname))
    (if (= (buffer-size) 0) (zimwiki-insert-header))
    (zimwiki-mode )))

(defun zimwiki-ffap-below ()
  "Open a link in new window below current."
  (interactive)
  (with-selected-window (split-window-below) (zimwiki-ffap))
)

;; with selection?
(defun zimwiki-vfap (&optional wikipath)
  "Read only mode ‘zimwiki-ffap’ WIKIPATH?"
  (interactive)
  (zimwiki-ffap wikipath)
  (read-only-mode))


; find a page
(defun zimwiki-helm-projectile ()
  "Go to a file using ‘helm-projectile’ (requires notebook in VCS)."
  (interactive)
  (helm-projectile)
  (zimwiki-mode))

; find a page but dont go there, just insert it
(defun zimwiki-buffer-to-link (buffer)
  "Make a link of a given BUFFER."
  (zimwiki-mklink (zimwiki-path2wiki (expand-file-name (buffer-file-name buffer))))
)

(defun zimwiki-buffer-close-insert (cur)
   "Go away from CUR buffer created soley to get link.  Probably a bad idea."
  ; TODO: find a way to restore buffer list. maybe dont kill the buffer incase it was already open?
  (let* ((res (current-buffer)))
   ;(switch-to-buffer cur) ; go back to where we are told
   (if (not (string= (buffer-file-name res) (buffer-file-name cur)))
    (progn (kill-buffer res) ; go back by killing buffer we just created
           (insert (zimwiki-buffer-to-link res))
	   ))
  )
)

;; search/projectile insert results
(defun zimwiki-insert-helm-projectile ()
  "Use projectile to insert on the current page.
Opens projectile buffer before switching back"
  (interactive)
  (let* ((cur (current-buffer)))
      (zimwiki-helm-projectile)
      (zimwiki-buffer-close-insert cur))
)

(defun zimwiki-insert-search ()
  "Search zim notebook with ag."
  (interactive)
  (let* ((cur (current-buffer)))
    (zimwiki-search)
    (zimwiki-buffer-close-insert cur))
)

; wrap in a link
; TODO: at-point for 'filename does not catpure + but does get :
(defun zimwiki-link-wrap ()
  "Wrap current word as link."
  (interactive)
  (let*
      ((bounds (bounds-of-thing-at-point 'filename))
      (x (car bounds))
      (y (cdr bounds))
      (s (buffer-substring-no-properties x y)))
    (progn
      (delete-region x y)
      (goto-char x)
      (insert (zimwiki-mklink s))
      )))
  
(defun zimwiki-insert-header ()
  "Insert header on a new page."
  (interactive)
  (goto-char 0)
  (insert (concat
      "Content-Type: text/x-zim-wiki\n"
      "Wiki-Format: zim 0.4\n"
      "Creation-Date: " (format-time-string "%Y-%m-%dT%H:%M:%S%z")  "\n"
      "====== "
      (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
      " ======\n"
      "Created: " (format-time-string "%A %d %B %Y") "\n";Created Thursday 17 May 2018
      ))
 )


(defun zimwiki-buffer-path-to-kill-ring ()
  "Put the current file full path onto the kill ring."
  (interactive)
  (kill-new (expand-file-name (buffer-file-name)))
)

(defun zimwiki-insert-kill-ring-as-link ()
  "Put the current file full path onto the kill ring."
  (interactive)
  (insert (zimwiki-mklink (zimwiki-path2wiki (current-kill 0))))
)
(defun zimwiki-insert-prev-buffer-link ()
   "Link previous buffer path as wiki."
   (interactive)
   (insert (zimwiki-buffer-to-link (other-buffer (current-buffer) 1)))
)

; http://ergoemacs.org/emacs/elisp_create_major_mode_keymap.html
(defvar zimwiki-mode-map
  (let ((map (make-sparse-keymap)))
    ; go places
    (define-key map (kbd "C-c f")     'zimwiki-helm-projectile); go to a page by searching file names
    (define-key map (kbd "C-c C-f")   'zimwiki-search)         ; find in all of notebook
    (define-key map (kbd "C-c RET")   'zimwiki-ffap)           ; go to link
    (define-key map (kbd "C-c M-RET") 'zimwiki-ffap-below)     ; go to link in new window

    ; make links
    (define-key map (kbd "C-c l")   'zimwiki-insert-helm-projectile); 
    (define-key map (kbd "C-c C-l") 'zimwiki-insert-search)         ; 

    (define-key map (kbd "C-c w")   'zimwiki-link-wrap)      ; a:b -> [[a:b]]
    (define-key map (kbd "C-c y")   'zimwiki-buffer-path-to-kill-ring) ; copy current file path
    (define-key map (kbd "C-c p")   'zimwiki-insert-kill-ring-as-link) ; paste as a link
    (define-key map (kbd "C-c C-p") 'zimwiki-insert-prev-buffer-link) ; buffer before this one as a wiki link

    ;date/time
    (define-key map (kbd "C-c n")   'zimwiki-goto-now)       ; go to now page
    (define-key map (kbd "C-c N")   'zimwiki-insert-now-link); link to curret date/time
    (define-key map (kbd "C-c C-n") 'zimwiki-insert-current-at-now) ; insert cur page into now page (and go there)

    ; tree
    ;(define-key map (kbd "C-c t")   'neotree-toggle)  ; toggle tree
    ;(define-key map (kbd "C-c T")   'neotree-find)    ; find thing in tree

    ; org mode theft
    ;(define-key map (kbd "M-RET")   'org-insert-item)    ; insert new list item

    map)
   "Keymap for ‘zimwiki-mode’.")

(define-derived-mode zimwiki-mode text-mode "zimwiki"
  "Major mode for eding zim wiki."
  (dokuwiki-mode)             ; start with wiki mode
  (flyspell-mode)
  (use-local-map zimwiki-mode-map) ; unnecessary?

)

(provide 'zimwiki-mode)


; quick tests -- because earlier did something dumb
(ert-deftest zimwiki-test-mklink-full () (should (string= (zimwiki-mklink "foo:bar" "baz") "[[foo:bar|baz]]")))
(ert-deftest zimwiki-test-mklink-path () (should (string= (zimwiki-mklink "foo:bar") "[[foo:bar]]")))

(ert-deftest zimwiki-test-path2wiki ()
  (let ((zimwiki-root "/a/b"))
    (should (string= (zimwiki-path2wiki "./a/b") "+a:b"))
    (should (string= (zimwiki-path2wiki "/a/b/c/d") ":c:d"))
    (should (string= (zimwiki-path2wiki "/a/b/e/f.txt") ":e:f"))))

(ert-deftest zimwiki-test-path2wiki-tilda ()
  (let ((zimwiki-root "~/a/b"))
    (should (string= (zimwiki-path2wiki (expand-file-name "~/a/b/c/d")) ":c:d"))
    (should (string= (zimwiki-path2wiki "~/a/b/y/z") ":y:z"))))

;; TODO:
;;  * agenda "[ ] task [d: yyyy-mm-dd]"
;;  * backlink collection (use sqlitedb? zimwiki uses?)
;;  * tags

;; prettify headers?
; https://github.com/sabof/org-bullets/blob/master/org-bullets.el

;;; zimwiki-mode.el ends here
