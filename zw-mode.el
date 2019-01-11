;;; zw-mode.el --- Zim Deskopt Wiki edit mode          -*- lexical-binding: t; -*-

;; URL: https://github.com/WillForan/zw-mode
;; Author: Will Foran <willforan+zw-mode@gmail.com>
;; Keywords: outlines
;; Package-Requires: ((emacs "25") (helm-ag "0.58") (helm-projectile "0.14.0") (dokuwiki-mode "0.1.1"))
;; Version: 0.0.1

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

(defgroup zw nil
  "Major mode for a zim wiki notebook (dokuwiki derivitive)."
  :group 'text
  :prefix "zw-"
  :tag "ZimWiki"
:link '(url-link "http://zim-wiki.org"))

(defcustom zw-root "/home/foranw/notes/PersonalWiki"
  "The root folder for the zim wiki notebook."
  :group 'zw
  :type 'string
 )
(defcustom zw-journal-datestr "Calendar/%Y/Week_%02V.txt"
  "Path as time format to journal pages."
  :group 'zw
  :type 'string
  )
(defcustom zw-now-disp "[d: %Y-%m-%d]"
  "How to insert date/time."
  :group 'zw
  :type 'string
  )


(defun zw-now-page ()
  "What is the path to the page for this time."
  (let ((datestr (format-time-string zw-journal-datestr)))
	(concat zw-root "/" datestr)))

(defun zw-goto-now ()
  "Go to the journal page for now."
  (interactive)
  (switch-to-buffer (find-file-noselect (zw-now-page)))
  ; TODO: if empyt buffer, add date template? have zw-insert-header, but it wont be date
  (zw-mode)
)

(defun zw-search ()
  "Search zim notebook with ag."
  (interactive)
  (helm-do-ag zw-root)
  (zw-mode)
)

(defun zw-mklink (path &optional text)
  "Make a link from a PATH with optional TEXT: [[path]] or [[path|text]]."
  (let* ((text (if text (concat "|" text) "")))
   (concat "[[" path text "]]")))

(defun zw-link-now ()
  "Link to current day."
  (zw-mklink
     (zw-path2wiki (zw-now-page))
     (format-time-string zw-now-disp)))

;; :a:b to /zw-root/a/b.txt
;; TODO: +a:b $(cwd)/a/b/.txt
;;       [[a:b]] $(cwd)/a/b.txt should just work (?)
;;       deal with spaces
(defun zw-wiki2path (zp)
  "Transform zim link ZP (':a:b') to file path /root/a/b.txt ."
  (let*
      ((zr (concat zw-root "/" ))
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

(defun zw-path2wiki (zp)
  "Transform path ZP ('./a/b.txt') to wike path."
  (let*
      (
       (zp (replace-regexp-in-string "^\\./" "+" zp)) ; relative is +
       (zp (replace-regexp-in-string (concat "^" zw-root) ":" zp)) ; root
       (zp (replace-regexp-in-string (concat "^" (expand-file-name zw-root)) ":" zp)) ; root
       (zp (replace-regexp-in-string ".txt" "" zp)) ; no extension
       (zp (replace-regexp-in-string "/" ":"  zp)) ; all slashes to :
       (zp (replace-regexp-in-string ":+" ":"  zp)) ; replace extra :'s
       )
    zp))

(defun zw-insert-now-link ()
  "Insert now string in current buffer."
  (interactive)
  (insert (zw-link-now))
)

(defun zw-insert-current-at-now ()
  "Insert current page into now page (and go to now page)."
  (interactive)
  (let ((cur (zw-path2wiki (buffer-file-name))))
    (progn
      (zw-goto-now)
      ; go to end
      (goto-char (point-max)) ; TODO: change for week -- search for week dayname
      (insert "\n")
      (insert (zw-mklink cur)))))


;; at point
(defun zw-ffap (&optional wikipath)
  "Goto file from WIKIPATH."
  (interactive)
  (let* (;(name (or wikipath (word-at-point)))
         (name (or wikipath (ffap-string-at-point 'file)))
         (name (zw-wiki2path name))
         (fname (expand-file-name name)))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
        (find-file-at-point fname))
    (if (= (buffer-size) 0) (zw-insert-header))
    (zw-mode )))

(defun zw-ffap-below ()
  "Open a link in new window below current."
  (interactive)
  (with-selected-window (split-window-below) (zw-ffap))
)

;; with selection?
(defun zw-vfap (&optional wikipath)
  "Read only mode zw-ffap WIKIPATH?"
  (interactive)
  (zw-ffap wikipath)
  (read-only-mode))


; find a page
(defun zw-helm-projectile ()
  "Go to a file using helm-projectile (requires notebook in VCS)."
  (interactive)
  (helm-projectile)
  (zw-mode))

; find a page but dont go there, just insert it
(defun zw-buffer-to-link (buffer)
  "Make a link of a given BUFFER."
  (zw-mklink (zw-path2wiki (expand-file-name (buffer-file-name buffer))))
)

(defun zw-buffer-close-insert (cur)
   "Go away from CUR buffer created soley to get link.  Probably a bad idea."
  ; TODO: find a way to restore buffer list. maybe dont kill the buffer incase it was already open?
  (let* ((res (current-buffer)))
   ;(switch-to-buffer cur) ; go back to where we are told
   (if (not (string= (buffer-file-name res) (buffer-file-name cur)))
    (progn (kill-buffer res) ; go back by killing buffer we just created
           (insert (zw-buffer-to-link res))
	   ))
  )
)

;; search/projectile insert results
(defun zw-insert-helm-projectile ()
  "Use projectile to insert on the current page.
Opens projectile buffer before switching back"
  (interactive)
  (setq cur (current-buffer))
  (zw-helm-projectile)
  (zw-buffer-close-insert cur)
)

(defun zw-insert-search ()
  "Search zim notebook with ag."
  (interactive)
  (let* ((cur (current-buffer)))
    (zw-search)
    (zw-buffer-close-insert cur))
)

; wrap in a link
; TODO: at-point for 'filename does not catpure + but does get :
(defun zw-link-wrap ()
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
      (insert (zw-mklink s))
      )))
  
(defun zw-insert-header ()
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


(defun zw-buffer-path-to-kill-ring ()
  "Put the current file full path onto the kill ring."
  (interactive)
  (kill-new (expand-file-name (buffer-file-name)))
)

(defun zw-insert-kill-ring-as-link ()
  "Put the current file full path onto the kill ring."
  (interactive)
  (insert (zw-mklink (zw-path2wiki (current-kill 0))))
)
(defun zw-insert-prev-buffer-link ()
   "Link previous buffer path as wiki."
   (interactive)
   (insert (zw-buffer-to-link (other-buffer (current-buffer) 1)))
)

; http://ergoemacs.org/emacs/elisp_create_major_mode_keymap.html
(defvar zw-mode-map
  (let ((map (make-sparse-keymap)))
    ; go places
    (define-key map (kbd "C-c f")     'zw-helm-projectile); go to a page by searching file names
    (define-key map (kbd "C-c C-f")   'zw-search)         ; find in all of notebook
    (define-key map (kbd "C-c RET")   'zw-ffap)           ; go to link
    (define-key map (kbd "C-c M-RET") 'zw-ffap-below)     ; go to link in new window

    ; make links
    (define-key map (kbd "C-c l")   'zw-insert-helm-projectile); 
    (define-key map (kbd "C-c C-l") 'zw-insert-search)         ; 

    (define-key map (kbd "C-c w")   'zw-link-wrap)      ; a:b -> [[a:b]]
    (define-key map (kbd "C-c y")   'zw-buffer-path-to-kill-ring) ; copy current file path
    (define-key map (kbd "C-c p")   'zw-insert-kill-ring-as-link) ; paste as a link
    (define-key map (kbd "C-c C-p") 'zw-insert-prev-buffer-link) ; buffer before this one as a wiki link

    ;date/time
    (define-key map (kbd "C-c n")   'zw-goto-now)       ; go to now page
    (define-key map (kbd "C-c N")   'zw-insert-now-link); link to curret date/time
    (define-key map (kbd "C-c C-n") 'zw-insert-current-at-now) ; insert cur page into now page (and go there)

    ; tree
    ;(define-key map (kbd "C-c t")   'neotree-toggle)  ; toggle tree
    ;(define-key map (kbd "C-c T")   'neotree-find)    ; find thing in tree

    ; org mode theft
    ;(define-key map (kbd "M-RET")   'org-insert-item)    ; insert new list item

    map)
   "Keymap for zw-mode.")

(define-derived-mode zw-mode text-mode "zw"
  "Major mode for eding zim wiki."
  (dokuwiki-mode)             ; start with wiki mode
  (flyspell-mode)
  (use-local-map zw-mode-map) ; unnecessary?

)

(provide 'zw-mode)


; quick tests -- because earlier did something dumb
(ert-deftest zw-test-mklink-full () (should (string= (zw-mklink "foo:bar" "baz") "[[foo:bar|baz]]")))
(ert-deftest zw-test-mklink-path () (should (string= (zw-mklink "foo:bar") "[[foo:bar]]")))

(ert-deftest zw-test-path2wiki ()
  (let ((zw-root "/a/b"))
    (should (string= (zw-path2wiki "./a/b") "+a:b"))
    (should (string= (zw-path2wiki "/a/b/c/d") ":c:d"))
    (should (string= (zw-path2wiki "/a/b/e/f.txt") ":e:f"))))

(ert-deftest zw-test-path2wiki-tilda ()
  (let ((zw-root "~/a/b"))
    (should (string= (zw-path2wiki (expand-file-name "~/a/b/c/d")) ":c:d"))
    (should (string= (zw-path2wiki "~/a/b/y/z") ":y:z"))))

;; TODO:
;;  * agenda "[ ] task [d: yyyy-mm-dd]"
;;  * backlink collection (use sqlitedb? zimwiki uses?)
;;  * tags

;; prettify headers?
; https://github.com/sabof/org-bullets/blob/master/org-bullets.el

;;; zw-mode.el ends here
