;;; package --- Summary:
; Zim Wiki mode 

;;; Commentary:
; ffap code from:
;   https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a/

;;; Code:

(defgroup zw-mode nil
  "Major mode for a zim wiki notebook (dokuwiki derivitive)."
  :group 'text
  :prefix "zw-"
  :tag "ZimWiki"
:link '(url-link "http://zim-wiki.org"))

(defcustom zim-root "/home/foranw/notes/PersonalWiki"
  "The root folder for the zim wiki notebook"
  :group 'zw-mode
  :type 'string
 )
(defcustom zw-journal-datestr "Calendar/%Y/Week_%02u.txt"
  "Path as time format to journal pages."
  :group 'zw-mode
  :type 'string
  )
(defcustom zw-now-disp "[d: %Y-%m-%d]"
  "How to insert date/time."
  :group 'zw-mode
  :type 'string
  )


;; confiugre other packages
(require 'ffap)
(require 'neotree)
(require 'helm-ag)

(defun zw-now-page ()
  "What is the path to the page for this time"
  (let ((datestr (format-time-string zw-journal-datestr)))
	(concat zim-root "/" datestr)))

(defun zw-goto-now ()
  "Go to the journal page for now"
  (interactive)
  (switch-to-buffer (find-file-noselect (zw-now-page)))
  ; TODO: if empyt buffer, add template?
  (zw-mode)
)

(defun zw-search ()
  "Search zim notebook with ag."
  (interactive)
  (helm-do-ag zim-root)
  (zw-mode)
)

(defun zw-mklink (path &optional text)
  "Make a link from a PATH with optional TEXT. [[path]] or [[path|text]]"
  (let* ((text (if text (concat "|" text) "")))
   (concat "[[" path text "]]")))

(defun zw-link-now ()
  "link to current day"
  (zw-mklink
     (zw-path2wiki (zw-now-page))
     (format-time-string zw-now-disp)))

;; :a:b to /zim-root/a/b.txt
;; TODO: +a:b $(cwd)/a/b/.txt
;;       [[a:b]] $(cwd)/a/b.txt should just work (?)
;;       deal with spaces
(defun zw-wiki2path (zp)
  "Transform zim link ZP (':a:b') to file path /root/a/b.txt ."
  (let*
      ((zr (concat zim-root "/" ))
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
  "Transform path P ('./a/b.txt') to wike path."
  (let*
      ((zp (replace-regexp-in-string "^./" "+" zp)) ; relative is +
       (zp (replace-regexp-in-string ".txt" "" zp)) ; no extension
       (zp (replace-regexp-in-string (concat "^" zim-root) ":" zp)) ; root
       (zp (replace-regexp-in-string "/" ":"  zp)) ; all slashes to :
       ; add [] -- done by zw-mklink
       ;(zp (replace-regexp-in-string "^" "["  zp))
       ;(zp (replace-regexp-in-string "$" "]"  zp)))
       (zp (replace-regexp-in-string ":+" ":"  zp)) ; replace extra :'s
       )
    zp))

(defun zw-insert-now-link ()
  "Insert now string in current buffer"
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
      (goto-char (point-max)) ; TODO: change for week
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
    (zw-mode )))

;; with selection?
(defun zw-vfap (&optional wikipath)
  "Read only mode zw-ffap WIKIPATH?"
  (interactive)
  (zw-ffap wikipath)
  (read-only-mode))

(defun zw-deft ()
  "Deft search without evil mode."
  (interactive)
  (deft)
  (evil-insert-state nil)
)



; http://ergoemacs.org/emacs/elisp_create_major_mode_keymap.html
(defvar zw-mode-map
  (let ((map (make-sparse-keymap)))
    ; links
    (define-key map (kbd "C-c f")   'zw-search)       ; find in all of notebook
    (define-key map (kbd "C-c RET") 'zw-ffap)         ; go to link

    ;date/time
    (define-key map (kbd "C-c N")   'zw-goto-now)       ; go to now page
    (define-key map (kbd "C-c n")   'zw-insert-now-link); link to curret date/time
    (define-key map (kbd "C-c C-n") 'zw-insert-current-at-now) ; insert cur page into now page

    ; tree 
    (define-key map (kbd "C-c t")   'neotree-toggle)  ; toggle tree
    (define-key map (kbd "C-c T")   'neotree-find)    ; find thing in tree

    map)
   "Keymap for zw-mode.")

(define-derived-mode zw-mode text-mode "zm"
  "Major mode for eding zim wiki."
  (dokuwiki-mode)             ; start with wikimode
  (use-local-map zw-mode-map) ; unnecessary?

  ; deft -- probably want to use helm-ag instead
  (set (make-local-variable 'deft-directory) zim-root)
  (set (make-local-variable 'deft-recursive) t)
  (set (make-local-variable 'deft-strip-summary-regexp)
        (concat "^\\(Content-Type.*\n*\\)?"
  	      "\\(Wiki-Format:.*\n*\\)?"
  	      "\\(Creation-Date:.*\n*\\)?"
  	      "\\( *======.*\nCreated .*\n*\\)?") )
  (set (make-local-variable 'deft-use-filename-as-title) t)
)

(provide 'zw-mode)


; quick tests -- because earlier did something dumb
(ert-deftest mklink-full () (should (= (zw-mklink "foo:bar" "baz") "[[foo:bar|baz]]")))
(ert-deftest mklink-path () (should (= (zw-mklink "foo:bar") "[[foo:bar]]")))


;; TODO: bindings
;; C-c f => file to link
;; C-c C-w => go to week, insert from page under date
;; C-c l => insert link (+search?)

;; prettify headers?
;; https://github.com/sabof/org-bullets/blob/master/org-bullets.el

; dont use deft, use helm-ag instead
; (unload-feature 'deft t)
; (setq deft-directory zim-root)
; (setq deft-recursive t)
; (setq deft-strip-summary-regexp
;       (concat "^\\(Content-Type.*\n*\\)?"
; 	      "\\(Wiki-Format:.*\n*\\)?"
; 	      "\\(Creation-Date:.*\n*\\)?"
; 	      "\\( *======.*\nCreated .*\n*\\)?") )
; (setq deft-use-filename-as-title t)
; (require 'deft)
; 
; ; get out of evilmode
; ; http://huxiaoxing.com/setup/emacs.html#org086e564
