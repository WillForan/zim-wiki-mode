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
(require 'helm-projectile)

(defun zw-now-page ()
  "What is the path to the page for this time"
  (let ((datestr (format-time-string zw-journal-datestr)))
	(concat zim-root "/" datestr)))

(defun zw-goto-now ()
  "Go to the journal page for now"
  (interactive)
  (switch-to-buffer (find-file-noselect (zw-now-page)))
  ; TODO: if empyt buffer, add date template? have zw-insert-header, but it wont be date
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
    (if (= (buffer-size) 0) (zw-insert-header))
    (zw-mode )))

;; with selection?
(defun zw-vfap (&optional wikipath)
  "Read only mode zw-ffap WIKIPATH?"
  (interactive)
  (zw-ffap wikipath)
  (read-only-mode))


; find a page 
(defun zw-helm-projectile ()
  "go to a file using helm-projectile (req. notebook in VCS)"
  (interactive)
  (helm-projectile)
  (zw-mode))

; find a page but dont go there, just insert it
(defun zw-buffer-to-link (buffer)
  "make a link of a given buffer"
  (zw-mklink (zw-path2wiki (expand-file-name (buffer-file-name buffer))))
)

(defun zw-buffer-close-insert (cur)
   "go away from buffer created soley to get link. probably a bad idea"
  ; TODO: find a way to restore buffer list. maybe dont kill the buffer incase it was already open?
  (let* ((res (current-buffer)))
   ;(switch-to-buffer cur) ; go back to where we are told
   (kill-buffer res) ; go back by killing buffer we just created
   (insert (zw-buffer-to-link res))
  )
)

;; search/projectile insert results
(defun zw-insert-helm-projectile ()
  "use projectile to insert on the current page.
   opens projectile buffer before switching back"
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
; TODO: filename does not catpure + but does get :
(defun zw-link-wrap ()
  "wrap current word as link"
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
  "insert header on a new page"
  (interactive)
  (goto-char 0)
  (insert (concat
      "Content-Type: text/x-zim-wiki\n"
      "Wiki-Format: zim 0.4\n"
     ;"Creation-Date:.*\n*\\)?"
      " ====== "
      (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
      " ======\n"
      ;"Created:  \n*"
 )))

(defun zw-deft ()
  "Deft search without evil mode."
  (interactive)
  (deft)
  (evil-insert-state nil)
)


(defun zw-buffer-path-to-kill-ring ()
  "put the current file full path onto the kill ring"
  (interactive)
  (kill-new (expand-file-name (buffer-file-name)))
)

(defun zw-insert-kill-ring-as-link ()
  "put the current file full path onto the kill ring"
  (interactive)
  (insert (zw-mklink (zw-path2wiki (current-kill 0))))
)
(defun zw-insert-prev-buffer-link ()
   "link previous buffer path as wiki"
   (interactive)
   (insert (zw-buffer-to-link (other-buffer (current-buffer) 1)))
)

; http://ergoemacs.org/emacs/elisp_create_major_mode_keymap.html
(defvar zw-mode-map
  (let ((map (make-sparse-keymap)))
    ; go places
    (define-key map (kbd "C-c f")   'zw-helm-projectile); go to a page by searching file names
    (define-key map (kbd "C-c C-f") 'zw-search)         ; find in all of notebook
    (define-key map (kbd "C-c RET") 'zw-ffap)           ; go to link

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
    (define-key map (kbd "C-c t")   'neotree-toggle)  ; toggle tree
    (define-key map (kbd "C-c T")   'neotree-find)    ; find thing in tree

    map)
   "Keymap for zw-mode.")

(define-derived-mode zw-mode text-mode "zw"
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
  	      "\\( *======.*\n\\)?\\(Created .*\n*\\)?") ) 
  (set (make-local-variable 'deft-use-filename-as-title) t)
)

(provide 'zw-mode)


; quick tests -- because earlier did something dumb
(ert-deftest mklink-full () (should (= (zw-mklink "foo:bar" "baz") "[[foo:bar|baz]]")))
(ert-deftest mklink-path () (should (= (zw-mklink "foo:bar") "[[foo:bar]]")))



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
