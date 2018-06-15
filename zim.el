; ffap code from:
;   https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a/

(setq zim-root "/home/foranw/src/notes/zimwork/")
(replace-regexp-in-string "^:+" zim-root zimpath)
;; :a:b to /zim-root/a/b.txt
;; TODO: +a:b $(cwd)/a/b/.txt
;;       [[a:b]] $(cwd)/a/b.txt should just work (?)
;;       deal with spaces
(defun zim-path (zp)
  (let*
      ((zp (replace-regexp-in-string "^:+" zim-root zp))
       (zp (replace-regexp-in-string ":+" "/"  zp))
       ; anything after a pipe
       (zp (replace-regexp-in-string "\|.*" ""  zp))
       ; remove any [ or ]
       (zp (replace-regexp-in-string "[][]" ""  zp))
       (zp (concat zp ".txt") ))
      zp
   )
)

;; at point
(defun ffap-zim (&optional wikipath)
  (interactive)
  (let* (;(name (or wikipath (word-at-point)))
         (name (or wikipath (ffap-string-at-point 'file)))
         (name (zim-path name))
         (fname (expand-file-name name)))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
      (find-file-at-point fname))))

;; with selection?
(defun vfap-zim (&optional filename)
  (interactive)
  (ffap-zim filename)
  (read-only-mode))

;(set-key [(control c)] 'ffap-zim)

;; TODO: bindings
;; C-c RET => ffap-zim
;; C-c t => toggle neotree?
;; C-c T => neotree-find
;; C-c f => file to link
;; C-c d => insert date
;; C-c w => go to week
;; C-c C-w => go to week, insert from page under date
;; C-c l => insert link (+search?)
