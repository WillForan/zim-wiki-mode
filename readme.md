# zw-mode
An elisp package for editing [zim-wiki](http://zim-wiki.org) in emacs.

This package primarily provides convenient page linking and journaling by extending `dokuwiki-mode` and wrapping functions around `helm-projectile` and `helm-ag`.

### Motivation
Even though any editor can edit zim's plain text files, none are particularly efficient. Hopefully, `zw-mode` reduces that friction in emacs.

## Demo
using zw-mode with evil-mode and leuven theme

![demo gif](demo.gif?raw=true) 

  0. <kbd>C-c n</kbd> go to the "now" page
     * now page date format defined by `zw-journal-datestr`
  1. <kbd>C-c l</kbd> create a new page in hierarchy by searching current tree 
     * via `helm-projectile`
  2. <kbd>C-c RET</kbd> follow the link we just created
  3. <kbd>C-c N</kbd> insert a link to the current date 
  4. write up some text about what we did
  5. <kbd>C-c C-l</kbd> link in that page that has "emacs cider" content but whos name is not memberible
     * helm provides <kbd>C-z</kbd> to preview
  6. <kbd>C-c f</kbd> go to title: we want to add things to that just linked in page.
     * could <kbd>C-c RET</kbd> on the link we created, but lets go there with by search file names (page titles)
     * or go by search text again (<kbd>C-c C-f</kbd>)
  7. <kbd>C-c C-p</kbd> link prev page: while we are there lets add where we came from
  8. <kbd>C-c C-n</kbd> then link the page we are currently editing to the now page
  9. <kbd>C-c w</kbd> paths freehand and wrap them in a link

N.B.
  * when following a link, we need to be on the word (not the `[[` or `]]` part)
  * no autosave
  * wiki must be under version control for `helm-projectile` (<kbd>C-c f</kbd> and <kbd>C-c l</kbd>)

## Install
1. install emacs packages `ffap`, `dokuwiki-mode`, `helm-ag`, and `helm-projectile`
2. put [`zw-mode.el`](zw-mode.el?raw=true) within your load-path and load it.
3. customize `zim-root` to your zim wiki notebook location.
4. start wiki-ing with `M-x zw-goto-now`


### Try it 
copy and evalute (`M-x eval-buffer`) the following lines
```elisp
; install dependencies
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)
(package-install 'ffap)
(package-install 'dokuwiki-mode)
(package-install 'helm-ag)
(package-install 'helm-projectile) 
(package-install 'flyspell-mode) 

; get -- maybe read over, be sure it's not going to add you to a botnet
(browse-url-emacs "https://github.com/WillForan/zw-mode/blob/master/zw-mode.el?raw=true")
; add functions temporarily install
(eval-buffer)
; set path to your zim wiki
(setq zim-root "/path/to/your/zimwiki")
; start it up
(zw-goto-now)
```

### use-package 
```
curl  "https://github.com/WillForan/zw-mode/blob/master/zw-mode.el?raw=true" > ~/path/to/zw-mode/zw-mode.el
```

`~/.emacs` might look like

```elisp
(use-package zw-mode
  :load-path "~/path/to/zw-mode"
  :bind ("C-c n" . zw-goto-now)
  :init 
    (setq zim-root "~/path/to/wiki")
    (setq zw-jounal-datestr "Calendar/%Y/Week_%02V.txt") ; see: C-h f format-time-string
)
```


## Setup

 * You should already have a notebook established with zim-wiki. 
 * This mode was developed with both the journal and version control plugins enabled.
 * At the very least, you will need to customize the root path variable to match the location of the zim wiki. If not using `use-package`, this can be set interactively like `M-x customize-group RET zw-mode RET`

## Keys
Default keys. Rearranged and annotated output of `C-c ?`

```
; go places
C-c n		zw-goto-now                 jump to now page
C-c f		zw-helm-projectile          go to page by title search
C-c C-f		zw-search                   go to page by content search
C-c RET		zw-ffap                     go to link cursor is over
C-c M-RET	zw-ffap-below               open link in new window below current

; insert links
C-c l		zw-insert-helm-projectile   insert link by title searching
C-c C-l		zw-insert-search            insert link by filename/title search
C-c N		zw-insert-now-link          link now page on current page
C-c w		zw-link-wrap                wrap e.g a:b:c into [[a:b:c]]

; link based on history
C-c C-p		zw-insert-prev-buffer-link  insert link on current page to previous buffer
C-c y		zw-buffer-path-to-kill-ring copy current buffer file name
C-c p		zw-insert-kill-ring-as-link paste filename as link

; date operations
C-c n		zw-goto-now                 jump to now page
C-c N		zw-insert-now-link          link now page on current page
C-c C-n		zw-insert-current-at-now    put current page link on now page, go to now page

; neotree
C-c T		neotree-find
C-c t		neotree-toggle
```
