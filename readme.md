# zim-wiki-mode
An elisp package for editing [zim-wiki](http://zim-wiki.org) in emacs.

This package primarily provides convenient page linking and journaling by extending `dokuwiki-mode` and wrapping functions around `helm-projectile`, `helm-ag`, and `link-hint`. A hydra menu is provided with `pretty-hydra`.

### Motivation
Even though any editor can edit zim's plain text files, none are particularly efficient. Hopefully, `zim-wiki-mode` reduces that friction in emacs.

## Demo
using zim-wiki-mode with evil-mode and leuven theme

![demo gif](demo.gif?raw=true) 

  0. <kbd>C-c C-n</kbd> go to the "now" page
     * now page date format defined by `zim-wiki-journal-datestr`
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
2. put [`zim-wiki-mode.el`](zim-wiki-mode.el?raw=true) within your load-path and load it.
3. customize `zim-wiki-root` to your zim wiki notebook location.
4. start wiki-ing with `M-x zim-wiki-goto-now`

### use-package 
```
curl  "https://github.com/WillForan/zim-wiki-mode/blob/master/zim-wiki-mode.el?raw=true" > ~/path/to/zim-wiki-mode.el
```

`~/.emacs` might look like

```elisp
;; pull packges from repos not yet in e.g. melpa
(use-package quelpa :ensure t 
  :config
    (quelpa '(zim-wiki-mode :fetcher github :repo "WillForan/zim-wiki-mode"))
)
;; setup wiki mode
(use-package zim-wiki-mode
  :bind ("C-c C-n" . zim-wiki-goto-now)
  :init
    (add-hook 'zim-wiki-mode-hook 'flyspell-mode)
  :config
    (setq zim-wiki-root "~/notes/PersonalWiki")
    (setq zim-wiki-journal-datestr "Calendar/%Y/%02m.txt")
    (evil-leader/set-key-for-mode 'zim-wiki-mode "z" 'zim-wiki-hydra/body)
)
```


### Try it from *scratch*
copy and evalute (`M-x eval-buffer`) the following lines
```elisp
;; install dependencies
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)
(package-install 'ffap)
(package-install 'dokuwiki-mode)
(package-install 'helm-ag)
(package-install 'helm-projectile) 
(package-install 'flyspell-mode) 
(package-install 'link-hint) 
(package-install 'pretty-hydra) 

;; get -- maybe read over, be sure it's not going to add you to a botnet
(browse-url-emacs "https://github.com/WillForan/zim-wiki-mode/blob/master/zim-wiki-mode.el?raw=true")
;; add functions temporarily install
(eval-buffer)
;; set path to your zim wiki
(setq zim-wiki-root "/path/to/your/zim-wiki")
;; start it up
(zim-wiki-goto-now)
```


## Setup

 * You should already have a notebook established with zim-wiki. 
 * This mode was developed with both the journal and version control plugins enabled.
 * At the very least, you will need to customize the root path variable to match the location of the zim wiki. If not using `use-package`, this can be set interactively like `M-x customize-group RET zim-wiki-mode RET`

## Keys
Default keys. Rearranged and annotated output of `C-c ?`

```
; menu
C-c C-z      zim-wiki-hydra/body              see all the options

; go places
C-c C-n		zim-wiki-goto-now                 jump to now page
C-c M-f		zim-wiki-helm-projectile          go to page by title search
C-c C-f		zim-wiki-search                   go to page by content search
C-c RET		zim-wiki-ffap                     go to link cursor is over
C-c M-RET	zim-wiki-ffap-below               open link in new window below current

; insert links
C-c M-l		zim-wiki-insert-helm-projectile   insert link by title searching
C-c C-l		zim-wiki-insert-search            insert link by filename/title search
C-c C-N		zim-wiki-insert-now-link          link now page on current page
C-c M-w		zim-wiki-link-wrap                wrap e.g a:b:c into [[a:b:c]]

; link based on history
C-c C-p		zim-wiki-insert-prev-buffer-link  insert link on current page to previous buffer
C-c M-y		zim-wiki-buffer-path-to-kill-ring copy current buffer file name
C-c M-p		zim-wiki-insert-kill-ring-as-link paste filename as link

; date operations
C-c C-n		zim-wiki-goto-now                 jump to now page
C-c C-N		zim-wiki-insert-now-link          link now page on current page
C-c M-n		zim-wiki-insert-current-at-now    put current page link on now page, go to now page
```
