# zim-wiki-mode
An elisp package for editing [zim-wiki](http://zim-wiki.org) in emacs.

This package primarily provides convenient page linking and journaling by extending `dokuwiki-mode` and wrapping functions around `helm-projectile`, `helm-ag`, and `link-hint`. A menu is provided through `pretty-hydra`. A very basic and slow completion method (`zim-wiki-mode-complete`) uses `company-mode`.

## Motivation
Zim's plain text markup is open to any editor. But only Zim Desktop Wiki itself is any good at actually editing pages. `zim-wiki-mode` intends to improve the editing expereence in Emacs.


## use-package 

zim-wiki-mode is a [recipe](https://github.com/melpa/melpa/blob/master/recipes/zim-wiki-mode) in [melpa](https://melpa.org/)! 

If you want bleading edge, grab from git. `~/.emacs.d/init.el` might include

```elisp
;; setup wiki mode
(use-package zim-wiki-mode
  :quelpa ((dokuwiki :fetcher github :repo "WillForan/zim-wiki-mode") :upgrade t)
  :bind ("C-c C-n" . zim-wiki-goto-now)
  :init
    (add-hook 'zim-wiki-mode-hook 'flyspell-mode)
  :config
    (setq zim-wiki-always-root "~/notes/PersonalWiki") ; if not set, would use projectile directory
    (setq zim-wiki-journal-datestr "Calendar/%Y/%02m.txt")

    (zim-wiki-refresh-completion) ; SLOW. get list for company-cap

    (evil-leader/set-key-for-mode 'zim-wiki-mode "z" 'zim-wiki-hydra/body))


;; modified dokuwiki-mode with outline-magic symmetric headers
(use-package dokuwiki-mode
  :quelpa ((dokuwiki :fetcher github :repo "WillForan/dokuwiki-mode") :upgrade t)
  :ensure t
  :config
   (require outline-magic)
   (flyspell-mode 1))
```

## Setup

 * You should already have a notebook established with zim-wiki, referenced using `(setq zim-wiki-always-root ...` above
 * This mode was developed with both the journal and **version control** plugins enabled.
   * VC (`.git` directory) is necessary for `projectile` related commands.
 * If not a la `use-package` above, interactively set the location to your already existing zim notebook  `M-x customize-group RET zim-wiki RET`

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
