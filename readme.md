# zim-wiki-mode
An elisp package for editing [zim-wiki](http://zim-wiki.org) in emacs.

This package primarily provides convenient page linking and journaling by extending `dokuwiki-mode` and wrapping functions around `helm-projectile`, `helm-ag`, and `link-hint`. A menu is provided through `pretty-hydra`. A very basic and slow-to-initialize completion method (`zim-wiki-mode-complete`) uses `company-mode`. Zim Desktop Wiki's sqlite db can be accessed for tag completion (`company-capf`) and backlink lookup (`heml`).

## Motivation
Zim's plain text markup is open to any editor. But only [Zim Desktop Wiki](https://zim-wiki.org/) itself is any good at actually editing pages. `zim-wiki-mode` intends to improve the editing experience in Emacs.

`zim-wiki-mode` also make editing on Android a bit more accessible, however it's a kludge. See [syncthing](https://syncthing.net/) (or owncloud, gdrive, dropbox, etc), [termux](https://termux.dev/en/), and `pkg install emacs`

## Demo
using `zim-wiki-mode` with the `zerodark` theme and [M+ 1M font](https://github.com/coz-m/MPLUS_FONTS). 

![demo gif](demo.gif?raw=true)

 * link insert with <kbd>C-c M-l</kbd>
 * link auto-complete after `(zim-wiki-refresh-completions)`
 * link insert from search with helm-ag <kbd>C-c C-l</kbd>
 * insert new header after `(require 'outline-magic)` <kbd>M-RET</kbd> and <kbd> M-S-left/right</kbd>
 * autocomplete tags. req `(zim-wiki-list-tags-refresh)`, stored in `zim-wiki-all-tags`
 * insert link to today <kbd>C-c C-n</kbd>
 * follow link hint <kbd>C-c l</kbd>
 * hydra back links <kbd>C-c C-z <</kbd>


## use-package

zim-wiki-mode is a [recipe](https://github.com/melpa/melpa/blob/master/recipes/zim-wiki-mode) in [melpa](https://melpa.org/)!

If you want bleeding edge, grab from git. `~/.emacs.d/init.el` might include

```elisp
;; setup wiki mode
(use-package zim-wiki-mode
  :quelpa ((dokuwiki :fetcher github :repo "WillForan/zim-wiki-mode") :upgrade t)
  :bind
    ("C-c z" . zim-wiki-goto-now)
    (:map zim-wiki-mode-map
            ("M-p" . #'outline-previous-visible-heading)
            ("M-n" . #'outline-next-visible-heading)
            ("M-S-<return>" . #'org-insert-item))
  :init
    (add-hook 'zim-wiki-mode-hook 'flyspell-mode)
  :config
    (setq zim-wiki-always-root "~/notes/PersonalWiki") ; if not set, would use projectile directory
    (setq zim-wiki-journal-datestr "Calendar/%Y/%02m.txt")

    (zim-wiki-refresh-completion) ; SLOW. get list for company-cap
    (zim-wiki-list-tags-refresh)  ; get tags from sqlite3 db

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

