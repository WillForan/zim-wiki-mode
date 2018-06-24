# zw-mode
elisp package for editing [zim-wiki](http://zim-wiki.org) in emacs.

## Demo
[video](demo.ogv?raw=true) (ogg video)

  0. <kbd>C-c n</kbd> go to the "now" page
     * page string defined by `zw-journal-datestr`
  1. <kbd>C-c l</kbd> create a new page in hierarchy by searching current tree 
     * searching "zim emacs" and change the result to "Example"
     * via `helm-projectile`
  2. <kbd>C-RET</kbd> follow the link we just created
  3. <kbd>C-c N</kbd> insert a link to the current date 
  4. write up some text about what we did
  5. <kbd>C-c C-l</kbd> link in that page that has "emacs cider" content but whos name is not memberible
     * helm provides <kbd>C-z</kbd> to preview
    <kbd>C-c C-l</kbd> emacs cider 
  6. <kbd>C-c f</kbd> go to title: we want to add things to that just linked in page.
     * could <kbd>C-c RET</kbd> on the link we created, but lets go there with by search file names (page titles)
     * or go by search text again (<kbd>C-c C-f</kbd>)
  7. <kbd>C-c C-p</kbd> link prev page: while we are there lets add where we came from
  8. <kbd>C-c C-n</kbd> then link the page we are currently editing to the now page
  9. <kbd>C-c w</kbd> paths freehand and wrap them in a link

N.B.
  * when following a link, we need to be on the word (not the `[[` or `]]` part)
  * no autosave

