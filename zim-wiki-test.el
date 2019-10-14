
;; tests for zimwwiki-mode
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
