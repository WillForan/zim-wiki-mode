;; tests for zim-wiki-mode
(ert-deftest zim-wiki-test-mklink-full () (should (string= (zim-wiki-mklink "foo:bar" "baz") "[[foo:bar|baz]]")))
(ert-deftest zim-wiki-test-mklink-path () (should (string= (zim-wiki-mklink "foo:bar") "[[foo:bar]]")))

(ert-deftest zim-wiki-test-path2wiki ()
  (let ((zim-wiki-always-root "/a/b"))
    (should (string= (zim-wiki-path2wiki "./a/b") "+a:b"))
    (should (string= (zim-wiki-path2wiki "/a/b/c/d") ":c:d"))
    (should (string= (zim-wiki-path2wiki "/a/b/e/f.txt") ":e:f"))))

(ert-deftest zim-wiki-test-path2wiki-tilda ()
  (let ((zim-wiki-always-root "~/a/b"))
    (should (string= (zim-wiki-path2wiki (expand-file-name "~/a/b/c/d")) ":c:d"))
    (should (string= (zim-wiki-path2wiki "~/a/b/y/z") ":y:z"))))

(ert-deftest zim-wiki-ffap-file-test ()
  (let ((zim-wiki-always-root "/r"))
    (with-temp-buffer
      ;;           5   9 11  15
      ;;           |   | |   |
      (insert "xzy [[:a:b| [link] text]] foobar ")

      ;;BUG: [[a:b| [problem] ]]  ; will throw errors
      (goto-char (point-min))
      (should (string= "/r/a/b.txt" (zim-wiki-ffap-file)))
      (goto-char 5)
      (should (string= "/r/a/b.txt" (zim-wiki-ffap-file)))
      (goto-char 9)
      (should (string= "/r/a/b.txt" (zim-wiki-ffap-file)))
      (goto-char 11)
      (should (string= "/r/a/b.txt" (zim-wiki-ffap-file)))
      (goto-char 15)
      (should (string= "/r/a/b.txt" (zim-wiki-ffap-file)))
      (goto-char (point-max))
      (should (string= "/r/a/b.txt" (zim-wiki-ffap-file))))))
