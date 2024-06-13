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

(defmacro zim-wiki-ffap-file-test (point)
  "Setup link text in temp buffer.  Goto POINT.  Check zim-wiki-ffap-file."
  `(ert-deftest ,(intern (format "zim-wiki-path-%s" point)) ()
       (let ((zim-wiki-always-root "/r"))
         (with-temp-buffer
           ;;           5   9 11  15
           ;;           |   | |   |
           (insert "xzy [[:a:b| [link] text]] foobar ")
           (goto-char ,point)
           (should (string= "/r/a/b.txt" (zim-wiki-ffap-file)))))))

(dolist (point '(0 5 9 11 15 100)) (eval `(zim-wiki-ffap-file-test ,point)))


(ert-deftest zim-wiki-ffap-file-test-spaces ()
  "Spaces in wiki path."
  (let ((zim-wiki-always-root "/r"))
    (with-temp-buffer
      (insert "xzy [[:a:b c d| [link] text]] foobar") ;
      (goto-char (point-min))
      (should (string= "/r/a/b_c_d.txt" (zim-wiki-ffap-file))))))
