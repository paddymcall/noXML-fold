;; test noxml-fold-mode

;;; run from command line as `emacs --no-init-file --no-site-file -batch -l noxml-fold.el -l tests/tests.el -f ert-run-tests-batch-and-exit'

(require 'xmltok)
(require 'nxml-mode)

(ert-deftest noxml-test-fold-item ()
  (let ((xml-doc "<div><p><hi>hello</hi> to you!</p></div>")
	(cases
	 '((inline ((1 . 6) 35 . 41)
		   #("<div><p><hi>hello</hi> to you!</p></div>" 0 1 (fontified t face (nxml-tag-delimiter)) 1 2 (fontified t face (nxml-element-local-name)) 2 3 (fontified t face (nxml-element-local-name)) 3 4 (fontified t face (nxml-element-local-name)) 4 5 (fontified t face (nxml-tag-delimiter)) 5 6 (fontified t face (nxml-tag-delimiter)) 6 7 (fontified t face (nxml-element-local-name)) 7 8 (fontified t face (nxml-tag-delimiter)) 8 9 (fontified t face (nxml-tag-delimiter)) 9 10 (fontified t face (nxml-element-local-name)) 10 11 (fontified t face (nxml-element-local-name)) 11 12 (fontified t face (nxml-tag-delimiter)) 12 13 (fontified t face (nxml-text)) 13 14 (fontified t face (nxml-text)) 14 15 (fontified t face (nxml-text)) 15 16 (fontified t face (nxml-text)) 16 17 (fontified t face (nxml-text)) 17 18 (fontified t face (nxml-tag-delimiter)) 18 19 (fontified t face (nxml-tag-slash)) 19 20 (fontified t face (nxml-element-local-name)) 20 21 (fontified t face (nxml-element-local-name)) 21 22 (fontified t face (nxml-tag-delimiter)) 22 23 (fontified t face (nxml-text)) 23 24 (fontified t face (nxml-text)) 24 25 (fontified t face (nxml-text)) 25 26 (fontified t face (nxml-text)) 26 27 (fontified t face (nxml-text)) 27 28 (fontified t face (nxml-text)) 28 29 (fontified t face (nxml-text)) 29 30 (fontified t face (nxml-text)) 30 31 (fontified t face (nxml-tag-delimiter)) 31 32 (fontified t face (nxml-tag-slash)) 32 33 (fontified t face (nxml-element-local-name)) 33 34 (fontified t face (nxml-tag-delimiter)) 34 35 (fontified t face (nxml-tag-delimiter)) 35 36 (fontified t face (nxml-tag-slash)) 36 39 (fontified t face (nxml-element-local-name)) 39 40 (fontified t face (nxml-tag-delimiter)))
		   ))))
    (dolist (case cases)
      ;; these calls are necessary to make fontification work
      ;; properly; no success with simple temp buffers
      (with-current-buffer (get-buffer-create "*tmp noxml-tests tmp*")
	(switch-to-buffer (current-buffer))
	(erase-buffer)
	(insert xml-doc)
	(goto-char (point-min))
	(fundamental-mode)
	(nxml-mode)
	(should
	 (equal
	  (progn
	    (apply 'noxml-fold-item (butlast case))
	    (buffer-substring (point-min) (point-max)))
	  (car (last case))))))))

;; (ert 'noxml-test-fold-item)

(ert-deftest noxml-test-overlay-prioritize ()
  (let ((xml-doc "<div><p><hi>hello</hi> to you!</p></div>")
	(cases '((1 41 inline 0))))
    (dolist (case cases)
      (with-temp-buffer
	(insert xml-doc)
	(nxml-mode)
	(should
	 (equal
	  (apply 'noxml-overlay-prioritize (butlast case))
	  (car (last case))))))))

;; (ert 'noxml-test-overlay-prioritize)

