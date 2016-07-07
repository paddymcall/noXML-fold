;; test noxml-fold-mode

;;; run from command line as `emacs --no-init-file --no-site-file -batch -l noxml-fold.el -l tests/tests.el -f ert-run-tests-batch-and-exit'

(require 'xmltok)
(require 'nxml-mode)

(defun noxml-tests-ensure-fontified ()
  "This makes sure that the current buffer is fully fontified.

Found this here:
http://stackoverflow.com/questions/2504418/emacs-lisp-buffer-not-running-font-lock-mode-until-opened-by-user
---> `ps-print-ensure-fontified'. But better: `hfy-force-fontification'.
"
  (let ((start (point-min))
	(end (point-max)))
    (eval-and-compile (require 'font-lock))
    (if (boundp 'font-lock-cache-position)
      (or font-lock-cache-position
          (set 'font-lock-cache-position (make-marker))))
    (if (and (boundp 'jit-lock-mode) (symbol-value 'jit-lock-mode))
	(jit-lock-fontify-now start end))
    (if (and (boundp 'lazy-lock-mode) (symbol-value 'lazy-lock-mode))
	(apply 'lazy-lock-fontify-region (list start end)))
    (when font-lock-defaults
      (font-lock-fontify-buffer))))

(ert-deftest noxml-test-fold-item ()
  (let ((xml-doc "<div><p><hi>hello</hi> to you!</p></div>")
	(noxml-fold-spec-list nil)
	(cases
	 '(((inline ((1 . 6) 35 . 41))
	    (help-echo
	     #("<div><p><hi>hello</hi> to you!</p></div>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 4
	       (face
		(nxml-element-local-name))
	       4 5
	       (face
		(nxml-tag-delimiter))
	       5 6
	       (face
		(nxml-tag-delimiter))
	       6 7
	       (face
		(nxml-element-local-name))
	       7 8
	       (face
		(nxml-tag-delimiter))
	       8 9
	       (face
		(nxml-tag-delimiter))
	       9 11
	       (face
		(nxml-element-local-name))
	       11 12
	       (face
		(nxml-tag-delimiter))
	       12 17
	       (face
		(nxml-text))
	       17 18
	       (face
		(nxml-tag-delimiter))
	       18 19
	       (face
		(nxml-tag-slash))
	       19 21
	       (face
		(nxml-element-local-name))
	       21 22
	       (face
		(nxml-tag-delimiter))
	       22 30
	       (face
		(nxml-text))
	       30 31
	       (face
		(nxml-tag-delimiter))
	       31 32
	       (face
		(nxml-tag-slash))
	       32 33
	       (face
		(nxml-element-local-name))
	       33 34
	       (face
		(nxml-tag-delimiter))
	       34 35
	       (face
		(nxml-tag-delimiter))
	       35 36
	       (face
		(nxml-tag-slash))
	       36 39
	       (face
		(nxml-element-local-name))
	       39 40
	       (face
		(nxml-tag-delimiter)))
	     display "[div]" mouse-face highlight noxml-fold-display-string-spec "[div]" noxml-fold-type inline evaporate t priority 0 category noxml-fold))
	   ((inline ((9 . 13) 18 . 23))
	    (help-echo
	     #("<hi>hello</hi>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 3
	       (face
		(nxml-element-local-name))
	       3 4
	       (face
		(nxml-tag-delimiter))
	       4 9
	       (face
		(nxml-text))
	       9 10
	       (face
		(nxml-tag-delimiter))
	       10 11
	       (face
		(nxml-tag-slash))
	       11 13
	       (face
		(nxml-element-local-name))
	       13 14
	       (face
		(nxml-tag-delimiter)))
	     display "[hi]" mouse-face highlight noxml-fold-display-string-spec "[hi]" noxml-fold-type inline evaporate t priority -16 category noxml-fold))
	   ((inline ((6 . 9) 31 . 35))
	    (help-echo
	     #("<p><hi>hello</hi> to you!</p>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 2
	       (face
		(nxml-element-local-name))
	       2 3
	       (face
		(nxml-tag-delimiter))
	       3 4
	       (face
		(nxml-tag-delimiter))
	       4 6
	       (face
		(nxml-element-local-name))
	       6 7
	       (face
		(nxml-tag-delimiter))
	       7 12
	       (face
		(nxml-text))
	       12 13
	       (face
		(nxml-tag-delimiter))
	       13 14
	       (face
		(nxml-tag-slash))
	       14 16
	       (face
		(nxml-element-local-name))
	       16 17
	       (face
		(nxml-tag-delimiter))
	       17 25
	       (face
		(nxml-text))
	       25 26
	       (face
		(nxml-tag-delimiter))
	       26 27
	       (face
		(nxml-tag-slash))
	       27 28
	       (face
		(nxml-element-local-name))
	       28 29
	       (face
		(nxml-tag-delimiter)))
	     display "[p]" mouse-face highlight noxml-fold-display-string-spec "[p]" noxml-fold-type inline evaporate t priority 0 category noxml-fold))
	   ;; the same for block elements
	   ((block ((1 . 6) 35 . 41))
	    (help-echo
	     #("</div>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 2
	       (face
		(nxml-tag-slash))
	       2 5
	       (face
		(nxml-element-local-name))
	       5 6
	       (face
		(nxml-tag-delimiter)))
	     display "[div]" mouse-face highlight noxml-fold-display-string-spec "[div]" noxml-fold-type block evaporate t priority 0 category noxml-fold)
	    (help-echo
	     #("<div>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 4
	       (face
		(nxml-element-local-name))
	       4 5
	       (face
		(nxml-tag-delimiter)))
	     display "[div]" mouse-face highlight noxml-fold-display-string-spec "[div]" noxml-fold-type block evaporate t priority 0 category noxml-fold))
	   ((block ((9 . 13) 18 . 23))
	    (help-echo
	     #("</hi>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 2
	       (face
		(nxml-tag-slash))
	       2 4
	       (face
		(nxml-element-local-name))
	       4 5
	       (face
		(nxml-tag-delimiter)))
	     display "[hi]" mouse-face highlight noxml-fold-display-string-spec "[hi]" noxml-fold-type block evaporate t priority -32 category noxml-fold)
	    (help-echo
	     #("<hi>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 3
	       (face
		(nxml-element-local-name))
	       3 4
	       (face
		(nxml-tag-delimiter)))
	     display "[hi]" mouse-face highlight noxml-fold-display-string-spec "[hi]" noxml-fold-type block evaporate t priority -16 category noxml-fold))
	   ((block ((6 . 9) 31 . 35))
	    (help-echo
	     #("</p>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 2
	       (face
		(nxml-tag-slash))
	       2 3
	       (face
		(nxml-element-local-name))
	       3 4
	       (face
		(nxml-tag-delimiter)))
	     display "[p]" mouse-face highlight noxml-fold-display-string-spec "[p]" noxml-fold-type block evaporate t priority -16 category noxml-fold)
	    (help-echo
	     #("<p>" 0 1
	       (face
		(nxml-tag-delimiter))
	       1 2
	       (face
		(nxml-element-local-name))
	       2 3
	       (face
		(nxml-tag-delimiter)))
	     display "[p]" mouse-face highlight noxml-fold-display-string-spec "[p]" noxml-fold-type block evaporate t priority 0 category noxml-fold)))))
    (dolist (case cases)
      ;; these calls are necessary to make fontification work
      ;; properly; no success with simple temp buffers
      (with-temp-buffer
	(insert xml-doc)
	(goto-char (point-min))
	(nxml-mode)
	(noxml-tests-ensure-fontified)
	(noxml-fold-mode)
	(apply 'noxml-fold-item (car case))
	;; to see what's going on:
	;; (message "%s" (text-properties-at (point)))
	;; (mapc (lambda (ov)
	;; 	(pp "%s" ov)
	;; 	(pp "%s" (overlay-properties ov)))
	;;       (overlays-in (point-min) (point-max)))
	;; (message "Rep: %s/%s" (1+ (- (length cases) (length (member case cases)))) (length cases))
	;; check the text as string
	(should
	 (equal
	  (buffer-substring (point-min) (point-max))
	  ;; (buffer-substring-no-properties (point-min) (point-max))
	  xml-doc))
	(should
	 (equal
	  ;; reverses expectations!
	  (mapcar (lambda (ov)
		    ;; copy all properties into the result field for new tests)
		    ;; (message "Present:")
		    ;; (pp (overlay-properties ov))
		    ;; (message "Target:")
		    ;; (pp (last case))
		    (overlay-properties ov))
		  (overlays-in (point-min) (point-max)))
	  (cdr case)))))))

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

(ert-deftest noxml-test-flatten-spec-list ()
  (should
   (equal
    (noxml-fold-flatten-spec-list
     '(("⚓"
	("anchor"))
       ("⚡"
	("pb"))
       ("ₗ"
	("lb"))
       ("⚐"
	("note"))
       ("ₓ"
	("gap"))
       ("➶"
	("ref" "ptr"))
       ("noxml-render-direct-children" nil)
       (noxml-get-content
	("label" "hi" "q" "corr" "subst" "persName" "span" "lem" "rdg" "emph" "del" "unclear" "w" "add"))
       (noxml-render-first-child
	("app"))))
    '(("app" . noxml-render-first-child)
     ("add" . noxml-get-content)
     ("w" . noxml-get-content)
     ("unclear" . noxml-get-content)
     ("del" . noxml-get-content)
     ("emph" . noxml-get-content)
     ("rdg" . noxml-get-content)
     ("lem" . noxml-get-content)
     ("span" . noxml-get-content)
     ("persName" . noxml-get-content)
     ("subst" . noxml-get-content)
     ("corr" . noxml-get-content)
     ("q" . noxml-get-content)
     ("hi" . noxml-get-content)
     ("label" . noxml-get-content)
     ("ptr" . "➶")
     ("ref" . "➶")
     ("gap" . "ₓ")
     ("note" . "⚐")
     ("lb" . "ₗ")
     ("pb" . "⚡")
     ("anchor" . "⚓")))))

;; (ert 'noxml-test-flatten-spec-list)

(ert-deftest noxml-test-find-default-namespace ()
  (let ((cases '(("<div>hello</div>" . nil)
		 ("<div xmlns=\"http://www.tei-c.org/ns/1.0\" ><p><hi>hello</hi> to you!</p></div>" . "http://www.tei-c.org/ns/1.0"))))
    (dolist (case cases)
      (with-temp-buffer
	(insert (car case))
	(should
	 (equal
	  (noxml-find-default-namespace)
	  (cdr case)))))))
