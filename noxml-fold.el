;;; noxml-fold.el --- Fold away XML things.

;; Copyright (C) 2014--2015 Patrick McAllister

;; Author: Patrick McAllister <pma@rdorte.org>
;; Keywords: xml, folding
;; URL: https://github.com/paddymcall/noXML-fold

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Emacs minor mode that tries to enable useful folding for
;; XML files, copying a lot from AUCTeX's tex-fold.el. It presupposes
;; that nxml-mode is the major-mode.

;; The most useful entry points for users are `noXML-fold-dwim', and
;; `noXML-fold-region'.

;; Since this mode uses overlays, it does *not* scale: for very
;; long/deeply nested XML, you should only fold what's within view, or
;; make use of `narrow-to-region'.

(require 'overlay)
(require 'nxml-mode)

;;; configuration, vars etc.

(defcustom noXML-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean
  :group 'noXML-fold)

(defcustom noXML-fold-help-echo-max-length 70
  "Maximum length of help echo message for folded overlays.
Set it to zero in order to disable help echos."
  :type 'integer
  :group 'noXML-fold)

(defvar noXML-fold-open-spots nil)
(make-variable-buffer-local 'noXML-fold-open-spots)

(defcustom noXML-is-not-inline-regexp "^\\s-*<[^/]"
  "A regular expression use to determine whether an element is inline or not.

If this matches, it's considered a block element. See `noXML-is-inline'.

TODO: bad solution this here. See info: (nxml-mode) Paragraphs for a better way to do this (and integrate).
See also nxml-mode.el::;;; Paragraphs."
  :type 'regexp
  :group 'noXML-fold)

(defcustom noXML-inline-elements nil
  "A list of element names that are always considered inline."
  :type '(repeat string)
  :group 'noXML-fold)

(defcustom noXML-block-elements nil
  "A list of element names that are always considered block elements."
  :type '(repeat string)
  :group 'noXML-fold)

(defcustom noXML-toggle-input-method t
  "If true, then I'll change the input-method after you type > or <. Not yet implemented, though."
  :type 'boolean 
  :group 'noXML-fold)

(defvar noXML-overlay-priority-step 16
  "Numerical difference of priorities between nested overlays.
The step should be big enough to allow setting a priority for new
overlays between two existing ones.")

(defcustom noXML-fold-force-fontify t
  "Force the buffer to be fully fontified by folding it."
  :group 'noXML-fold
  :type 'boolean)

(defcustom noXML-fold-add-to-screen 5000
  "How many chars to search above and below the screen edge when
trying to fold things.

If you have a <paragraph>...</paragraph>, for example, that stretches over
10.000 chars, and your screen shows around 4000 chars, and you
are at the beginning of the <paragraph>, then it will fold the
whole thing for you only if `noXML-fold-add-to-screen' is >
6000."
  :group 'noXML-fold
  :type 'integer)

(make-variable-buffer-local 'noXML-fold-add-to-screen)

(defface noXML-fold-folded-face
  '((((class color) (background light))
     (:foreground "SlateBlue"))
    (((class color) (background dark))
     (:foreground "SlateBlue1"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for the display string of folded content."
  :group 'noXML-fold)

(defvar noXML-fold-folded-face 'noXML-fold-folded-face
  "Face for the display string of folded content.")

(defface noXML-fold-unfolded-face
  '((((class color) (background light))
     (:background "#f2f0fd"))
    (((class color) (background dark))
     (:background "#38405d"))
    (((class grayscale) (background light))
     (:background "LightGray"))
    (((class grayscale) (background dark))
     (:background "DimGray"))
    (t (:inverse-video t)))
  "Face for folded content when it is temporarily opened."
  :group 'noXML-fold)

(defvar noXML-fold-unfolded-face 'noXML-fold-unfolded-face
  "Face for folded content when it is temporarily opened.")


(defvar noXML-fold-ellipsis "..."
  "String used as display string for overlays instead of a zero-length string.")

(defcustom noXML-fold-command-prefix "\C-c\C-o\C-f"
  "Prefix key to use for commands in noXML-fold mode.
The value of this variable is checked as part of loading noXML-fold mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'noXML-fold)

(defcustom noXML-fold-auto nil
  "If non-nil, fold macros automatically. Leave this at `nil' for the time being!!"
  :group 'noXML-fold
  :type 'boolean)

(defvar noXML-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" 'noXML-fold-dwim)
    (define-key map "\C-b" 'noXML-fold-buffer)
    (define-key map "\C-r" 'noXML-fold-region)
    (define-key map "\C-m" 'noXML-fold-macro)
    (define-key map "\C-e" 'noXML-fold-env)
    (define-key map "b"    'noXML-fold-clearout-buffer)
    (define-key map "r"    'noXML-fold-clearout-region)
    (define-key map "i"    'noXML-fold-clearout-item)
    map))

(defgroup noXML-fold nil
  "Fold xml elements."
  :group 'languages)

(defcustom noXML-fold-unspec-block-display-string "[bl]"
  "Display string for unspecified environments.
This string will be displayed if a single environment is being
hidden which is not specified in `noXML-fold-env-spec-list'."
  :type '(string)
  :group 'noXML-fold)

(defcustom noXML-fold-unspec-inline-display-string "[inl]"
  "Display string for unspecified macros.
This string will be displayed if a single macro is being hidden
which is not specified in `noXML-fold-macro-spec-list'."
  :type '(string)
  :group 'noXML-fold)

(defcustom noXML-fold-unspec-use-name t
  "If non-nil use the name of an unspecified item as display string.
Set it to nil if you want to use the values of the variables
`noXML-fold-unspec-block-display-string' or
`noXML-fold-unspec-inline-display-string' respectively as a display
string for any unspecified macro or environment."
  :type 'boolean
  :group 'noXML-fold)

(defcustom noXML-fold-spec-list nil
  "List of replacement specifiers and elements to fold.

The first element of each item can be a string, an integer or a
function symbol.  The second element is a list of elements to fold.

If the first element is a string, it will be used as a display
replacement for the whole element.

If the first element is a function symbol, the function will be
called with all mandatory arguments of the macro and the result
of the function call will be used as a replacement for the macro.

Setting this variable does not take effect immediately.  Use
Customize or reset the mode.

An example I use for TEI XML:

`((\"⚓\" (\"anchor\"));; some string specifiers
 (\"⚡\" (\"pb\"))
 (\"ₗ\" (\"lb\"))
 (\"⚐\" (\"note\"))
 (\"ₓ\" (\"gap\"))
 (\"➶\" (\"ref\" \"ptr\"))
 (noXML-render-direct-children nil);; okay, I don't use this much
 (noXML-get-content
  (\"label\" \"hi\" \"q\" \"corr\" \"persName\" \"span\" \"lem\" \"rdg\" \"emph\" \"del\" \"unclear\" \"w\" \"add\"));; don't hide the content here
 (noXML-render-first-child (\"app\")))';; for the app elements, show whatever is the first sub-element
"
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1)
				(function :tag "Function to execute"))
			(repeat :tag "Element" (string))))
  :group 'noXML-fold)

(defvar noXML-fold-spec-list-internal nil
  "Internal list of display strings and macros to fold.
Is updated when the noXML-Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, i.e. contents of both `noXML-fold-spec-list'
and <mode-prefix>-fold-macro-spec-list.")
;; (make-variable-buffer-local 'noXML-fold-spec-list-internal);; not a good idea: do this in mode definition below (see http://www.emacswiki.org/emacs/BufferLocalVariable)

;;; utility functions

(defun noXML-fold-flatten-spec-list (specList)
  "Flattens the SPECLIST `noXML-fold-spec-list such that each element name can be found easily with assoc."
  (let (flatList)
    (dolist (set specList flatList)
      (unless (eq (nth 1 set) nil)
	(dolist (item (nth 1 set))
	  (setq flatList (cons (cons item (car set)) flatList)))))))

(defun noXML-element-attribute-get-value (name &optional prefix postfix)
  "Find the value of the last parsed element's attribute NAME."
    (let (
	  (atts (or xmltok-attributes xmltok-namespace-attributes))
	  result
	  (prefix (or prefix ""))
	  (postfix (or postfix ""))
	  )
      (while atts
	(let* ((attribute (car atts))
	       (name-start (xmltok-attribute-name-start attribute)))
	  (if (string= name (buffer-substring-no-properties (xmltok-attribute-name-start attribute) (xmltok-attribute-name-end attribute)))
	      (setq result (buffer-substring-no-properties (xmltok-attribute-value-start attribute) (xmltok-attribute-value-end attribute)))
	    t)
	  (if result (setq atts nil) (setq atts (cdr atts)))))
      (if result (concat prefix result postfix) nil)))

(defun noXML-find-element-start (position)
  "Returns starting position of the enclosing element.

If point is in data, comment, or closing tag, returns position of the opening tag.

If point is in empty element, returns start of that element.

See also: http://www.dpawson.co.uk/relaxng/nxml/nxmlGeneral.html (nxml-beginning/end-of-element)."
  (interactive "d")
  (save-excursion
    (let((nxml-sexp-element-flag nil))
      (progn
	 (goto-char position);; go where we want
	 (goto-char ;; go to the end of the element
	  (if (looking-at "<")
	      (nxml-token-after)
	    (nxml-token-before)))
	 (nxml-token-before)
	 (cond 
	  ((string-equal xmltok-type "empty-element") t)
	  ((memq xmltok-type '(data space)) (progn (nxml-scan-element-backward (point) t) xmltok-start))
	  ((string-equal xmltok-type "start-tag") t);; already evaluated above
	  ((string-equal xmltok-type "end-tag") (progn (nxml-backward-element)));;	  
	  (t t)
	  )
	 (if (called-interactively-p 'interactive)
	     (message "The element starts at char: %d" xmltok-start)
	   xmltok-start)))))

(defun noXML-find-element-end (position)
  "Returns the char position of the element found at POSITION."
  (interactive "d")
  (save-excursion
    (let (
	  (nxml-sexp-element-flag nil)
	  (start-element (noXML-find-element-start position))
	  end-of-end-tag
	  xmltok-start)
      (progn 
	(setq end-of-end-tag (nxml-scan-element-forward start-element))
	(if (called-interactively-p 'interactive)
	    (message "The element ends at char: %d" end-of-end-tag)
	  end-of-end-tag)))))


(defun noXML-fold-make-overlay (ov-start ov-end type display-string-spec priority)
  "Make a noXML-fold overlay extending from OV-START to OV-END.
TYPE is a symbol which is used to describe the content to hide
and may be 'inline for inline elements or 'block for block
elements.  DISPLAY-STRING-SPEC is the original specification of
the display string in the variable `noXML-fold-spec-list' and may
be a string or an integer. PRIORITY sets the priority of the
item.

TODO: review info:nxml-mode#Paragraphs for a different (better?) solution."
  ;; Calculate priority before the overlay is instantiated.  We don't
  ;; want `noXML-overlay-prioritize' to pick up a non-prioritized one.
  (let* (;;(priority (noXML-overlay-prioritize ov-start ov-end type))
	(ov (make-overlay ov-start ov-end (current-buffer) t nil)))
    (overlay-put ov 'category 'noXML-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'noXML-fold-type type)
    (overlay-put ov 'noXML-fold-display-string-spec display-string-spec)
    ov))

(defun noXML-overlay-prioritize (start end &optional type)
  "Calculate a priority for an overlay extending from START to END. The optional TYPE should be 'inline, otherwise we default to 'block.
The calculated priority is lower than the minimum of priorities
of surrounding overlays and higher than the maximum of enclosed
overlays."
  (save-excursion
    (let ((ov-depth 0)
	  (nxml-sexp-element-flag nil)
	  (factor noXML-overlay-priority-step)
	  (type (or type (if (progn (goto-char start) (nxml-token-after) (noXML-is-inline))
			     'inline
			   'block)
			     )))
      (if (string-equal type 'inline)
	  (while (and (< (point-min) (point));; see http://www.emacswiki.org/emacs/NxmlMode#toc11 for this test
		      (condition-case nil
			  (progn
			    (nxml-backward-up-element) ; always returns nil
			    t)
			(error nil))
		      (noXML-is-inline);; stop at block level element
		      )
	    (setq ov-depth (- ov-depth factor)))
	ov-depth))))


(defun noXML-make-overlay-invisible ()
  "Hides the element surrounding point."
  (interactive)
  (save-excursion
    (let* (;; let* creates args sequentially; earlier definitions available in later definitions
	   (element-start (noXML-find-element-start (point)))
	   (element-end (noXML-find-element-end (point)))
	   (text (filter-buffer-substring element-start element-end))
	   (element (concat (substring text 0 2) "../>"))
	   (noxmloverlay (make-overlay element-start element-end))
	   )
      ;; here, we're at the end of the element
      (overlay-put noxmloverlay 'display element)
      (overlay-put noxmloverlay 'intangible ())
      (overlay-put noxmloverlay 'category 'noXML-fold)
	)))


(defun noXML-render-first-child (position)
  "Render an element by picking out the content of its first element. 

Useful for stuff like <app><lem>, or <choice><sic>.
"
  (let* (
	 (nxml-sexp-element-flag nil)
	 (start (noXML-find-element-start position))
	 (end (noXML-find-element-end start))
	 (content "[no content found]"))
    (save-excursion
      (progn
	(goto-char start)
	(nxml-forward-balanced-item);; to the end of elem1
	(nxml-forward-balanced-item);; either at end of elem2's opening tag, or at the end of elem1
	(if (= (point) end);; if at the end
	    (setq content (noXML-fold-get-element-name start));; elem1's empty, use its name
	  (nxml-backward-up-element);; go to the beginning of elem2
	  )
	(if (>= (point) end);; if the element has no content
	    (setq content (noXML-fold-get-element-name start));; use the element name
	  (while (< (point) end);; start walking forward
	    (cond ((looking-at "<[/!]");; are we at the start of an end tag or comment?
		   (nxml-forward-balanced-item));; then skip ahead and
						;; see what happens
						;; (we won't leave the
						;; context, because
						;; the while clause is
						;; still in effect)
		  (
		   (looking-at "<[a-zA-Z0-9]+");; the first element; we'll use its content
		   (progn 
		     (setq content (noXML-get-content (point)));; that's all we wanted, so go to end
		     (goto-char end)))
		  (t (setq content "[no sub-element found]");; the default behaviour
		     ))
	    ))))
    content
    ))

(defun noXML-render-direct-children (position)
  "Render an element by picking out the content of all its direct children.

Useful for stuff like <lg><l/><l/> etc.
"
  (let* (
	(start (noXML-find-element-start position))
	(end (noXML-find-element-end start))
	(nxml-sexp-element-flag nil)
	(content "[no content found]")
	name-of-child
	)
    (save-excursion
      (progn
	(goto-char start)
	(nxml-forward-balanced-item);; to the end of elem1
	(nxml-forward-balanced-item);; either at end of elem2's opening tag, or at the end of elem1
	(if (= (point) end);; if at the end
	    (setq content (noXML-fold-get-element-name start));; elem1's empty, use its name
	  (nxml-backward-up-element);; go to the beginning of elem2
	  )
	(if (>= (point) end);; if the element has no content
	    (setq content (noXML-fold-get-element-name start));; use the element name
	  (while (< (point) end);; start walking forward
	    (cond ((looking-at "<[/!]");; are we at the start of an end tag or comment?
		   (nxml-forward-balanced-item));; then skip ahead and
						;; see what happens
						;; (we won't leave the
						;; context, because
						;; the while clause is
						;; still in effect)
		  ((looking-at "<[a-zA-Z0-9]+");; an element; let's have a closer look
		   (progn 
		     (if (not name-of-child);; if we haven't found a child yet
			 (progn (setq name-of-child (noXML-fold-get-element-name (point));; set its name
			   content (noXML-get-content (point)));; and use its content
				(nxml-forward-element);; and skip forward to start of next sibling
				(nxml-forward-balanced-item)
				(nxml-backward-up-element)
				)
		       (progn ;; if we've found a child already, check if the current element has the same name
			(if (string= name-of-child (noXML-fold-get-element-name (point)))
			    (progn ;; if its the same, append its value to content
			      (nxml-forward-balanced-item)
			      (setq content (concat content "\n" (filter-buffer-substring (point) (progn (nxml-forward-balanced-item) (point))))))
			  (nxml-forward-element);; not so interesting if it's not the same as the child
			   )
			))))
		  (t  ;; per default: take the text here along (could be a tail of some element)
		   (setq content (concat content (filter-buffer-substring (point) (progn (nxml-forward-balanced-item) (point)))))))))
	content ))))

(defun noXML-get-content (position)
  "Gets the content of the element in overlay at POSITION in buffer."
  (let* (
	(from-here (noXML-find-element-start position))
	(to-here (noXML-find-element-end from-here))
	(nxml-sexp-element-flag t)
	content)
    (save-excursion
      (goto-char to-here)
      (while (> (point) from-here)
	  (goto-char (progn (nxml-token-before) xmltok-start))
	  (cond
	   ;; if the element starts at the beginning of the region, and is empty
	   ((and (= xmltok-start from-here)
		 (string-equal xmltok-type "empty-element"))
	    (setq content (noXML-fold-get-element-name xmltok-start)));; return its name
	   ;; if this is the end tag of the whole element, just ignore
	   ((= (nxml-token-after) to-here) t)
	   ((memq xmltok-type '(end-tag empty-element));; so this is a child element: use its overlays as content
	      ;; (progn
	      ;;   (noXML-fold-item 'block);; fold the region of the subelement here
	      ;; )
	    (progn
	      (goto-char (noXML-find-element-start (point)))
	      (let ((overlays (overlays-at (point))))
		(dolist (overlay overlays)
		  (if (overlay-get overlay 'display)
		      (setq content (concat (substring-no-properties (overlay-get overlay 'display)) content )))
		  ))))
	   ((string-equal xmltok-type "start-tag");; if we're on a start tag, ignore
	    t)
	   ((memq xmltok-type '(data space))
	    (setq content (concat (filter-buffer-substring xmltok-start (nxml-token-after)) content )))
	   (t t)))
	content
	)))

(defun noXML-fold-clearout-region (start end)
  "Permanently show all elements in region starting at START and ending at END."
  (interactive "r")
  (let ((overlays (overlays-in start end)))
    (noXML-fold-remove-overlays overlays)))

(defun noXML-fold-clearout-buffer ()
  "Permanently show all macros in the buffer."
  (interactive)
  (noXML-fold-clearout-region (point-min) (point-max)))

(defun noXML-fold-clearout-item ()
  "Remove all folds from the element on which point currently is located (and its children)."
  (interactive)
  (let ((overlays (overlays-in (noXML-find-element-start (point)) (noXML-find-element-end (point)))))
    (noXML-fold-remove-overlays overlays)))

(defun noXML-fold-remove-overlays (overlays)
  "Remove all overlays set by noXML-fold in OVERLAYS.
Return non-nil if a removal happened, nil
otherwise. Cf. `TeX-fold-remove-overlays'."
  (let (found)
    (while overlays
      (when (eq (overlay-get (car overlays) 'category) 'noXML-fold)
	(delete-overlay (car overlays))
	(setq found t))
      (setq overlays (cdr overlays)))
    found))

(defun noXML-fold-get-element-name (position)
  "Returns the name of the element POSITION is in, or, if POSITION is on an opening tag, that tag's name."
  (let ((nxml-sexp-element-flag nil))
   (save-excursion
     (goto-char position)
     (if (looking-at "<[^/!]")
	 (nxml-forward-balanced-item))
     (progn
       (nxml-scan-element-backward (nxml-token-before) t)
       (goto-char xmltok-start)	;; xmltok-start is set by nxml-scan-element-backward
       (xmltok-start-tag-local-name)))))

(defun noXML-fold-visible (&optional around-screen)
  "Fold what's approximately in the current window as best we can."
  (interactive
   (list (if current-prefix-arg (read-number "How many chars: ") noXML-fold-add-to-screen)))
  (let ((around-screen (or around-screen noXML-fold-add-to-screen 2000)))
    (noXML-fold-region (max (point-min) (- (window-start) around-screen)) (min (point-max) (+ (window-end) around-screen)))))

(defun noXML-fold-region (start end)
  "Fold all complete items in region from START to END."
  (interactive "r")
  (save-excursion 
    (save-restriction
      (let 
	  ((from-here start)
	   (to-here end)
	   (current-relative-depth 0)
	   (nxml-sexp-element-flag nil)
	   whackTree;; where we store tag starts and ends
	   elementVals
	   )
	(progn 
	  (narrow-to-region from-here to-here)
	  (goto-char to-here)
	;; walk through the region backwards, folding every proper element
	  (while (and (> (point) from-here)
		      (progn
			(nxml-token-before);; check where we are
			xmltok-type));; xmltok-type is nil if there's no token before
	    (progn
	      (if (member xmltok-type  (list 'start-tag 'end-tag 'empty-element));; if interesting
		  (unless (not (or whackTree (eq xmltok-type 'end-tag) (eq xmltok-type 'empty-element)));; make sure start-tag is not the first that's added to the tree
		    (progn
		      (add-to-list 'whackTree (cons xmltok-start (point)));; remember the current tag's start and end point
		      (setq elementVals ;; we need this for folding below
			    (cond 
			     ((equal xmltok-type 'start-tag) (cons (pop whackTree) (pop whackTree)
								   ;; cut out this element's start and end cons cell from the whackTree, that is, the first two elements.						
								   ))
			     ((equal xmltok-type 'empty-element) 
			      (pop whackTree)
			      )
			     ))
		      (if (member xmltok-type (list 'start-tag 'empty-element))
			  (if (noXML-is-inline)
			      (noXML-fold-item 'inline elementVals (* (- 0 (length whackTree)) noXML-overlay-priority-step))
			    (noXML-fold-item 'block elementVals (* (- 0 (length whackTree)) noXML-overlay-priority-step)))
			))))
	      (goto-char xmltok-start)
	      t))
	  )))))

(defun noXML-is-inline ()
  "Find out if last scanned element  is an inline element or not.

We simply check whether the start tag is preceded by only white
space or nothing to the start of the line (see
`noXML-is-not-inline-regexp' for the regex used). You can override
this by adding the element name to `noXML-inline-elements'."
  (save-excursion
      (let 
	  ((block-regexp (or noXML-is-not-inline-regexp "^\\s-*<[^/]"))
	   is-inline)
	(if (member (xmltok-start-tag-local-name) noXML-inline-elements)
	    t
	  (if (member (xmltok-start-tag-local-name) noXML-block-elements)
	      nil
	    (if (and xmltok-start (not (eq xmltok-type ())))
		(unless (eq xmltok-start (point-min))
		  (progn
		    (goto-char (+ 2 xmltok-start))
		    (if (not (looking-back block-regexp  (- xmltok-start 50)))
			t
		      ())))))))))


(defun noXML-fold-buffer ()
  "Hide all configured macros and environments in the current buffer.
The relevant macros are specified in the variable `noXML-fold-macro-spec-list'
and `noXML-fold-math-spec-list', and environments in `noXML-fold-env-spec-list'."
  (interactive)
  (noXML-fold-clearout-region (point-min) (point-max))
  (noXML-fold-region (point-min) (point-max)))


(defun noXML-fold-item (type &optional elementPositions depth)
  "Hide the item on which point currently is located.
TYPE specifies the type of item and can be one of the symbols
'inline or 'block, for inline and block elements respectively.
ELEMENTPOSITIONS specifies where the element starts and ends (or start and end of opening and closing element of block elements), and DEPTH
specifies the depth in the document tree (if not supplied, we try
to find out).  Return non-nil if an item was found and folded,
nil otherwise. Based on `TeX-fold-item'."
  (save-excursion
    (let* (
	   (item-start 
	    (if (listp (car elementPositions))
		(car (car elementPositions))
	      (car elementPositions))
	    )
	   (item-end (if (listp (cdr elementPositions))
			 (cdr (cdr elementPositions))
		       (cdr elementPositions)))
	   (item-name (xmltok-start-tag-qname))
	   )
      (when item-start
	(let* (
	       ;; figure out what this item is called (setq item-name 'anchor)
	       ;; (item-end (noXML-find-element-end item-start))
	       (starts-and-ends ;; a list of the start and end of each thing that should be overlayed here
		(if (and (not (eq xmltok-type 'empty-element)) (eq type 'block))
		    (list
		     (cons item-start 
			   (cdr (car elementPositions))
			   )
		     (cons
		      (car (cdr elementPositions))
		      item-end))
		  (list (cons item-start item-end))))
	       (display-string-spec ;; what to show when folding
		(or 
		 (cdr (assoc item-name noXML-fold-spec-list-internal))
		 (if noXML-fold-unspec-use-name
			(concat "[" item-name "]")
		      (if (eq type 'block)
			  noXML-fold-unspec-block-display-string
			noXML-fold-unspec-inline-display-string))))
	       (ovs ;; set up a list of the overlays
		 (mapcar
		  (lambda (start-and-end)
		    (noXML-fold-make-overlay 
		     (car start-and-end)
		     (cdr start-and-end)
		     type display-string-spec 
		     (if depth 
			 (* depth noXML-overlay-priority-step)
		       ;; recalculate overlay depth if not supplied
		       (noXML-overlay-prioritize (car start-and-end) (cdr start-and-end)))
		     ))
		  starts-and-ends)))
	  (noXML-fold-hide-item ovs))))))

(defun noXML-expression-start-end (position)
  "Find the start and end of an item at POSITION"
  (let ((nxml-sexp-element-flag nil))
    (save-excursion
      (let ((position position))
	(progn
	  (goto-char position)
	  (list (cons
		 position
		 (nxml-token-after))))))))

(defun noXML-fold-make-help-echo (start end)
  "Return a string to be used as the help echo of folded overlays.
The text between START and END will be used for this but cropped
to the length defined by `noXML-fold-help-echo-max-length'.  Line
breaks will be replaced by spaces. This is also what gets shown when 
the mouse is on the point."
  (let* ((spill (+ start noXML-fold-help-echo-max-length))
	 (lines (split-string (buffer-substring start (min end spill)) "\n"))
	 (result (pop lines)))
    (dolist (line lines)
      ;; Strip leading whitespace
      (when (string-match "^[ \t]+" line)
	(setq line (replace-match "" nil nil line)))
      ;; Strip trailing whitespace
      (when (string-match "[ \t]+$" line)
	(setq line (replace-match "" nil nil line)))
      (setq result (concat result " " line)))
    (when (> end spill) (setq result (concat result "...")))
    result))

(defun noXML-fold-partition-list (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))

;; this allows us to use a function instead of the simple invisible
;; property: we have to set invisible to this:
;; (put 'noXML-fold 'reveal-toggle-invisible 'noXML-fold-reveal-toggle-invisible)

(defun noXML-fold-show-item (ov)
  "Show a single element.
Remove the respective properties from the overlay OV."
  (overlay-put ov 'mouse-face nil)
  (if (featurep 'xemacs)
      (progn
	(set-extent-property ov 'end-glyph nil)
	(overlay-put ov 'invisible nil))
    (overlay-put ov 'display nil)
    (overlay-put ov 'help-echo nil)
    (when font-lock-mode
      (overlay-put ov 'face noXML-fold-unfolded-face))))

(defun noXML-fold-post-command ()
  "Take care to fold/unfold stuff as we move along."
  ;; `with-local-quit' is not supported in XEmacs.
  (condition-case nil
      (let ((inhibit-quit nil))
	(condition-case err
	    (let* ((spots (noXML-fold-partition-list
			   (lambda (x)
			     ;; We refresh any spot in the current
			     ;; window as well as any spots associated
			     ;; with a dead window or a window which
			     ;; does not show this buffer any more.
			     (or (eq (car x) (selected-window))
				 (not (window-live-p (car x)))
				 (not (eq (window-buffer (car x))
					  (current-buffer)))))
			   noXML-fold-open-spots))
		   (old-ols (mapcar 'cdr (car spots))))
	      (setq noXML-fold-open-spots (cdr spots))
	      (when (or (and (boundp 'disable-point-adjustment)
			     disable-point-adjustment)
			(and (boundp 'global-disable-point-adjustment)
			     global-disable-point-adjustment)
			;; See preview.el on how to make this configurable.
			(memq this-command
			      (list (key-binding [left]) (key-binding [right])
				    'backward-char 'forward-char
				    'mouse-set-point)))
		;; Open new overlays.
		(dolist (ol (nconc (when ;; we use either the overalays in the region 
				       (and noXML-fold-unfold-around-mark
					      (boundp 'mark-active)
					      mark-active)
				     (overlays-at (mark)))
				   (overlays-at (point))));; or at the point
		  (when (eq (overlay-get ol 'category) 'noXML-fold)
		    (push (cons (selected-window) ol) noXML-fold-open-spots)
		    (setq old-ols (delq ol old-ols))
		    (noXML-fold-show-item ol))))
	      ;; Close old overlays.
	      (dolist (ol old-ols)
		(when (and (eq (current-buffer) (overlay-buffer ol))
			   (not (rassq ol noXML-fold-open-spots))
			   (or (not (featurep 'xemacs))
			       (and (featurep 'xemacs)
				    (not (extent-detached-p ol)))))
		  (if (and (>= (point) (overlay-start ol))
			   (<= (point) (overlay-end ol)))
		      ;; Still near the overlay: keep it open.
		      (push (cons (selected-window) ol) noXML-fold-open-spots)
		    ;; Really close it.
		    (noXML-fold-hide-item (list ol))))))
	  (error (message "noXML-fold: %s" err))))
    (quit (setq quit-flag t))))

(defun noXML-fold-hide-item (ovs)
  "Hide a single inline or block item.
That means, put respective properties onto overlay OV. Based on `TeX-fold-hide-item'."
  (dolist (ov ovs)
    (let* ((nxml-sexp-element-flag nil)
	   (ov-start (overlay-start ov))
	   (ov-end (overlay-end ov))
	   (spec (overlay-get ov 'noXML-fold-display-string-spec))
	   (computed (cond ;; the specification spec can be either a string or a function
		      ((stringp spec)
		     ;;(noXML-fold-expand-spec spec ov-start ov-end);; yes, not really relevant for xml folding i think
		     spec
		     )
		    ((functionp spec);; if we have a function to call here
		     (let ((arg ov-start) result)
		       (setq result (or (condition-case nil
					    (save-restriction
					      (apply spec arg nil));; the nil is necessary, last arg to apply is a list
					  ;; (noXML-render-first-child arg)
					  ;; (noXML-render-direct-children arg)
					  (error nil))
					(format "[Error: Content extraction function %s had a problem.]" spec)))
		       (nxml-token-before);; reset scan state
		       result;; show result
		       ))
		    (t "[Error: No content found]")))
	 (display-string (if (listp computed) (car computed) computed))
	 (face (when (listp computed) (cadr computed))))
    ;; Cater for zero-length display strings.
    (when (string= display-string "") (setq display-string noXML-fold-ellipsis))
    ;; Add a linebreak to the display string and adjust the overlay end
    ;; in case of an overfull line.
    (when (noXML-fold-overfull-p ov-start ov-end display-string)
      (setq display-string (concat display-string "\n"))
      (move-overlay ov ov-start (save-excursion
				  (goto-char ov-end)
				  (skip-chars-forward " \t")
				  (point))))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'display display-string);; see info: File: elisp,  Node: Text Props and Strings,  Prev: Nonprinting Characters,  Up: String Type
    (if (featurep 'xemacs)
	(let ((glyph (make-glyph (if (listp display-string)
				     (car display-string)
				   display-string))))
	  ;; (overlay-put ov 'invisible t)
	  (overlay-put ov 'invisible 'noXML-fold)
	  (when font-lock-mode
	    (if face
		(set-glyph-property glyph 'face face)
	      (set-glyph-property glyph 'face noXML-fold-folded-face)))
	  (set-extent-property ov 'end-glyph glyph))
      (when font-lock-mode
	(overlay-put ov 'face noXML-fold-folded-face))
      (unless (zerop noXML-fold-help-echo-max-length)
	(overlay-put ov 'help-echo (noXML-fold-make-help-echo
				    (overlay-start ov) (overlay-end ov))))))))


(defun noXML-fold-dwim ()
  "Hide or show items according to the current context.
If there is folded content, unfold it.  If there is a marked
region, fold all configured content in this region.  If there is
no folded content but an inline  or block element, fold it."
  (interactive)
  (cond ((use-region-p) 
	 (cond
	  ((noXML-fold-clearout-region (region-beginning) (region-end)) (message "Unfolded region."))
	  ((noXML-fold-region (region-beginning) (region-end)) (message "Folded element."))
	  ))
	((overlays-at (point)) (noXML-fold-clearout-item) (message "Unfolded item."))
	((noXML-fold-visible) (message "Folded window."))))

(defun noXML-make-overlay-visible (position)
  (interactive "d");; interactive, with the point of the mark as an integer
  (let (
	(noxmlov (make-overlay (noXML-find-element-start (point)) (noXML-find-element-end (point)) ))
	)
    (overlay-put noxmlov 'invisible nil)
    ))

;; I think this is not very useful for xml-folding:
;; (defun noXML-fold-expand-spec (spec ov-start ov-end)
;;   "Expand instances of {<num>}, [<num>], <<num>>, and (<num>).
;; Replace them with the respective macro argument."
;;   (let ((spec-list (split-string spec "||"))
;; 	(delims '((?{ . ?}) (?[ . ?]) (?< . ?>) (?\( . ?\))))
;; 	index success)
;;     (catch 'success
;;       ;; Iterate over alternatives.
;;       (dolist (elt spec-list)
;; 	(setq spec elt
;; 	      index nil)
;; 	;; Find and expand every placeholder.
;; 	(while (and (string-match "\\([[{<]\\)\\([1-9]\\)\\([]}>]\\)" elt index)
;; 		    ;; Does the closing delim match the opening one?
;; 		    (string-equal
;; 		     (match-string 3 elt)
;; 		     (char-to-string
;; 		      (cdr (assq (string-to-char (match-string 1 elt))
;; 				 delims)))))
;; 	  (setq index (match-end 0))
;; 	  (let ((arg (car (save-match-data
;; 			    ;; Get the argument.
;; 			    (noXML-fold-macro-nth-arg
;; 			     (string-to-number (match-string 2 elt))
;; 			     ov-start ov-end
;; 			     (assoc (string-to-char (match-string 1 elt))
;; 				    delims))))))
;; 	    (when arg (setq success t))
;; 	    ;; Replace the placeholder in the string.
;; 	    (setq elt (replace-match (or arg noXML-fold-ellipsis) nil t elt)
;; 		  index (+ index (- (length elt) (length spec)))
;; 		  spec elt)))
;; 	(when success (throw 'success nil))))
;;     spec))

;; (defun noXML-fold-macro-nth-arg (n macro-start &optional macro-end delims)
;;   "Return a property list of the argument number N of a macro.
;; The start of the macro to examine is given by MACRO-START, its
;; end optionally by MACRO-END.  With DELIMS the type of delimiters
;; can be specified as a cons cell containing the opening char as
;; the car and the closing char as the cdr.  The chars have to have
;; opening and closing syntax as defined in
;; `TeX-search-syntax-table'.

;; The first item in the returned list is the string specified in
;; the argument, the second item may be a face if the argument
;; string was fontified.  In Emacs the string holds text properties
;; as well, so the second item is always nil.  In XEmacs the string
;; does not enclose any faces, so these are given in the second item
;; of the resulting list."
;;   (save-excursion
;;     (let* ((macro-end (or macro-end
;; 					  (noXML-find-element-end macro-start)))
;; 	   (open-char (if delims (car delims) ?<))
;; 	   (open-string (char-to-string open-char))
;; 	   (close-char (if delims (cdr delims) ?>))
;; 	   (close-string (char-to-string close-char))
;; 	   content-start content-end)
;;       (goto-char macro-start)
;;       (if (condition-case nil
;; 	      (progn
;; 		(while (> n 0)
;; 		  (skip-chars-forward (concat "^" open-string) macro-end)
;; 		  (when (= (point) macro-end)
;; 		    (error nil))
;; 		  (setq content-start (progn
;; 					(skip-chars-forward
;; 					 (concat open-string " \t"))
;; 					(point)))
;; 		  (noXML-find-element-end macro-start)
;; 		  (setq content-end (save-excursion
;; 				      (backward-char)
;; 				      (skip-chars-backward " \t")
;; 				      (point)))
;; 		  (setq n (1- n)))
;; 		t)
;; 	    (error nil))
;; 	  (list (noXML-fold-buffer-substring content-start content-end)
;; 		(when (and (featurep 'xemacs)
;; 			   (extent-at content-start))
;; 		  ;; A glyph in XEmacs does not seem to be able to hold more
;; 		  ;; than one face, so we just use the first one we get.
;; 		  (car (extent-property (extent-at content-start) 'face))))
;; 	nil))))

(defun noXML-fold-overfull-p (ov-start ov-end display-string)
  "Return t if an overfull line will result after adding an overlay.
The overlay extends from OV-START to OV-END and will display the
string DISPLAY-STRING."
  (and (not (featurep 'xemacs)) ; Linebreaks in glyphs don't
				; work in XEmacs anyway.
       (save-excursion
	 (goto-char ov-end)
	 (search-backward "\n" ov-start t))
       (not (string-match "\n" display-string))
       (> (+ (- ov-start
		(save-excursion
		  (goto-char ov-start)
		  (line-beginning-position)))
	     (length display-string)
	     (- (save-excursion
		  (goto-char ov-end)
		  (line-end-position))
		ov-end))
	  (current-fill-column))))
  
(defun noXML-fold-buffer-substring (start end)
  "Return the contents of buffer from START to END as a string.
Like `buffer-substring' but copy overlay display strings as well."
  ;; Swap values of `start' and `end' if necessary.
  (when (> start end) (let ((tmp start)) (setq start end end tmp)))
  (let ((overlays (overlays-in start end))
	result)
    ;; Get rid of overlays not under our control or not completely
    ;; inside the specified region.
    (dolist (ov overlays)
      (when (or (not (eq (overlay-get ov 'category) 'noXML-fold))
		(< (overlay-start ov) start)
		(> (overlay-end ov) end))
	(setq overlays (remove ov overlays))))
    (if (null overlays)
	(buffer-substring start end)
      ;; Sort list according to ascending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (< (overlay-start a) (overlay-start b)))))
      ;; Get the string from the start of the region up to the first overlay.
      (setq result (buffer-substring start (overlay-start (car overlays))))
      (let (ov)
	(while overlays
	  (setq ov (car overlays)
		overlays (cdr overlays))
	  ;; Add the display string of the overlay.
	  (setq result (concat result (overlay-get ov 'display)))
	  ;; Remove overlays contained in the current one.
	  (dolist (elt overlays)
	    (when (< (overlay-start elt) (overlay-end ov))
	      (setq overlays (remove elt overlays))))
	  ;; Add the string from the end of the current overlay up to
	  ;; the next overlay or the end of the specified region.
	  (setq result (concat result (buffer-substring (overlay-end ov)
							(if overlays
							    (overlay-start
							     (car overlays))
							  end))))))
      result)))


;; not strictly useful for folding
(defun noXML-where (&optional attribute)
  "Display the hierarchy of XML elements the point is on as a path.

Following a suggestion from http://www.emacswiki.org/emacs/NxmlMode#toc11.
"
  (interactive)
  (let ((nxml-sexp-element-flag nil)
	path)
    (save-excursion
      (save-restriction
	(widen)
	(while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
		    (condition-case nil
			(progn
			  (nxml-backward-up-element) ; always returns nil
			  t)
		      (error nil)))
	  (setq path (cons (concat (xmltok-start-tag-local-name)
				   (if attribute
				       (noXML-element-attribute-get-value attribute "[" "]")
				     "")) 
			   path)))
	(if (called-interactively-p t)
	    (message "/%s" (mapconcat 'identity path "/"))
	  (format "/%s" (mapconcat 'identity path "/")))))))


;;; load everything as minor mode
;;;###autoload
(define-minor-mode noXML-fold-mode
  "Minor mode for hiding and revealing XML tags.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  nil " noXML" (list (cons noXML-fold-command-prefix noXML-fold-keymap))
  (if (and noXML-fold-mode (string-equal "nxml-mode" major-mode))
      (progn
	;; (set 'nxml-sexp-element-flag nil);; functions depend on this!---> should *really* be bound in functions as needed
	(set (make-local-variable 'search-invisible) t)
	(set (make-local-variable 'noXML-fold-spec-list-internal) nil)
	;; (setq-default noXML-fold-spec-list-internal nil)
	(add-hook 'post-command-hook 'noXML-fold-post-command t t)
	;; (add-hook 'noXML-fill-newline-hook 'noXML-fold-update-at-point nil t)
	(add-hook 'noXML-after-insert-macro-hook
		  (lambda ()
		    (when (and noXML-fold-mode noXML-fold-auto)
		      (save-excursion
			(backward-char)
			(or (noXML-fold-item 'inline (point))
			    (noXML-fold-item 'block (point)))))))
	;; Update the `noXML-fold-*-spec-list-internal' variables.
	(set (intern "noXML-fold-spec-list-internal") (noXML-fold-flatten-spec-list (symbol-value (intern "noXML-fold-spec-list")))))
    (kill-local-variable 'search-invisible)
    (kill-local-variable 'noXML-fold-spec-list-internal)
    (remove-hook 'post-command-hook 'noXML-fold-post-command t)
    ;; (remove-hook 'LaTeX-fill-newline-hook 'noXML-fold-update-at-point t)
    (noXML-fold-clearout-buffer))
  ;; (noXML-set-mode-name)
  )

(provide 'noxml-fold)

;;; noxml-fold.el ends here
