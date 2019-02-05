;;; el_client.el --- XELB Code Generator  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 'el_client' is responsible for converting XCB XML description files into
;; Elisp libraries.  Here are a few design guidelines:
;; + The generated codes should be human-readable and conform to the Elisp
;;   coding conventions.  Names mentioned in X specifications are preferred.
;; + Deprecated features such as <valueparam> should be dropped, for
;;   - they would generate incompatible codes, and
;;   - they are probably already dropped upstream.
;; + All documentations (within <doc> tags) and comments should be stripped
;;   out to reduce the overall amount of code.  XELB application developers are
;;   then encouraged to refer to the corresponding specifications to get an
;;   authoritative explanation.

;; This file is only intended to be run as a script.

;; References:
;; + xcb/proto (git://anongit.freedesktop.org/xcb/proto)

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'eieio)
(require 'pp)

;; Only used to eliminate compile warnings when distributed.
(require 'xcb-types nil t)

;;;; Variables

(defconst xelb-excluded-replies<25 '(xcb:xkb:GetKbdByName~reply)
  "Excluded replies for Emacs < 25 (they're too long to load/compile).")

(defvar xelb-prefix "xcb:" "Namespace of this module.")

(defvar xelb-error-alist nil "Record X errors in this module.")

(defvar xelb-event-alist nil "Record X events in this module.")

(defvar xelb-xge-alist nil "Record X generic events in this module.")

(defvar xelb-imports nil "Record imported libraries.")

(defvar xelb-pad-count -1 "<pad> node counter.")

(defvar xelb-request-fields nil "Fields in the current request.")

;;;; Helper functions

(defsubst xelb-node-name (node)
  "Return the tag name of node NODE."
  (car node))

(defsubst xelb-node-attr (node attr)
  "Return the attribute ATTR of node NODE."
  (cdr (assoc attr (cadr node))))

(defsubst xelb-node-type (node)
  "Return the type of node NODE."
  (let ((type-name (xelb-node-attr node 'type))
        type)
    (if (string-match ":" type-name)
        ;; Defined explicitly.
        (if (setq type
                  (intern-soft (concat "xcb:"
                                       (replace-regexp-in-string "^xproto:" ""
                                                                 type-name))))
            type
          (error "Undefined type: %s" type-name))
      (if (setq type (or (intern-soft (concat xelb-prefix type-name))
                         (intern-soft (concat "xcb:" type-name))))
          ;; Defined by the core protocol or this extension.
          type
        (catch 'break
          (dolist (i xelb-imports)
            (setq type (intern-soft (concat i type-name)))
            (when type
              (throw 'break type))))
        (if type
            ;; Defined by an imported extension.
            type
          ;; Not defined.
          (error "Undefined type: %s" type-name))))))

(defsubst xelb-escape-name (name)
  "Replace underscores in NAME with dashes."
  (replace-regexp-in-string "_" "-" name))

(defsubst xelb-node-name-escape (node)
  "Return the tag name of node NODE and escape it."
  (xelb-escape-name (xelb-node-name node)))

(defsubst xelb-node-attr-escape (node attr)
  "Return the attribute ATTR of node NODE and escape it."
  (xelb-escape-name (xelb-node-attr node attr)))

(defsubst xelb-node-subnodes (node &optional mark-auto-padding)
  "Return all the subnodes of node NODE as a list.

If MARK-AUTO-PADDING is non-nil, all <list>'s fitting for padding will include
an `xelb-auto-padding' attribute."
  (let ((subnodes (cddr node)))
    (when mark-auto-padding
      ;; Remove all <comment>'s and <doc>'s
      (cl-delete-if (lambda (i) (or (eq 'comment (car i)) (eq 'doc (car i))))
                    subnodes)
      (dotimes (i (1- (length subnodes)))
        (when (and (eq 'list (xelb-node-name (elt subnodes i)))
                   (pcase (xelb-node-name (elt subnodes (1+ i)))
                     ((or `reply `pad))
                     (_ t)))
          (setf (cadr (elt subnodes i))
                (nconc (cadr (elt subnodes i)) `((xelb-auto-padding . t)))))))
    subnodes))

(defsubst xelb-node-subnode (node)
  "Return the (only) subnode of node NODE with useless contents skipped."
  (let ((result (xelb-node-subnodes node)))
    (catch 'break
      (dolist (i result)
        (unless (and (listp i)
                     (or (eq (xelb-node-name i) 'comment)
                         (eq (xelb-node-name i) 'doc)))
          (throw 'break i))))))

(defun xelb-node-size (node)
  "Return the size of NODE in bytes."
  (pcase (xelb-node-name node)
    (`pad (xelb-node-attr node 'bytes))
    (`field (xelb-type-size (xelb-node-type node)))
    (`list (* (xelb-type-size (xelb-node-type node))
              (xelb-parse-expression (xelb-node-subnode node))))
    ((or `comment `doc) 0)
    (x (error "Unexpected element: <%s>" x))))

(defun xelb-type-size (type &optional slot)
  "Return size of TYPE in bytes."
  (pcase (or (get type 'xcb--typealias) type)
    (`xcb:-ignore 0)
    ((or `xcb:-u1 `xcb:-i1 `xcb:void) 1)
    ((or `xcb:-u2 `xcb:-i2) 2)
    ((or `xcb:-u4 `xcb:-i4) 4)
    (`xcb:-u8 8)
    (`xcb:-pad (cl--slot-descriptor-initform slot))
    (`xcb:-list
     (let ((initform (cadr (cl--slot-descriptor-initform slot))))
       (* (plist-get initform 'size)
          (xelb-type-size (plist-get initform 'type)))))
    ((and x (guard (child-of-class-p x 'xcb:-struct)))
     (apply #'+
            (mapcar (lambda (slot)
                      (xelb-type-size (cl--slot-descriptor-type slot) slot))
                    (eieio-class-slots x))))
    (x (error "Unknown size of type: %s" x))))

(defsubst xelb-generate-pad-name ()
  "Generate a new slot name for <pad>."
  (make-symbol (format "pad~%d" (cl-incf xelb-pad-count))))

;;;; Entry & root element

(defun xelb-parse (file)
  "Parse an XCB protocol description file FILE (XML)."
  (let ((pp-escape-newlines nil)        ;do not escape newlines
        result header)
    (with-temp-buffer
      (insert-file-contents file)
      (setq result (libxml-parse-xml-region (point-min) (point-max)))
      (unless (eq 'xcb (xelb-node-name result))
        ;; There's an extra comment.
        (setq result (xelb-node-subnode result)))
      (cl-assert (eq 'xcb (xelb-node-name result)))
      (setq header (xelb-node-attr result 'header))
      (unless (string= header "xproto")
        (setq xelb-prefix (concat xelb-prefix header ":")))
      ;; Print header
      (princ (format "\
;;; xcb-%s.el --- X11 %s  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file was generated by 'el_client.el' from '%s',
;; which you can retrieve from <git://anongit.freedesktop.org/xcb/proto>.

;;; Code:

\(require 'xcb-types)

"
                     header
                     (let ((extension-name (xelb-node-attr result
                                                           'extension-name)))
                       (if extension-name
                           (concat extension-name " extension")
                         "core protocol"))
                     (file-name-nondirectory file)))
      ;; Print extension info (if any)
      (let ((extension-xname (xelb-node-attr result 'extension-xname))
            (extension-name (xelb-node-attr result 'extension-name))
            (major-version (xelb-node-attr result 'major-version))
            (minor-version (xelb-node-attr result 'minor-version)))
        (when extension-xname
          (pp `(defconst ,(intern (concat xelb-prefix "-extension-xname"))
                 ,extension-xname)))
        (when extension-name
          (pp `(defconst ,(intern (concat xelb-prefix "-extension-name"))
                 ,extension-name)))
        (when major-version
          (pp `(defconst ,(intern (concat xelb-prefix "-major-version"))
                 ,(string-to-number major-version))))
        (when minor-version
          (pp `(defconst ,(intern (concat xelb-prefix "-minor-version"))
                 ,(string-to-number minor-version))))
        (when (or extension-xname extension-name major-version minor-version)
          (princ "\n")))
      ;; Print contents
      (dolist (i (xelb-node-subnodes result))
        (let ((result (xelb-parse-top-level-element i)))
          (when result                  ;skip <doc>, comments, etc
            (dolist (j result)
              (eval j)                 ;Make it immediately available.
              (pp j))
            (princ "\n"))))
      ;; Print error/event alists
      (when xelb-error-alist
        (pp
         `(defconst ,(intern (concat xelb-prefix "error-number-class-alist"))
            ',xelb-error-alist "(error-number . error-class) alist."))
        (princ "\n"))
      (when xelb-event-alist
        (pp
         `(defconst ,(intern (concat xelb-prefix "event-number-class-alist"))
            ',xelb-event-alist "(event-number . event-class) alist."))
        (princ "\n"))
      (when xelb-xge-alist
        (pp
         `(defconst ,(intern (concat xelb-prefix "xge-number-class-alist"))
            ',xelb-xge-alist "(xge-number . event-class) alist."))
        (princ "\n"))
      ;; Print footer
      (princ (format "\


(provide 'xcb-%s)

;;; xcb-%s.el ends here
" header header)))))

;;;; XCB: top-level elements

(defun xelb-parse-top-level-element (node)
  "Parse a top-level node NODE."
  (setq xelb-pad-count -1)
  (pcase (xelb-node-name node)
    (`import (xelb-parse-import node))
    (`struct (xelb-parse-struct node))
    (`union (xelb-parse-union node))
    ((or `xidtype `xidunion)
     (xelb-parse-xidtype node))         ;they are basically the same
    (`enum (xelb-parse-enum node))
    (`typedef (xelb-parse-typedef node))
    (`request (xelb-parse-request node))
    (`event (xelb-parse-event node))
    (`error (xelb-parse-error node))
    (`eventcopy (xelb-parse-eventcopy node))
    (`errorcopy (xelb-parse-errorcopy node))
    (`eventstruct (xelb-parse-eventstruct node))
    ((or `comment `doc))                ;ignored
    (x (error "Unsupported top-level element: <%s>" x))))

(defun xelb-parse-import (node)
  "Parse <import>."
  (let* ((name (xelb-node-subnode node))
         (header (intern (concat "xcb-" name))))
    (require header)
    (push (concat "xcb:" name ":") xelb-imports)
    `((require ',header))))

(defun xelb-parse-struct (node)
  "Parse <struct>."
  (let ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
        (contents (xelb-node-subnodes node t)))
    `((defclass ,name (xcb:-struct)
        ,(apply #'nconc (mapcar #'xelb-parse-structure-content contents))))))

(defun xelb-parse-union (node)
  "Parse <union>."
  (let ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
        (contents (xelb-node-subnodes node)))
    `((defclass ,name (xcb:-union)
        ,(apply #'nconc
                `((~size :initform
                         ,(apply #'max (mapcar #'xelb-node-size contents))))
                (mapcar #'xelb-parse-structure-content contents))))))

(defun xelb-parse-xidtype (node)
  "Parse <xidtype>."
  (let ((name (intern (concat xelb-prefix (xelb-node-attr node 'name)))))
    `((xcb:deftypealias ',name 'xcb:-u4))))

(defun xelb-parse-enum (node)
  "Parse <enum>."
  (let ((name-prefix (concat xelb-prefix (xelb-node-attr node 'name) ":"))
        (items (xelb-node-subnodes node))
        (value 0))
    (delq nil                ;remove nil's produced by tags like <doc>
          (mapcar (lambda (i)
                    (when (eq (xelb-node-name i) 'item)
                      ;; Only handle <item> tags
                      (let* ((name (xelb-node-attr i 'name))
                             (name (intern (concat name-prefix name)))
                             (expression (xelb-node-subnode i)))
                        (if expression
                            (setq value (xelb-parse-expression expression))
                          (setq value (1+ value)))
			;; Omit the rare enums that do not fit in a fixnum in
			;; 32-bit Emacs, so that the resulting .el and .elc
			;; files are portable to 32-bit Emacs.  Admittedly
			;; this is a kludge.
			(unless (and (integerp value)
				     (not (<= -536870912 value 536870911)))
			  `(defconst ,name ,value)))))
                  items))))

(defun xelb-parse-typedef (node)
  "Parse <typedef>."
  (let* ((oldname (xelb-node-attr node 'oldname))
         (oldname (or (intern-soft (concat xelb-prefix oldname))
                      (intern-soft (concat "xcb:" oldname))
                      (intern (concat xelb-prefix oldname))))
         (newname (intern (concat xelb-prefix
                                  (xelb-node-attr node 'newname)))))
    `((xcb:deftypealias ',newname ',oldname))))

(defun xelb-parse-request (node)
  "Parse <request>.

The `combine-adjacent' attribute is simply ignored."
  (let* ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
         (opcode (string-to-number (xelb-node-attr node 'opcode)))
         (contents `((~opcode :initform ,opcode :type xcb:-u1)))
         (subnodes (xelb-node-subnodes node t))
         expressions
         result reply-name reply-contents)
    ;; Fill `xelb-request-fields'.
    (setq xelb-request-fields nil)
    (dolist (i subnodes)
      (unless (eq (xelb-node-name i) 'reply)
        (let ((name (xelb-node-attr i 'name)))
          (when name
            (push (intern (xelb-escape-name name)) xelb-request-fields)))))
    (dolist (i subnodes)
      (if (not (eq (xelb-node-name i) 'reply))
          (progn
            (setq result (xelb-parse-structure-content i))
            (if (eq 'exprfield (xelb-node-name i))
                ;; Split into field and expression
                (setq contents (nconc contents (list (car result)))
                      expressions (nconc expressions (list (cadr result))))
              (setq contents (nconc contents result))))
        ;; Parse <reply>
        (setq xelb-pad-count -1)        ;reset padding counter
        (setq xelb-request-fields nil)  ;Clear `xelb-request-fields'.
        (setq reply-name
              (intern (concat xelb-prefix (xelb-node-attr node 'name)
                              "~reply")))
        (setq reply-contents (xelb-node-subnodes i t))
        (setq reply-contents
              (apply #'nconc
                     (mapcar #'xelb-parse-structure-content reply-contents)))))
    (setq xelb-request-fields nil)      ;Clear `xelb-request-fields'.
    (delq nil contents)
    (delq nil
          `((defclass ,name (xcb:-request) ,contents)
            ;; The optional expressions
            ,(when expressions
               `(cl-defmethod xcb:marshal ((obj ,name)) nil
                              ,@expressions
                              (cl-call-next-method obj)))
            ,(when (memq reply-name xelb-excluded-replies<25)
               ;; Redefine `defclass' as no-op.
               '(eval-and-compile
                  (when (< emacs-major-version 25)
                    (fset 'xcb:-defclass (symbol-function 'defclass))
                    (defmacro defclass (&rest _args)))))
            ;; The optional reply body
            ,(when reply-name
               (delq nil reply-contents)
               ;; Insert slots for sequence number and reply length.
               (setcdr reply-contents (append '((~sequence :type xcb:CARD16)
                                                (length :type xcb:CARD32))
                                              (cdr reply-contents)))
               `(defclass ,reply-name (xcb:-reply) ,reply-contents))
            ,(when (memq reply-name xelb-excluded-replies<25)
               ;; Bring back the original defination of `defclass'.
               '(eval-and-compile
                  (when (< emacs-major-version 25)
                    (fset 'defclass (symbol-function 'xcb:-defclass)))))))))

(defun xelb-parse-event (node)
  "Parse <event>."
  (let ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
        (event-number (string-to-number (xelb-node-attr node 'number)))
        (no-sequence-number (xelb-node-attr node 'no-sequence-number))
        (xge (xelb-node-attr node 'xge))
        (contents (xelb-node-subnodes node t))
        xge-extension)
    (setq xge-extension (and xge (not (eq name 'xcb:GeGeneric))))
    (setq contents
          (apply #'nconc (mapcar #'xelb-parse-structure-content contents)))
    (unless (or no-sequence-number xge)
      (setcdr contents (append '((~sequence :type xcb:CARD16))
                               (cdr contents))))
    ;; Add the event code.
    (unless (and xge (not xge-extension))
      (push `(,(if xge '~evtype '~code) :initform ,event-number) contents))
    (if xge-extension
        (setq xelb-xge-alist
              (nconc xelb-xge-alist `((,event-number . ,name))))
      (setq xelb-event-alist
            (nconc xelb-event-alist `((,event-number . ,name)))))
    `((defclass ,name (,(if xge 'xcb:-generic-event 'xcb:-event)) ,contents))))

(defun xelb-parse-error (node)
  "Parse <error>."
  (let ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
        (error-number (string-to-number (xelb-node-attr node 'number)))
        (contents (xelb-node-subnodes node t)))
    (setq xelb-error-alist (nconc xelb-error-alist `((,error-number . ,name))))
    `((defclass ,name (xcb:-error)
        ,(append
          ;; The error code.
          `((~code :initform ,error-number))
          ;; The contents.
          (apply #'nconc (mapcar #'xelb-parse-structure-content contents)))))))

(defun xelb-parse-eventcopy (node)
  "Parse <eventcopy>."
  (let* ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
         (refname (xelb-node-attr node 'ref))
         (refname (or (intern-soft (concat xelb-prefix refname))
                      (intern-soft (concat "xcb:" refname))
                      (intern (concat xelb-prefix refname))))
         (xge (child-of-class-p refname 'xcb:-generic-event))
         (event-number (string-to-number (xelb-node-attr node 'number))))
    (if xge
        (setq xelb-xge-alist
              (nconc xelb-xge-alist `((,event-number . ,name))))
      (setq xelb-event-alist
            (nconc xelb-event-alist `((,event-number . ,name)))))
    `((defclass ,name (xcb:-event ,refname) ;Shadow the method of ref.
        ((,(if xge '~evtype '~code) :initform ,event-number))))))

(defun xelb-parse-errorcopy (node)
  "Parse <errorcopy>."
  (let* ((name (intern (concat xelb-prefix (xelb-node-attr node 'name))))
         (refname (xelb-node-attr node 'ref))
         (refname (or (intern-soft (concat xelb-prefix refname))
                      (intern-soft (concat "xcb:" refname))
                      (intern (concat xelb-prefix refname))))
         (error-number (string-to-number (xelb-node-attr node 'number))))
    (setq xelb-error-alist (nconc xelb-error-alist `((,error-number . ,name))))
    `((defclass ,name (xcb:-error ,refname) ;Shadow the method of ref
        ((~code :initform ,error-number))))))

(defun xelb-parse-eventstruct (node)
  "Parse <eventstruct>."
  (let ((name (intern (concat xelb-prefix (xelb-node-attr node 'name)))))
    ;; Only conventional events are supported (and we don't check opcode).
    `((defclass ,name (xcb:-event) nil))))

;;;; XCB: structure contents

(defun xelb-parse-structure-content (node)
  "Parse a structure content node NODE."
  (pcase (xelb-node-name node)
    (`pad (xelb-parse-pad node))
    (`required_start_align (xelb-parse-required_start_align node))
    (`field (xelb-parse-field node))
    (`fd (xelb-parse-fd node))
    (`list (xelb-parse-list node))
    (`exprfield (xelb-parse-exprfield node))
    (`switch (xelb-parse-switch node))
    ((or `comment `doc))                ;simply ignored
    (x (error "Unsupported structure content: <%s>" x))))

;; The car of the result shall be renamed to prevent duplication of slot names
(defun xelb-parse-pad (node)
  "Parse <pad>."
  (let ((bytes (xelb-node-attr node 'bytes))
        (align (xelb-node-attr node 'align)))
    (if bytes
        `((,(xelb-generate-pad-name)
           :initform ,(string-to-number bytes) :type xcb:-pad))
      (if align
          `((,(xelb-generate-pad-name)
             :initform ,(string-to-number align) :type xcb:-pad-align))
        (error "Invalid <pad> field")))))

(defun xelb-parse-required_start_align (node)
  "Parse <required_start_align>."
  (let ((align (xelb-node-attr node 'align))
        (offset (xelb-node-attr node 'offset)))
    `((,(xelb-generate-pad-name)
       :initform ,(if offset
                      (vector (string-to-number align)
                              (string-to-number offset))
                    (string-to-number align))
       :type xcb:-pad-align))))

(defun xelb-parse-field (node)
  "Parse <field>."
  (let* ((name (intern (xelb-node-attr-escape node 'name)))
         (type (xelb-node-type node)))
    `((,name :initarg ,(intern (concat ":" (symbol-name name))) :type ,type))))

(defun xelb-parse-fd (node)
  "Parse <fd>."
  (let ((name (intern (xelb-node-attr-escape node 'name))))
    `((,name :type xcb:fd))))

(defun xelb-parse-list (node)
  "Parse <list>."
  (let* ((name (intern (xelb-node-attr-escape node 'name)))
         (name-alt (intern (concat (xelb-node-attr-escape node 'name) "~")))
         (type (xelb-node-type node))
         (size (xelb-parse-expression (xelb-node-subnode node))))
    `((,name-alt :initform '(name ,name type ,type size ,size)
                 :type xcb:-list)
      (,name :initarg ,(intern (concat ":" (symbol-name name)))
             :type xcb:-ignore))))

;; The car of result is the field declaration, and the cadr is the expression
;; to be evaluated.
(defun xelb-parse-exprfield (node)
  "Parse <exprfield>."
  (let* ((name (intern (xelb-node-attr-escape node 'name)))
         (type (xelb-node-type node))
         (value (xelb-parse-expression (xelb-node-subnode node))))
    `((,name :type ,type)
      (setf (slot-value obj ',name) ,value))))

;; The only difference between <bitcase> and <case> is whether the `condition'
;; is a list
;; The name attribute of <bitcase> and <case> seems not useful here.
(defun xelb-parse-switch (node)
  "Parse <switch>."
  (let ((name (intern (xelb-node-attr-escape node 'name)))
        (expression (xelb-parse-expression (car (xelb-node-subnodes node))))
        ;; <case> and <bitcase> only
        (cases (cl-remove-if-not (lambda (i)
                                   (memq (xelb-node-name i) '(case bitcase)))
                                 (xelb-node-subnodes node)))
        fields)
    ;; Avoid duplicated slot names by appending "*" if necessary
    (let (names name)
      (dolist (case cases)
        (pcase (xelb-node-name case)
          ((or `bitcase `case)
           (dolist (field (xelb-node-subnodes case))
             (pcase (xelb-node-name field)
               ((or `enumref `pad `doc `comment `required_start_align))
               (_
                (setq name (xelb-node-attr field 'name))
                (when (member name names)
                  (while (member name names)
                    (setq name (concat name "*")))
                  (setcdr (assoc 'name (cadr field)) name))
                (cl-pushnew name names :test #'equal))))))))
    (setq cases
          (mapcar (lambda (i)
                    (let ((case-name (xelb-node-name i))
                          condition name-list tmp)
                      (when (or (eq case-name 'bitcase) (eq case-name 'case))
                        (dolist (j (xelb-node-subnodes i t))
                          (pcase (xelb-node-name j)
                            (`enumref
                             (setq condition
                                   (nconc condition
                                          (list (xelb-parse-enumref j)))))
                            (_
                             (setq tmp (xelb-parse-structure-content j))
                             (setq fields (nconc fields tmp))
                             (setq name-list
                                   (nconc name-list (list (caar tmp)))))))
                        (when (eq case-name 'bitcase)
                          (setq condition (if (= 1 (length condition))
                                              ;; Flatten 1-element list.
                                              (car condition)
                                            (if (cl-every #'integerp condition)
                                                (apply #'logior condition)
                                              `(logior ,@condition))))))
                      `(,condition ,@name-list)))
                  cases))
    `((,name :initform '(expression ,expression cases ,cases)
             :type xcb:-switch)
      ,@fields)))

;;;; XCB: expressions

(defun xelb-parse-expression (node)
  "Parse an expression node NODE."
  (when node
    (pcase (xelb-node-name node)
      (`op (xelb-parse-op node))
      (`fieldref (xelb-parse-fieldref node))
      (`paramref (xelb-parse-paramref node))
      (`value (xelb-parse-value node))
      (`bit (xelb-parse-bit node))
      (`enumref (xelb-parse-enumref node))
      (`unop (xelb-parse-unop node))
      (`sumof (xelb-parse-sumof node))
      (`popcount (xelb-parse-popcount node))
      (`listelement-ref (xelb-parse-listelement-ref node))
      ((or `comment `doc))              ;simply ignored
      (x (error "Unsupported expression: <%s>" x)))))

(defun xelb-parse-op (node)
  "Parse <op>."
  (let* ((subnodes (xelb-node-subnodes node))
         (x (xelb-parse-expression (car subnodes)))
         (y (xelb-parse-expression (cadr subnodes))))
    (pcase (xelb-node-attr node 'op)
      ("+" `(+ ,x ,y))
      ("-" `(- ,x ,y))
      ("*" `(* ,x ,y))
      ("/" `(/ ,x ,y))
      ("&" `(logand ,x ,y))
      ("<<" `(lsh ,x ,y))
      (x (error "Unsupported operator: `%s'" x)))))

(defun xelb-parse-fieldref (node)
  "Parse <fieldref>."
  (let ((name (intern (xelb-escape-name (xelb-node-subnode node)))))
    (if (or (not xelb-request-fields)   ;Probably not a request.
            (memq name xelb-request-fields)
            (not (string-suffix-p "-len" (symbol-name name))))
        `(xcb:-fieldref ',name)
      `(length
        (xcb:-fieldref ',(intern (substring (symbol-name name) 0 -4)))))))

(defun xelb-parse-paramref (node)
  "Parse <paramref>."
  `(xcb:-paramref ',(intern (xelb-escape-name (xelb-node-subnode node)))))

(defun xelb-parse-value (node)
  "Parse <value>."
  (string-to-number
   (replace-regexp-in-string "^0x" "#x" (xelb-node-subnode node))))

(defun xelb-parse-bit (node)
  "Parse <bit>."
  (let ((bit (string-to-number (xelb-node-subnode node))))
    (cl-assert (<= 0 bit 31))
    (lsh 1 bit)))

(defun xelb-parse-enumref (node)
  "Parse <enumref>."
  (let ((name (concat (xelb-node-attr node 'ref) ":"
                      (xelb-node-subnode node))))
    (symbol-value (or (intern-soft (concat xelb-prefix name))
                      (intern-soft (concat "xcb:" name))
                      (intern (concat xelb-prefix name))))))

(defun xelb-parse-unop (node)
  "Parse <unop>."
  (cl-assert (string= "~" (xelb-node-attr node 'op)))
  `(lognot ,(xelb-parse-expression (xelb-node-subnode node))))

(defun xelb-parse-sumof (node)
  "Parse <sumof>."
  (let* ((ref (intern (xelb-node-attr-escape node 'ref)))
         (expression (xelb-node-subnode node))
         (list-data `(slot-value obj ',ref)))
    (if (not expression)
        `(apply #'+ ,list-data)
      (setq expression (xelb-parse-expression expression))
      `(apply #'+ (mapcar (lambda (i)
                            (eval ',expression (list (nconc '(obj) i))))
                          ,list-data)))))

(defun xelb-parse-popcount (node)
  "Parse <popcount>."
  (let ((expression (xelb-parse-expression (xelb-node-subnode node))))
    `(xcb:-popcount ,expression)))

(defun xelb-parse-listelement-ref (_node)
  "Parse <listelement-ref>."
  'obj)                      ;a list element is internally named 'obj'

;;;; The entry

(setq debug-on-error t)
(setq edebug-all-forms t)

(if (not argv)
    (error "Usage: el_client.el <protocol.xml> [additional_load_paths]")
  (add-to-list 'load-path default-directory)
  (dolist (i (cdr argv))
    (add-to-list 'load-path i))
  (require 'xcb-types)
  (xelb-parse (car argv)))

;;; el_client.el ends here
