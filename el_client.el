;;; el_client.el --- XELB Code Generator  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Keywords: unix

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; el_client is responsible for converting XCB XML description files into Elisp
;; libraries. Here are a few design guidelines:
;; + The generated codes should be human-readable and conform to the Elisp
;;   coding conventions. Names mentioned in X specifications are preferred.
;; + Deprecated features such as <valueparam> should be dropped, for
;;   - they would generate incompatible codes, and
;;   - they are probably already dropped upstream.
;; + All documentations (within <doc> tags) and comments should be stripped
;;   out to reduce the overall amount of code. XELB application developers are
;;   then encouraged to refer to the corresponding specifications to get an
;;   authoritative explanation.

;; This file is only intended to be run as a script.

;; References:
;; + xcb/proto (git://anongit.freedesktop.org/xcb/proto)

;;; Code:

(require 'cl-lib)

(setq pp-escape-newlines nil)

;;;; Variables

(defvar prefix "xcb:" "Namespace of this module.")
(make-variable-buffer-local 'prefix)

(defvar error-alist nil "Record X errors in this module.")
(make-variable-buffer-local 'error-alist)

(defvar event-alist nil "Record X events in this module.")
(make-variable-buffer-local 'event-alist)

(defvar pad-count 0 "<pad> node counter.")
(make-variable-buffer-local 'pad-count)

;;;; Helper functions

(defsubst node-name (node)
  "Return the tag name of node NODE."
  (car node))

(defsubst node-attr (node attr)
  "Return the attribute ATTR of node NODE."
  (cdr (assoc attr (cadr node))))

(defsubst escape-name (name)
  "Replace underscores in NAME with dashes."
  (replace-regexp-in-string "_" "-" name))

(defsubst node-name-escape (node)
  "Return the tag name of node NODE and escape it."
  (escape-name (node-name node)))

(defsubst node-attr-escape (node attr)
  "Return the attribute ATTR of node NODE and escape it."
  (escape-name (node-attr node attr)))

(defsubst node-subnodes (node)
  "Return all the subnodes of node NODE as a list."
  (cddr node))

(defsubst node-subnode (node)
  "Return the (only) subnode of node NODE with useless contents skipped."
  (let ((result (node-subnodes node)))
    (catch 'break
      (dolist (i result)
        (unless (and (listp i)
                     (or (eq (node-name i) 'comment) (eq (node-name i) 'doc)))
          (throw 'break i))))))

;;;; Entry & root element

(defun parse (file)
  "Parse an XCB protocol description file FILE (XML)."
  (let (result header)
    (with-temp-buffer
      (insert-file-contents file)
      (setq result (libxml-parse-xml-region (point-min) (point-max) nil t))
      (cl-assert (eq 'xcb (node-name result)))
      (setq header (node-attr result 'header))
      (unless (string= header "xproto")
        (setq prefix (concat prefix header ":")))
      ;; Print header
      (princ (format "\
;;; -*- lexical-binding: t -*-
;; This file was generated from `%s' by `el_client.el'.
\n(require 'xcb-types)\n\n" (file-name-nondirectory file)))
      ;; Print extension info (if any)
      (let ((extension-xname (node-attr result 'extension-xname))
            (extension-name (node-attr result 'extension-name))
            (major-version (node-attr result 'major-version))
            (minor-version (node-attr result 'minor-version)))
        (when extension-xname
          (pp `(defconst ,(intern (concat prefix "-extension-xname"))
                 ,extension-xname)))
        (when extension-name
          (pp `(defconst ,(intern (concat prefix "-extension-name"))
                 ,extension-name)))
        (when major-version
          (pp `(defconst ,(intern (concat prefix "-major-version"))
                 ,(string-to-number major-version))))
        (when minor-version
          (pp `(defconst ,(intern (concat prefix "-minor-version"))
                 ,(string-to-number minor-version))))
        (when (or extension-xname extension-name major-version minor-version)
          (princ "\n")))
      ;; Print contents
      (dolist (i (node-subnodes result))
        (let ((result (parse-top-level-element i)))
          (when result                  ;skip <doc>, comments, etc
            (dolist (j result)
              (pp j))
            (princ "\n"))))
      ;; Print error/event alists
      (when error-alist
        (pp `(defconst ,(intern (concat prefix "error-number-class-alist"))
               ',error-alist "(error-number . error-class) alist"))
        (princ "\n"))
      (when event-alist
        (pp `(defconst ,(intern (concat prefix "event-number-class-alist"))
               ',event-alist "(event-number . event-class) alist"))
        (princ "\n"))
      ;; Print footer
      (princ (format "\n\n(provide 'xcb-%s)\n" header)))))

;;;; XCB: top-level elements

(defun parse-top-level-element (node)
  "Parse a top-level element."
  (setq pad-count 0)
  (pcase (node-name node)
    (`import (parse-import node))
    (`struct (parse-struct node))
    (`union (parse-union node))
    ((or `xidtype `xidunion)
     (parse-xidtype node))              ;they are basically the same
    (`enum (parse-enum node))
    (`typedef (parse-typedef node))
    (`request (parse-request node))
    (`event (parse-event node))
    (`error (parse-error node))
    (`eventcopy (parse-eventcopy node))
    (`errorcopy (parse-errorcopy node))
    ((or `comment `doc))                ;ignored
    (x (error "Unsupported top-level element: <%s>" x))))

(defun parse-import (node)
  "Parse <import>."
  (let ((header (intern (concat "xcb-" (node-subnode node)))))
    (require header)
    `((require ',header))))

(defun parse-struct (node)
  "Parse <struct>."
  (let ((name (intern (concat prefix (node-attr node 'name))))
        (contents (node-subnodes node))
        result)
    (setq contents
          (apply 'nconc
                 (mapcar (lambda (i)
                           (setq result (parse-structure-content i))
                           (when (eq (node-name i) 'pad) ;rename pad~
                             (setq result
                                   `((,(intern (format "pad~%d" pad-count))
                                      ,@(cdar result))))
                             (setq pad-count (1+ pad-count)))
                           result)
                         contents)))
    `((defclass ,name (xcb:-struct) ,contents))))

(defun parse-union (node)
  "Parse <union>."
  (let ((name (intern (concat prefix (node-attr node 'name))))
        (contents (node-subnodes node)))
    (setq contents
          (apply 'nconc
                 (mapcar (lambda (i)
                           (parse-structure-content i))
                         contents)))
    `((defclass ,name (xcb:-union) ,contents))))

(defun parse-xidtype (node)
  "Parse <xidtype>."
  (let ((name (intern (concat prefix (node-attr node 'name)))))
    `((xcb:deftypealias ',name 'xcb:-u4))))

(defun parse-enum (node)
  "Parse <enum>."
  (let ((name-prefix (concat prefix (node-attr node 'name) ":"))
        (items (node-subnodes node))
        (value 0))
    (delq nil                ;remove nil's produced by tags like <doc>
          (mapcar (lambda (i)
                    (when (eq (node-name i) 'item) ;only handle <item> tags
                      (let* ((name (node-attr i 'name))
                             (name (intern (concat name-prefix name)))
                             (expression (node-subnode i)))
                        (if expression
                            (setq value (parse-expression expression))
                          (setq value (1+ value)))
                        `(defconst ,name ,value))))
                  items))))

(defun parse-typedef (node)
  "Parse <typedef>."
  (let* ((oldname (node-attr node 'oldname))
         (oldname (or (intern-soft (concat "xcb:" oldname))
                      (intern (concat prefix oldname))))
         (newname (intern (concat prefix (node-attr node 'newname)))))
    `((xcb:deftypealias ',newname ',oldname))))

(defun parse-request (node)
  "Parse <request>.

The `combine-adjacent' attribute is simply ignored."
  (let* ((name (intern (concat prefix (node-attr node 'name))))
         (opcode (string-to-int (node-attr node 'opcode)))
         (contents `((~opcode :initform ,opcode :type xcb:-u1)))
         (subnodes (node-subnodes node))
         (reply-pad-count 0)
         expressions
         result reply-result reply-name reply-contents)
    (dolist (i subnodes)
      (if (not (eq (node-name i) 'reply))
          (progn
            (setq result (parse-structure-content i))
            (pcase (node-name i)
              (`pad                     ;rename pad~
               (setq result
                     `((,(intern (format "pad~%d" pad-count))
                        ,@(cdar result))))
               (setq pad-count (1+ pad-count))
               (setq contents (nconc contents result)))
              (`exprfield             ;split into field and expression
               (setq contents (nconc contents (list (car result))))
               (setq expressions (nconc expressions (list (cadr result)))))
              (_ (setq contents (nconc contents result)))))
        ;; Parse <reply>
        (setq reply-name
              (intern (concat prefix (node-attr node 'name) "~reply")))
        (setq reply-contents (node-subnodes i))
        (setq reply-contents
              (apply 'nconc
                     (mapcar (lambda (j)
                               (setq reply-result (parse-structure-content j))
                               (when (eq (node-name j) 'pad) ;rename pad~
                                 (setq reply-result
                                       `((,(intern (format "pad~%d"
                                                           reply-pad-count))
                                          ,@(cdar reply-result))))
                                 (setq reply-pad-count (1+ reply-pad-count)))
                               reply-result)
                             reply-contents)))))
    (delq nil contents)
    (delq nil
          `((defclass ,name (xcb:-request) ,contents)
            ;; The optional expressions
            ,(when expressions
               `(cl-defmethod xcb:marshal ((obj ,name) connection) nil
                              ,@expressions
                              (cl-call-next-method obj connection)))
            ;; The optional reply body
            ,(when reply-name
               (delq nil reply-contents)
               `(defclass ,reply-name (xcb:-reply) ,reply-contents))))))

(defun parse-event (node)
  "Parse <event>.

The `no-sequence-number' is ignored here since it's only used for
KeymapNotify event; instead, we handle this case in `xcb:unmarshal'."
  (let ((name (intern (concat prefix (node-attr node 'name))))
        (event-number (string-to-int (node-attr node 'number)))
        (xge (node-attr node 'xge))
        (contents (node-subnodes node))
        result)
    (setq contents
          (apply 'nconc
                 (mapcar (lambda (i)
                           (setq result (parse-structure-content i))
                           (when (eq (node-name i) 'pad) ;rename pad~
                             (setq result
                                   `((,(intern (format "pad~%d" pad-count))
                                      ,@(cdar result))))
                             (setq pad-count (1+ pad-count)))
                           result)
                         contents)))
    (when xge                           ;generic event
      (setq contents
            (append
             '((extension :type xcb:CARD8)
               (length :type xcb:CARD32)
               (evtype :type xcb:CARD16))
             contents)))
    (setq event-alist (nconc event-alist `((,event-number . ,name))))
    `((defclass ,name (xcb:-event) ,contents))))

(defun parse-error (node)
  "Parse <error>."
  (let ((name (intern (concat prefix (node-attr node 'name))))
        (error-number (string-to-int (node-attr node 'number)))
        (contents (node-subnodes node))
        result)
    (setq contents
          (apply 'nconc
                (mapcar (lambda (i)
                          (setq result (parse-structure-content i))
                          (when (eq (node-name i) 'pad) ;rename pad~
                            (setq result
                                  `((,(intern (format "pad~%d" pad-count))
                                     ,@(cdar result))))
                            (setq pad-count (1+ pad-count)))
                          result)
                        contents)))
    (setq error-alist (nconc error-alist `((,error-number . ,name))))
    `((defclass ,name (xcb:-error) ,contents))))

(defun parse-eventcopy (node)
  "Parse <eventcopy>."
  (let* ((name (intern (concat prefix (node-attr node 'name))))
         (refname (node-attr node 'ref))
         (refname (or (intern-soft (concat "xcb:" refname))
                      (intern (concat prefix refname))))
         (event-number (string-to-int (node-attr node 'number))))
    (setq event-alist (nconc event-alist `((,event-number . ,name))))
    `((defclass ,name (xcb:-event ,refname) nil)))) ;shadow the method of ref

(defun parse-errorcopy (node)
  "Parse <errorcopy>."
  (let* ((name (intern (concat prefix (node-attr node 'name))))
         (refname (node-attr node 'ref))
         (refname (or (intern-soft (concat "xcb:" refname))
                      (intern (concat prefix refname))))
         (error-number (string-to-int (node-attr node 'number))))
    (setq error-alist (nconc error-alist `((,error-number . ,name))))
    `((defclass ,name (xcb:-error ,refname) nil)))) ;shadow the method of ref

;;;; XCB: structure contents

(defun parse-structure-content (node)
  "Parse a structure content."
  (pcase (node-name node)
    (`pad (parse-pad node))
    (`field (parse-field node))
    (`fd (parse-fd node))
    (`list (parse-list node))
    (`exprfield (parse-exprfield node))
    (`switch (parse-switch node))
    ((or `comment `doc))                ;simply ignored
    (x (error "Unsupported structure content: <%s>" x))))

;; The car of the result shall be renamed to prevent duplication of slot names
(defun parse-pad (node)
  "Parse <pad>."
  (let ((bytes (node-attr node 'bytes))
        (align (node-attr node 'align)))
    (if bytes
        `((pad~ :initform ,(string-to-int bytes) :type xcb:-pad))
      (if align
          `((pad~ :initform ,(string-to-int align) :type xcb:-pad-align))
        (error "Invalid <pad> field")))))

(defun parse-field (node)
  "Parse <field>."
  (let* ((name (intern (node-attr-escape node 'name)))
         (type (node-attr node 'type))
         (type (or (intern-soft (concat "xcb:" type)) ;extension or xproto
                   (intern (concat prefix type)))))
    `((,name :initarg ,(intern (concat ":" (symbol-name name))) :type ,type))))

(defun parse-fd (node)
  "Parse <fd>."
  (let ((name (intern (node-attr-escape node 'name))))
    `((,name :type xcb:-fd))))

(defun parse-list (node)
  "Parse <list>."
  (let* ((name (intern (node-attr-escape node 'name)))
         (name-alt (intern (concat (node-attr-escape node 'name) "~")))
         (type (node-attr node 'type))
         (type (or (intern-soft (concat "xcb:" type))
                   (intern (concat prefix type))))
         (size (parse-expression (node-subnode node))))
    `((,name :initarg ,(intern (concat ":" (symbol-name name)))
             :type xcb:-ignore)
      (,name-alt :initform '(name ,name type ,type size ,size)
                 :type xcb:-list))))

;; The car of result is the field declaration, and the cadr is the expression
;; to be evaluated.
(defun parse-exprfield (node)
  "Parse <exprfield>."
  (let* ((name (intern (node-attr-escape node 'name)))
         (type (node-attr node 'type))
         (type (or (intern-soft (concat "xcb:" type))
                   (intern (concat prefix type))))
         (value (parse-expression (node-subnode node))))
    `((,name :type ,type)
      (setf (slot-value obj ',name) ',value))))

;; The only difference between <bitcase> and <case> is whether the `condition'
;; is a list
;; The name attribute of <bitcase> and <case> seems not useful here.
(defun parse-switch (node)
  "Parse <switch>."
  (let ((name (intern (node-attr-escape node 'name)))
        (expression (parse-expression (car (node-subnodes node))))
        (cases (cdr (node-subnodes node)))
        fields fields-name)
    ;; Avoid duplicated slot names by appending "*" if necessary
    (let (names name)
      (dolist (case cases)
        (pcase (node-name case)
          ((or `bitcase `case)
           (dolist (field (node-subnodes case))
             (pcase (node-name field)
               ((or `enumref `pad `doc `comment))
               (_
                (setq name (node-attr field 'name))
                (when (member name names)
                  (while (member name names)
                    (setq name (concat name "*")))
                  (setcdr (assoc 'name (cadr field)) name))
                (cl-pushnew name names :test 'equal))))))))
    (setq cases
          (mapcar (lambda (i)
                    (let ((case-name (node-name i))
                          condition name-list tmp)
                      (when (or (eq case-name 'bitcase) (eq case-name 'case))
                        (dolist (j (node-subnodes i))
                          (pcase (node-name j)
                            (`enumref
                             (setq condition
                                   (nconc condition (list (parse-enumref j)))))
                            (x
                             (setq tmp (parse-structure-content j))
                             (when (eq x 'pad) ;rename pad~
                               (setq tmp
                                     `((,(intern (format "pad~%d" pad-count))
                                        ,@(cdar tmp))))
                               (setq pad-count (1+ pad-count)))
                             (setq fields (nconc fields tmp))
                             (setq name-list
                                   (nconc name-list (list (caar tmp)))))))
                        (when (eq case-name 'bitcase)
                          (setq condition (if (= 1 (length condition))
                                              (car condition)
                                            `(logior ,@condition)))))
                      `(,condition ,@name-list)))
                  cases))
    `((,name :initform '(expression ,expression cases ,cases)
             :type xcb:-switch)
      ,@fields)))

;;;; XCB: expressions

(defun parse-expression (node)
  "Parse an expression."
  (when node
    (pcase (node-name node)
      (`op (parse-op node))
      (`fieldref (parse-fieldref node))
      (`paramref (parse-paramref node))
      (`value (parse-value node))
      (`bit (parse-bit node))
      (`enumref (parse-enumref node))
      (`unop (parse-unop node))
      (`sumof (parse-sumof node))
      (`popcount (parse-popcount node))
      (`listelement-ref (parse-listelement-ref node))
      ((or `comment `doc))                ;simply ignored
      (x (error "Unsupported expression: <%s>" x)))))

(defun parse-op (node)
  "Parse <op>."
  (let* ((subnodes (node-subnodes node))
         (x (parse-expression (car subnodes)))
         (y (parse-expression (cadr subnodes))))
    (pcase (node-attr node 'op)
      ("+" `(+ ,x ,y))
      ("-" `(- ,x ,y))
      ("*" `(* ,x ,y))
      ("/" `(/ ,x ,y))
      ("&" `(logand ,x ,y))
      ("<<" `(lsh ,x ,y))
      (x (error "Unsupported operator: `%s'" x)))))

(defun parse-fieldref (node)
  "Parse <fieldref>."
  `(xcb:-fieldref ',(intern (escape-name (node-subnode node)))))

(defun parse-paramref (node)
  "Parse <paramref>."
  `(xcb:-paramref ',(intern (escape-name (node-subnode node)))))

(defun parse-value (node)
  "Parse <value>."
  (string-to-int (replace-regexp-in-string "^0x" "#x" (node-subnode node))))

(defun parse-bit (node)
  "Parse <bit>."
  (let ((bit (string-to-int (node-subnode node))))
    (cl-assert (and (<= 0 bit) (>= 31 bit)))
    (lsh 1 bit)))

(defun parse-enumref (node)
  "Parse <enumref>."
  (let ((name (concat (node-attr node 'ref) ":" (node-subnode node))))
    (or (intern-soft (concat "xcb:" name))
        (intern (concat prefix name)))))

(defun parse-unop (node)
  "Parse <unop>."
  (cl-assert (string= "~" (node-attr node 'op)))
  `(lognot (parse-expression (node-subnode node))))

(defun parse-sumof (node)
  "Parse <sumof>."
  (let* ((ref (intern (node-attr-escape node 'ref)))
         (expression (node-subnode node))
         (list-data `(slot-value obj ',ref)))
    (if (not expression)
        `(apply '+ ,list-data)
      (setq expression (parse-expression expression))
      `(apply '+ (mapcar (lambda (i)
                           (eval ',expression (list (nconc '(obj) i))))
                         ,list-data)))))

(defun parse-popcount (node)
  "Parse <popcount>."
  (let ((expression (parse-expression (node-subnode node))))
    `(xcb:-popcount ,expression)))

(defun parse-listelement-ref (node)
  "Parse <listelement-ref>."
  'obj)                      ;a list element is internally named 'obj'

;;;; The entry

(setq debug-on-error t)
(setq edebug-all-forms t)

(if (not argv)
    (error "Usage: el_client.el <protocol.xml> [additional_load_paths]")
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (concat default-directory "/../"))
  (dolist (i (cdr argv))
    (add-to-list 'load-path i))
  (require 'xcb-types)
  (parse (car argv)))

;;; el_client.el ends here
