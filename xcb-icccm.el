;;; xcb-icccm.el --- Inter-Client Communication  -*- lexical-binding: t -*-
;;;                  Conventions Manual

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

;; This library implements ICCCM the same way as xcb/util-wm.

;; Usage tips:
;; + Do not forget to call `xcb:icccm:init' for _every_ connection using
;;   this library.
;; + Use `xcb:icccm:SendEvent' instead of `xcb:SendEvent' to send client
;;   messages defined in this library.

;; Todo:
;; + Interned atoms are actually connection-dependent.  Currently they are
;;   simply saved as global variables.

;; References:
;; + ICCCM (http://www.x.org/releases/X11R7.7/doc/xorg-docs/icccm/icccm.txt)
;; + xcb/util-wm (git://anongit.freedesktop.org/xcb/util-wm)

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'xcb)

;;;; ICCCM atoms

(eval-and-compile
  (defconst xcb:icccm:-atoms
    '(UTF8_STRING COMPOUND_TEXT TEXT C_STRING MANAGER
                  WM_PROTOCOLS WM_TAKE_FOCUS WM_DELETE_WINDOW
                  WM_STATE WM_CHANGE_STATE)
    "Atoms involved in ICCCM.")

  (dolist (atom xcb:icccm:-atoms)
    (eval `(defvar ,(intern (concat "xcb:Atom:" (symbol-name atom))) nil))))

(cl-defmethod xcb:icccm:init ((obj xcb:connection) &optional force)
  "Initialize ICCCM module.

This method must be called before using any other method in this module."
  (when (or force (not xcb:Atom:WM_PROTOCOLS))
    (xcb:icccm:intern-atoms obj xcb:icccm:-atoms force)))

(cl-defmethod xcb:icccm:intern-atoms ((obj xcb:connection) atoms
                                      &optional force)
  "Intern the X atoms listed in the list AOTMS.

The value of these atoms will be available in `xcb:Atom' namespace."
  (dolist (atom atoms)
    (let* ((name (symbol-name atom))
           (var-name (intern (concat "xcb:Atom:" name))))
      (when (or force (not (and (boundp var-name) (symbol-value var-name))))
        (set var-name
             (slot-value (xcb:+request-unchecked+reply obj
                             (make-instance 'xcb:InternAtom
                                            :only-if-exists 0
                                            :name-len (length name)
                                            :name name))
                         'atom))))))

;;;; Client message

(defclass xcb:icccm:SendEvent (xcb:SendEvent)
  ((propagate :initform 0)
   (event-mask :initform xcb:EventMask:NoEvent))
  :documentation "A fork of `xcb:SendEvent' to send ICCCM client messages.")

(defclass xcb:icccm:--ClientMessage ()
  ((data :type xcb:-ignore))            ;shadowed slot
  :documentation "To shadow the data slot in `xcb:ClientMessage'.")
;;
(defclass xcb:icccm:-ClientMessage
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)
   (type :initform xcb:Atom:WM_PROTOCOLS)
   (protocol :type xcb:ATOM)                 ;new slot
   (time :initarg :time :type xcb:TIMESTAMP) ;new slot
   (pad~0 :initform 12 :type xcb:-pad))      ;new slot
  :documentation "An ICCCM client message with data slot replaced by
protocol and time.")

(defclass xcb:icccm:WM_DELETE_WINDOW (xcb:icccm:-ClientMessage)
  ((protocol :initform xcb:Atom:WM_DELETE_WINDOW)
   (time :initform xcb:Time:CurrentTime))
  :documentation "Delete a window using the WM_DELETE_WINDOW client message.")

(defclass xcb:icccm:WM_TAKE_FOCUS (xcb:icccm:-ClientMessage)
  ((protocol :initform xcb:Atom:WM_TAKE_FOCUS))
  :documentation "Set a focus using the WM_TAKE_FOCUS client message.

A valid timestamp (rather than `xcb:Time:CurrentTime') must be supplied.")

;;;; Abstract classes for getting/changing (plain) list properties

(defclass xcb:icccm:-GetProperty (xcb:GetProperty)
  ((delete :initform 0)
   (long-offset :initform 0)
   (long-length :initform 1000000000))  ;as long as possible
  :documentation "Get an ICCCM property (request part).")

(defclass xcb:icccm:-GetProperty~reply (xcb:GetProperty~reply)
  nil
  :documentation "Get an ICCCM property (reply part).")
;;
(cl-defmethod xcb:unmarshal ((obj xcb:icccm:-GetProperty~reply) byte-array)
  "Fill in the fields in the reply of ICCCM GetProperty request OBJ
according to BYTE-ARRAY.

This method automatically format the value as 8, 16 or 32 bits array."
  (let ((retval (cl-call-next-method obj byte-array))
        tmp)
    (with-slots (~lsb length format value-len value) obj
      (if (or (= 0 value-len) (= 0 length))
          (setf value nil)              ;no available value
        (setq tmp value                 ;long-offset is always 0
              value nil)
        (pcase format
          (8
           (cl-assert (= value-len (length tmp)))
           (setf value tmp))
          (16
           (cl-assert (= (* 2 value-len) (length tmp)))
           (if ~lsb
               (dotimes (_ value-len)
                 (setf value (vconcat value
                                      (vector (xcb:-unpack-u2-lsb tmp 0))))
                 (setq tmp (substring tmp 2)))
             (dotimes (_ value-len)
               (setf value (vconcat value
                                    (vector (xcb:-unpack-u2 tmp 0))))
               (setq tmp (substring tmp 2)))))
          (32
           (cl-assert (= (* 4 value-len) (length tmp)))
           (if ~lsb
               (dotimes (_ value-len)
                 (setf value (vconcat value
                                      (vector (xcb:-unpack-u4-lsb tmp 0))))
                 (setq tmp (substring tmp 4)))
             (dotimes (_ value-len)
               (setf value (vconcat value (vector (xcb:-unpack-u4 tmp 0))))
               (setq tmp (substring tmp 4)))))
          (_ (cl-assert nil)))))
    retval))

(defclass xcb:icccm:-ChangeProperty (xcb:ChangeProperty)
  ((mode :initform xcb:PropMode:Replace)
   (format :initform 32)
   (data :initform nil))
  :documentation "Change an ICCCM property.")
;;
(cl-defmethod xcb:marshal ((obj xcb:icccm:-ChangeProperty))
  "Return the byte-array representation of an ICCCM ChangeProperty request.

This method automatically sets the data length."
  (with-slots (~lsb format data-len data) obj
    (setf data-len (length data))
    (setf data
          (pcase format
            (8 data)
            (16 (mapconcat (lambda (i) (if ~lsb (xcb:-pack-u2-lsb i)
                                         (xcb:-pack-u2 i)))
                           data []))
            (32 (mapconcat (lambda (i) (if ~lsb (xcb:-pack-u4-lsb i)
                                         (xcb:-pack-u4 i)))
                           data []))
            (_ (cl-assert nil)))))
  (cl-call-next-method obj))

;;;; Abstract classes for getting/changing text properties

(defclass xcb:icccm:-GetProperty-text (xcb:icccm:-GetProperty)
  ((type :initform xcb:GetPropertyType:Any))
  :documentation "Get an ICCCM text property (request part).")

(defclass xcb:icccm:-GetProperty-text~reply (xcb:icccm:-GetProperty~reply)
  nil
  :documentation "Get an ICCCM text property (reply part).")
;;
(cl-defmethod xcb:unmarshal ((obj xcb:icccm:-GetProperty-text~reply)
                             byte-array)
  "Fill in the fields in the reply of ICCCM GetProperty (text) request OBJ
according to BYTE-ARRAY.

This method automatically decodes the value (as string)."
  (let* ((retval (cl-call-next-method obj byte-array)))
    (with-slots (format type value) obj
      (when (symbolp type) (setq type (symbol-value type)))
      (when (and value (= format 8))
        (setf value
              (decode-coding-string
               (apply #'unibyte-string (append value nil))
               (cond ((= type xcb:Atom:UTF8_STRING) 'utf-8)
                     ((= type xcb:Atom:STRING) 'iso-latin-1)
                     ((= type xcb:Atom:COMPOUND_TEXT)
                      'compound-text-with-extensions)
                     ((or (eq type xcb:Atom:TEXT) (eq type xcb:Atom:C_STRING))
                      'no-conversion)
                     (t (error "[XELB:ICCCM] Unsupported encoding: %d"
                               type)))))))
    retval))

(defclass xcb:icccm:-ChangeProperty-text (xcb:icccm:-ChangeProperty)
  ((type :initform xcb:Atom:STRING)     ;may be changed
   (format :initform 8))
  :documentation "Change an ICCCM text property.")
;;
(cl-defmethod xcb:marshal ((obj xcb:icccm:-ChangeProperty-text))
  "Return the byte-array representation of an ICCCM ChangeProperty (text)
request OBJ.

This method automatically encodes the data (which is a string)."
  (with-slots (type data) obj
    (when (symbolp type) (setq type (symbol-value type)))
    (setf data
          (vconcat
           (encode-coding-string
            data
            (cond ((= type xcb:Atom:UTF8_STRING) 'utf-8)
                  ((= type xcb:Atom:STRING) 'iso-latin-1)
                  ((= type xcb:Atom:COMPOUND_TEXT)
                   'compound-text-with-extensions)
                  ((or (eq type xcb:Atom:TEXT) (eq type xcb:Atom:C_STRING))
                   'no-conversion)
                  (t (error "[XELB:ICCCM] Unsupported encoding: %d" type)))))))
  (cl-call-next-method obj))

;;;; Abstract classes for getting/changing single field properties

(defclass xcb:icccm:-GetProperty-single (xcb:icccm:-GetProperty)
  nil
  :documentation "Get an ICCCM single-valued property (request part).")

(defclass xcb:icccm:-GetProperty-single~reply (xcb:icccm:-GetProperty~reply)
  nil
  :documentation "Get a single-valued ICCCM property (reply part).")
;;
(cl-defmethod xcb:unmarshal ((obj xcb:icccm:-GetProperty-single~reply)
                             byte-array)
  "Fill in the fields in the reply of an ICCCM GetProperty (single-valued)
request OBJ according to BYTE-ARRAY."
  (let ((retval (cl-call-next-method obj byte-array)))
    (with-slots (value) obj
      (when value
        (cl-assert (= 1 (length value)))
        (setf value (elt value 0))))
    retval))

(defclass xcb:icccm:-ChangeProperty-single (xcb:icccm:-ChangeProperty)
  nil
  :documentation "Change a single-valued ICCCM property.")
;;
(cl-defmethod xcb:marshal ((obj xcb:icccm:-ChangeProperty-single))
  "Return the byte-array representation of a single-valued ICCCM ChangeProperty
request OBJ."
  (with-slots (data) obj
    (setf data `[,data]))
  (cl-call-next-method obj))

;;;; Abstract classes for getting/changing property with explicit fields

(defclass xcb:icccm:-GetProperty-explicit (xcb:icccm:-GetProperty)
  nil
  :documentation "Get an ICCCM property whose fields are explicitly listed out
(request part).")

(defclass xcb:icccm:-GetProperty-explicit~reply (xcb:icccm:-GetProperty~reply)
  nil
  :documentation "Get an ICCCM property whose fields are explicitly listed out
(reply part).")
;;
(cl-defmethod xcb:unmarshal ((obj xcb:icccm:-GetProperty-explicit~reply)
                             byte-array)
  "Fill in the reply of an ICCCM GetProperty request whose fields are
explicitly listed out."
  (let* ((retval (cl-call-next-method obj byte-array))
         (slots-orig (eieio-class-slots 'xcb:icccm:-GetProperty~reply))
         (slots (eieio-class-slots (eieio-object-class obj)))
         (slots (nthcdr (length slots-orig) slots))
         (value (slot-value obj 'value)))
    (unless value (setq value (make-vector (length slots) nil))) ;fallback
    ;; Set explicit fields from value field
    (dotimes (i (length value))
      (setf (slot-value obj (eieio-slot-descriptor-name (elt slots i)))
            (elt value i)))
    retval))

(defclass xcb:icccm:-ChangeProperty-explicit (xcb:icccm:-ChangeProperty)
  ((format :initform 32))
  :documentation "Change an ICCCM property whose fields are explicitly listed
out.")
;;
(cl-defmethod xcb:marshal ((obj xcb:icccm:-ChangeProperty-explicit))
  "Return the byte-array representation of an ICCCM ChangeProperty request
whose fields are explicitly listed out."
  (let* ((slots-orig (eieio-class-slots 'xcb:icccm:-ChangeProperty))
         (slots (eieio-class-slots (eieio-object-class obj)))
         (slots (nthcdr (length slots-orig) slots)))
    ;; Set data field from explicit fields
    (setf (slot-value obj 'data)
          (mapconcat (lambda (slot)
                       (list (slot-value obj
                                         (eieio-slot-descriptor-name slot))))
                     slots []))
    (cl-call-next-method obj)))

;;;; Client Properties

;; WM_NAME
(defclass xcb:icccm:get-WM_NAME (xcb:icccm:-GetProperty-text)
  ((property :initform xcb:Atom:WM_NAME)))
(defclass xcb:icccm:get-WM_NAME~reply (xcb:icccm:-GetProperty-text~reply)
  nil)
(defclass xcb:icccm:set-WM_NAME (xcb:icccm:-ChangeProperty-text)
  ((property :initform xcb:Atom:WM_NAME)))

;; WM_ICON_NAME
(defclass xcb:icccm:get-WM_ICON_NAME (xcb:icccm:-GetProperty-text)
  ((property :initform xcb:Atom:WM_ICON_NAME)))
(defclass xcb:icccm:get-WM_ICON_NAME~reply (xcb:icccm:-GetProperty-text~reply)
  nil)
(defclass xcb:icccm:set-WM_ICON_NAME (xcb:icccm:-ChangeProperty-text)
  ((property :initform xcb:Atom:WM_ICON_NAME)))

;; WM_SIZE_HINTS
(defconst xcb:icccm:WM_SIZE_HINTS:USPosition  1)
(defconst xcb:icccm:WM_SIZE_HINTS:USSize 2)
(defconst xcb:icccm:WM_SIZE_HINTS:PPosition 4)
(defconst xcb:icccm:WM_SIZE_HINTS:PSize 8)
(defconst xcb:icccm:WM_SIZE_HINTS:PMinSize 16)
(defconst xcb:icccm:WM_SIZE_HINTS:PMaxSize 32)
(defconst xcb:icccm:WM_SIZE_HINTS:PResizeInc 64)
(defconst xcb:icccm:WM_SIZE_HINTS:PAspect 128)
(defconst xcb:icccm:WM_SIZE_HINTS:PBaseSize 256)
(defconst xcb:icccm:WM_SIZE_HINTS:PWinGravity 512)
;;
(defclass xcb:icccm:-WM_SIZE_HINTS ()
  ((flags :initarg :flags :initform 0 :type xcb:-ignore)
   (x :initarg :x :initform 0 :type xcb:-ignore)
   (y :initarg :y :initform 0 :type xcb:-ignore)
   (width :initarg :width :initform 0 :type xcb:-ignore)
   (height :initarg :height :initform 0 :type xcb:-ignore)
   (min-width :initarg :min-width :initform 0 :type xcb:-ignore)
   (min-height :initarg :min-height :initform 0 :type xcb:-ignore)
   (max-width :initarg :max-width :initform 0 :type xcb:-ignore)
   (max-height :initarg :max-height :initform 0 :type xcb:-ignore)
   (width-inc :initarg :width-inc :initform 0 :type xcb:-ignore)
   (height-inc :initarg :height-inc :initform 0 :type xcb:-ignore)
   (min-aspect-num :initarg :min-aspect-num :initform 0 :type xcb:-ignore)
   (min-aspect-den :initarg :min-aspect-den :initform 0 :type xcb:-ignore)
   (max-aspect-num :initarg :max-aspect-num :initform 0 :type xcb:-ignore)
   (max-aspect-den :initarg :max-aspect-den :initform 0 :type xcb:-ignore)
   (base-width :initarg :base-width :initform 0 :type xcb:-ignore)
   (base-height :initarg :base-height :initform 0 :type xcb:-ignore)
   (win-gravity :initarg :win-gravity :initform 0 :type xcb:-ignore)))
;;
(defclass xcb:icccm:get-WM_SIZE_HINTS (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:WM_SIZE_HINTS)
   (type :initform xcb:Atom:WM_SIZE_HINTS)
   (long-length :initform 18)))         ;fixed
(defclass xcb:icccm:get-WM_SIZE_HINTS~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:icccm:-WM_SIZE_HINTS)
  nil)
(defclass xcb:icccm:set-WM_SIZE_HINTS
  (xcb:icccm:-ChangeProperty-explicit xcb:icccm:-WM_SIZE_HINTS)
  ((property :initform xcb:Atom:WM_SIZE_HINTS)
   (type :initform xcb:Atom:WM_SIZE_HINTS)))

;; WM_NORMAL_HINTS
(defclass xcb:icccm:get-WM_NORMAL_HINTS (xcb:icccm:get-WM_SIZE_HINTS)
  ((property :initform xcb:Atom:WM_NORMAL_HINTS)))
(defclass xcb:icccm:get-WM_NORMAL_HINTS~reply
  (xcb:icccm:get-WM_SIZE_HINTS~reply)
  nil)
(defclass xcb:icccm:set-WM_NORMAL_HINTS (xcb:icccm:set-WM_SIZE_HINTS)
  ((property :initform xcb:Atom:WM_NORMAL_HINTS)))

;; WM_HINTS
(defconst xcb:icccm:WM_HINTS:InputHint 1)
(defconst xcb:icccm:WM_HINTS:StateHint 2)
(defconst xcb:icccm:WM_HINTS:IconPixmapHint 4)
(defconst xcb:icccm:WM_HINTS:IconWindowHint 8)
(defconst xcb:icccm:WM_HINTS:IconPositionHint 16)
(defconst xcb:icccm:WM_HINTS:IconMaskHint 32)
(defconst xcb:icccm:WM_HINTS:WindowGroupHint 64)
(defconst xcb:icccm:WM_HINTS:MessageHint 128)
(defconst xcb:icccm:WM_HINTS:UrgencyHint 256)
;;
(defclass xcb:icccm:-WM_HINTS ()
  ((flags :initarg :flags :initform 0 :type xcb:-ignore)
   (input :initarg :input :initform 0 :type xcb:-ignore)
   (initial-state :initarg :initial-state :initform 0 :type xcb:-ignore)
   (icon-pixmap :initarg :icon-pixmap :initform 0 :type xcb:-ignore)
   (icon-window :initarg :icon-window :initform 0 :type xcb:-ignore)
   (icon-x :initarg :icon-x :initform 0 :type xcb:-ignore)
   (icon-y :initarg :icon-y :initform 0 :type xcb:-ignore)
   (icon-mask :initarg :icon-mask :initform 0 :type xcb:-ignore)
   (window-group :initarg :window-group :initform 0 :type xcb:-ignore)))
;;
(defclass xcb:icccm:get-WM_HINTS (xcb:icccm:-GetProperty-explicit)
  ;; (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:WM_HINTS)
   (type :initform xcb:Atom:WM_HINTS)
   (long-length :initform 9)))          ;fixed
(defclass xcb:icccm:get-WM_HINTS~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:icccm:-WM_HINTS)
  nil)
(defclass xcb:icccm:set-WM_HINTS
  (xcb:icccm:-ChangeProperty-explicit xcb:icccm:-WM_HINTS)
  ((property :initform xcb:Atom:WM_HINTS)
   (type :initform xcb:Atom:WM_HINTS)))

;; WM_CLASS
(defclass xcb:icccm:get-WM_CLASS (xcb:icccm:-GetProperty-text)
  ((property :initform xcb:Atom:WM_CLASS)
   (type :initform xcb:Atom:STRING)))
(defclass xcb:icccm:get-WM_CLASS~reply (xcb:icccm:-GetProperty-text~reply)
  ((instance-name :type xcb:-ignore)
   (class-name :type xcb:-ignore)))
;;
(cl-defmethod xcb:unmarshal ((obj xcb:icccm:get-WM_CLASS~reply) byte-array)
  ;; Split value into instance & class names
  (let* ((retval (cl-call-next-method obj byte-array))
         (tmp (slot-value obj 'value))
         (tmp (if tmp (split-string tmp "\0" t) '(nil nil))))
    (with-slots (instance-name class-name) obj
      (setf instance-name (car tmp)
            class-name (cadr tmp)))
    retval))
;;
(defclass xcb:icccm:set-WM_CLASS (xcb:icccm:-ChangeProperty-text)
  ((property :initform xcb:Atom:WM_CLASS)
   (type :initform xcb:Atom:STRING)
   (instance-name :initarg :instance-name :type xcb:-ignore)
   (class-name :initarg :class-name :type xcb:-ignore)))
;;
(cl-defmethod xcb:marshal ((obj xcb:icccm:set-WM_CLASS))
  (with-slots (data instance-name class-name) obj
    (setf data (concat instance-name "\0" class-name "\0")))
  (cl-call-next-method obj))

;; WM_TRANSIENT_FOR
(defclass xcb:icccm:get-WM_TRANSIENT_FOR (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:WM_TRANSIENT_FOR)
   (type :initform xcb:Atom:WINDOW)
   (long-length :initform 1)))
(defclass xcb:icccm:get-WM_TRANSIENT_FOR~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:icccm:set-WM_TRANSIENT_FOR (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:WM_TRANSIENT_FOR)))

;; WM_PROTOCOLS
(defclass xcb:icccm:get-WM_PROTOCOLS (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:WM_PROTOCOLS)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:icccm:get-WM_PROTOCOLS~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:icccm:set-WM_PROTOCOLS (xcb:icccm:-ChangeProperty)
  ((type :initform xcb:Atom:ATOM)
   (format :initform 32)))

;; WM_COLORMAP_WINDOWS
(defclass xcb:icccm:get-WM_COLORMAP_WINDOWS (xcb:icccm:-GetProperty)
  ((type :initform xcb:Atom:WINDOW)))
(defclass xcb:icccm:get-WM_COLORMAP_WINDOWS~reply
  (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:icccm:set-WM_COLORMAP_WINDOWS (xcb:icccm:-ChangeProperty)
  ((type :initform xcb:Atom:WINDOW)
   (format :initform 32)))

;; WM_CLIENT_MACHINE
(defclass xcb:icccm:get-WM_CLIENT_MACHINE (xcb:icccm:-GetProperty-text)
  ((property :initform xcb:Atom:WM_CLIENT_MACHINE)))
(defclass xcb:icccm:get-WM_CLIENT_MACHINE~reply
  (xcb:icccm:-GetProperty-text~reply)
  nil)
(defclass xcb:icccm:set-WM_CLIENT_MACHINE (xcb:icccm:-ChangeProperty-text)
  ((property :initform xcb:Atom:WM_CLIENT_MACHINE)))

;;;; Window Manager Properties

;; WM_STATE
(defconst xcb:icccm:WM_STATE:WithdrawnState 0)
(defconst xcb:icccm:WM_STATE:NormalState 1)
(defconst xcb:icccm:WM_STATE:IconicState 3)
;;
(defclass xcb:icccm:-WM_STATE ()
  ((state :initarg :state :type xcb:-ignore)
   (icon :initarg :icon :type xcb:-ignore)))
;;
(defclass xcb:icccm:get-WM_STATE (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:WM_STATE)
   (type :initform xcb:Atom:WM_STATE)
   (long-length :initform 2)))
(defclass xcb:icccm:get-WM_STATE~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:icccm:-WM_STATE)
  nil)
(defclass xcb:icccm:set-WM_STATE
  (xcb:icccm:-ChangeProperty-explicit xcb:icccm:-WM_STATE)
  ((property :initform xcb:Atom:WM_STATE)
   (type :initform xcb:Atom:WM_STATE)))

;; WM_ICON_SIZE
(defclass xcb:icccm:-WM_ICON_SIZE ()
  ((min-width :initarg :min-width :type xcb:-ignore)
   (min-height :initarg :min-height :type xcb:-ignore)
   (max-width :initarg :max-width :type xcb:-ignore)
   (max-height :initarg :max-height :type xcb:-ignore)
   (width-inc :initarg :width-inc :type xcb:-ignore)
   (height-inc :initarg :height-inc :type xcb:-ignore)))
;;
(defclass xcb:icccm:get-WM_ICON_SIZE (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:WM_ICON_SIZE)
   (type :initform xcb:Atom:WM_ICON_SIZE)
   (long-length :initform 6)))
(defclass xcb:icccm:get-WM_ICON_SIZE~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:icccm:-WM_ICON_SIZE)
  nil)
(defclass xcb:icccm:set-WM_ICON_SIZE
  (xcb:icccm:-ChangeProperty-explicit xcb:icccm:-WM_ICON_SIZE)
  ((property :initform xcb:Atom:WM_ICON_SIZE)
   (type :initform xcb:Atom:WM_ICON_SIZE)))



(provide 'xcb-icccm)

;;; xcb-icccm.el ends here
