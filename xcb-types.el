;;; xcb-types.el --- Type definitions for XCB  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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

;; This library defines various data types frequently used in XELB.  Simple
;; types are defined with `cl-deftype' while others are defined as classes.
;; Basically, data types are used for converting objects to/from byte arrays
;; rather than for validation purpose.  Classes defined elsewhere should also
;; support `xcb:marshal' and `xcb:unmarshal' methods in order to be considered
;; a type.  Most classes defined here are direct or indirect subclasses of
;; `xcb:-struct', which has implemented the fundamental marshalling and
;; unmarshalling methods.  These classes again act as the superclasses for more
;; concrete ones.  You may use `eieio-browse' to easily get an overview of the
;; inheritance hierarchies of all classes defined.

;; Please pay special attention to the byte order adopted in your application.
;; The global variable `xcb:lsb' specifies the byte order at the time of
;; instantiating a class (e.g. via `make-instance').  You may let-bind it to
;; temporarily change the byte order locally.

;; Todo:
;; + The current implementation of `eieio-default-eval-maybe' only `eval's a
;;   certain type of forms.  If this is changed in the future, we will have to
;;   adapt our codes accordingly.
;; + <paramref> for `xcb:-marshal-field'?

;; References:
;; + X protocol (http://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.txt)

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'cl-generic)
(require 'eieio)
(require 'xcb-debug)

(define-minor-mode xcb:debug
  "Debug-logging enabled if non-nil"
  :global t)

(defmacro xcb:-log (&optional format-string &rest objects)
  "Emit a message prepending the name of the function being executed.

FORMAT-STRING is a string specifying the message to output, as in
`format'.  The OBJECTS arguments specify the substitutions."
  (unless format-string (setq format-string ""))
  `(when xcb:debug
     (xcb-debug:message ,(concat "%s:\t" format-string "\n")
                        (xcb-debug:compile-time-function-name)
                        ,@objects)
     nil))

;;;; Fix backward compatibility issues with Emacs 24

(eval-and-compile
  (when (< emacs-major-version 25)
    ;; Copied from Emacs 25 with documentation and comments stripped.
    ;; The version of `with-slots' in Emacs 24 is buggy and inefficient.
    (defmacro with-slots (spec-list object &rest body)
      (declare (indent 2) (debug (sexp sexp def-body)))
      (require 'cl-lib)
      (macroexp-let2 nil object object
        `(cl-symbol-macrolet
             ,(mapcar (lambda (entry)
                        (let ((var  (if (listp entry) (car entry) entry))
                              (slot (if (listp entry) (cadr entry) entry)))
                          (list var `(slot-value ,object ',slot))))
                      spec-list)
           ,@body)))))

;; Backport some functions to Emacs 24

(eval-and-compile
  (unless (fboundp 'eieio-class-slots)
    (defun eieio-class-slots (class)
      (let* ((tmp (get class 'eieio-class-definition))
             (names (aref tmp 5))
             (initforms (aref tmp 6))
             (types (aref tmp 8))
             result)
        (dotimes (i (length names))
          (setq result (nconc result (list (vector (elt names i)
                                                   (elt initforms i)
                                                   (elt types i))))))
        result))))

(eval-and-compile
  (unless (fboundp 'eieio-slot-descriptor-name)
    (defsubst eieio-slot-descriptor-name (slot) (aref slot 0))))

(eval-and-compile
  (unless (fboundp 'cl--slot-descriptor-initform)
    (defsubst cl--slot-descriptor-initform (slot) (aref slot 1))))

(eval-when-compile
  (unless (fboundp 'cl--slot-descriptor-type)
    (defsubst cl--slot-descriptor-type (slot) (aref slot 2))))

;;;; Utility functions

(defsubst xcb:-pack-u1 (value)
  "1 byte unsigned integer => byte array."
  (vector value))

(defsubst xcb:-pack-i1 (value)
  "1 byte signed integer => byte array."
  (xcb:-pack-u1 (if (>= value 0) value
                  (1+ (logand #xFF (lognot (- value)))))))

(defsubst xcb:-pack-u2 (value)
  "2 bytes unsigned integer => byte array (MSB first)."
  (vector (logand (lsh value -8) #xFF) (logand value #xFF)))

(defsubst xcb:-pack-u2-lsb (value)
  "2 bytes unsigned integer => byte array (LSB first)."
  (vector (logand value #xFF) (logand (lsh value -8) #xFF)))

(defsubst xcb:-pack-i2 (value)
  "2 bytes signed integer => byte array (MSB first)."
  (xcb:-pack-u2 (if (>= value 0) value
                  (1+ (logand #xFFFF (lognot (- value)))))))

(defsubst xcb:-pack-i2-lsb (value)
  "2 bytes signed integer => byte array (LSB first)."
  (xcb:-pack-u2-lsb (if (>= value 0) value
                      (1+ (logand #xFFFF (lognot (- value)))))))

;; Due to loss of significance of floating-point numbers, `xcb:-pack-u8' and
;; `xcb:-pack-u8-lsb' may return approximate results.
(eval-and-compile
  (if (/= 0 (lsh 1 32))
      ;; 64 bit
      (progn
        (defsubst xcb:-pack-u4 (value)
          "4 bytes unsigned integer => byte array (MSB first, 64-bit)."
          (vector (logand (lsh value -24) #xFF) (logand (lsh value -16) #xFF)
                  (logand (lsh value -8) #xFF) (logand value #xFF)))
        (defsubst xcb:-pack-u4-lsb (value)
          "4 byte unsigned integer => byte array (LSB first, 64-bit)."
          (vector (logand value #xFF)
                  (logand (lsh value -8) #xFF)
                  (logand (lsh value -16) #xFF)
                  (logand (lsh value -24) #xFF)))
        (defsubst xcb:-pack-u8 (value)
          "8 bytes unsigned integer => byte array (MSB first)."
          (if (integerp value)
              (vector (logand (lsh value -56) #xFF)
                      (logand (lsh value -48) #xFF)
                      (logand (lsh value -40) #xFF)
                      (logand (lsh value -32) #xFF)
                      (logand (lsh value -24) #xFF)
                      (logand (lsh value -16) #xFF)
                      (logand (lsh value -8) #xFF)
                      (logand value #xFF))
            (let* ((msdw (min 4294967295. (truncate value 4294967296.)))
                   (lsdw (min 4294967295.
                              (truncate (- value (* msdw 4294967296.0))))))
              (vector (logand (lsh msdw -24) #xFF) (logand (lsh msdw -16) #xFF)
                      (logand (lsh msdw -8) #xFF) (logand msdw #xFF)
                      (logand (lsh lsdw -24) #xFF) (logand (lsh lsdw -16) #xFF)
                      (logand (lsh lsdw -8) #xFF) (logand lsdw #xFF)))))
        (defsubst xcb:-pack-u8-lsb (value)
          "8 bytes unsigned integer => byte array (LSB first)."
          (if (integerp value)
              (vector (logand value #xFF)
                      (logand (lsh value -8) #xFF)
                      (logand (lsh value -16) #xFF)
                      (logand (lsh value -24) #xFF)
                      (logand (lsh value -32) #xFF)
                      (logand (lsh value -40) #xFF)
                      (logand (lsh value -48) #xFF)
                      (logand (lsh value -56) #xFF))
            (let* ((msdw (min 4294967295. (truncate value 4294967296.)))
                   (lsdw (min 4294967295.
                              (truncate (- value (* msdw 4294967296.0))))))
              (vector (logand lsdw #xFF) (logand (lsh lsdw -8) #xFF)
                      (logand (lsh lsdw -16) #xFF) (logand (lsh lsdw -24) #xFF)
                      (logand msdw #xFF)
                      (logand (lsh msdw -8) #xFF)
                      (logand (lsh msdw -16) #xFF)
                      (logand (lsh msdw -24) #xFF))))))
    ;; 32 bit (30-bit actually; large numbers are represented as float type)
    (defsubst xcb:-pack-u4 (value)
      "4 bytes unsigned integer => byte array (MSB first, 32-bit)."
      (if (integerp value)
          (vector (logand (lsh value -24) #xFF) (logand (lsh value -16) #xFF)
                  (logand (lsh value -8) #xFF) (logand value #xFF))
        (let* ((msw (truncate value #x10000))
               (lsw (truncate (- value (* msw 65536.0)))))
          (vector (logand (lsh msw -8) #xFF) (logand msw #xFF)
                  (logand (lsh lsw -8) #xFF) (logand lsw #xFF)))))
    (defsubst xcb:-pack-u4-lsb (value)
      "4 bytes unsigned integer => byte array (LSB first, 32-bit)."
      (if (integerp value)
          (vector (logand value #xFF) (logand (lsh value -8) #xFF)
                  (logand (lsh value -16) #xFF) (logand (lsh value -24) #xFF))
        (let* ((msw (truncate value #x10000))
               (lsw (truncate (- value (* msw 65536.0)))))
          (vector (logand lsw #xFF) (logand (lsh lsw -8) #xFF)
                  (logand msw #xFF) (logand (lsh msw -8) #xFF)))))
    (defsubst xcb:-pack-u8 (value)
      "8 bytes unsigned integer => byte array (MSB first, 32-bit)."
      (if (integerp value)
          (vector 0 0 0 0
                  (logand (lsh value -24) #xFF) (logand (lsh value -16) #xFF)
                  (logand (lsh value -8) #xFF) (logand value #xFF))
        (let* ((msw (min #xFFFF (truncate value 281474976710656.)))
               (w1 (min #xFFFF
                        (truncate (setq value
                                        (- value (* msw 281474976710656.0)))
                                  4294967296.)))
               (w2 (min #xFFFF
                        (truncate (setq value (- value (* w1 4294967296.0)))
                                  #x10000)))
               (lsw (min #xFFFF (truncate (- value (* w2 65536.0))))))
          (vector (logand (lsh msw -8) #xFF) (logand msw #xFF)
                  (logand (lsh w1 -8) #xFF) (logand w1 #xFF)
                  (logand (lsh w2 -8) #xFF) (logand w2 #xFF)
                  (logand (lsh lsw -8) #xFF) (logand lsw #xFF)))))
    (defsubst xcb:-pack-u8-lsb (value)
      "8 bytes unsigned integer => byte array (LSB first, 32-bit)."
      (if (integerp value)
          (vector (logand value #xFF) (logand (lsh value -8) #xFF)
                  (logand (lsh value -16) #xFF) (logand (lsh value -24) #xFF)
                  0 0 0 0)
        (let* ((msw (min #xFFFF (truncate value 281474976710656.)))
               (w1 (min #xFFFF
                        (truncate (setq value
                                        (- value (* msw 281474976710656.0)))
                                  4294967296.)))
               (w2 (min #xFFFF
                        (truncate (setq value (- value (* w1 4294967296.0)))
                                  #x10000)))
               (lsw (min #xFFFF (truncate (- value (* w2 65536.0))))))
          (vector (logand lsw #xFF) (logand (lsh lsw -8) #xFF)
                  (logand w2 #xFF) (logand (lsh w2 -8) #xFF)
                  (logand w1 #xFF) (logand (lsh w1 -8) #xFF)
                  (logand msw #xFF) (logand (lsh msw -8) #xFF)))))))

(defsubst xcb:-pack-i4 (value)
  "4 bytes signed integer => byte array (MSB first)."
  (xcb:-pack-u4 (if (>= value 0)
                    value
                  (+ value 4294967296.)))) ;treated as float for 32-bit

(defsubst xcb:-pack-i4-lsb (value)
  "4 bytes signed integer => byte array (LSB first)."
  (xcb:-pack-u4-lsb (if (>= value 0)
                        value
                      (+ value 4294967296.)))) ;treated as float for 32-bit

(defsubst xcb:-unpack-u1 (data offset)
  "Byte array => 1 byte unsigned integer."
  (aref data offset))

(defsubst xcb:-unpack-i1 (data offset)
  "Byte array => 1 byte signed integer."
  (let ((value (xcb:-unpack-u1 data offset)))
    (if (= 0 (logand #x80 value))
        value
      (- (logand #xFF (lognot (1- value)))))))

(defsubst xcb:-unpack-u2 (data offset)
  "Byte array => 2 bytes unsigned integer (MSB first)."
  (logior (lsh (aref data offset) 8) (aref data (1+ offset))))

(defsubst xcb:-unpack-u2-lsb (data offset)
  "Byte array => 2 bytes unsigned integer (LSB first)."
  (logior (aref data offset) (lsh (aref data (1+ offset)) 8)))

(defsubst xcb:-unpack-i2 (data offset)
  "Byte array => 2 bytes signed integer (MSB first)."
  (let ((value (xcb:-unpack-u2 data offset)))
    (if (= 0 (logand #x8000 value))
        value
      (- (logand #xFFFF (lognot (1- value)))))))

(defsubst xcb:-unpack-i2-lsb (data offset)
  "Byte array => 2 bytes signed integer (LSB first)."
  (let ((value (xcb:-unpack-u2-lsb data offset)))
    (if (= 0 (logand #x8000 value))
        value
      (- (logand #xFFFF (lognot (1- value)))))))

;; Due to loss of significance of floating-point numbers, `xcb:-unpack-u8' and
;; `xcb:-unpack-u8-lsb' may return approximate results.
(eval-and-compile
  (if (/= 0 (lsh 1 32))
      ;; 64-bit
      (progn
        (defsubst xcb:-unpack-u4 (data offset)
          "Byte array => 4 bytes unsigned integer (MSB first, 64-bit)."
          (logior (lsh (aref data offset) 24) (lsh (aref data (1+ offset)) 16)
                  (lsh (aref data (+ offset 2)) 8) (aref data (+ offset 3))))
        (defsubst xcb:-unpack-u4-lsb (data offset)
          "Byte array => 4 bytes unsigned integer (LSB first, 64-bit)."
          (logior (aref data offset) (lsh (aref data (1+ offset)) 8)
                  (lsh (aref data (+ offset 2)) 16)
                  (lsh (aref data (+ offset 3)) 24)))
        (defsubst xcb:-unpack-u8 (data offset)
          "Byte array => 8 bytes unsigned integer (MSB first)."
          (let ((msb (aref data offset)))
            (+ (if (> msb 31) (* msb 72057594037927936.0) (lsh msb 56))
               (logior (lsh (aref data (1+ offset)) 48)
                       (lsh (aref data (+ offset 2)) 40)
                       (lsh (aref data (+ offset 3)) 32)
                       (lsh (aref data (+ offset 4)) 24)
                       (lsh (aref data (+ offset 5)) 16)
                       (lsh (aref data (+ offset 6)) 8)
                       (aref data (+ offset 7))))))
        (defsubst xcb:-unpack-u8-lsb (data offset)
          "Byte array => 8 bytes unsigned integer (LSB first)."
          (let ((msb (aref data (+ offset 7))))
            (+ (if (> msb 31) (* msb 72057594037927936.0) (lsh msb 56))
               (logior (lsh (aref data (+ offset 6)) 48)
                       (lsh (aref data (+ offset 5)) 40)
                       (lsh (aref data (+ offset 4)) 32)
                       (lsh (aref data (+ offset 3)) 24)
                       (lsh (aref data (+ offset 2)) 16)
                       (lsh (aref data (1+ offset)) 8)
                       (aref data offset))))))
    ;; 32-bit (30-bit actually; large numbers are represented as float type)
    (defsubst xcb:-unpack-u4 (data offset)
      "Byte array => 4 bytes unsigned integer (MSB first, 32-bit)."
      (let ((msb (aref data offset)))
        (+ (if (> msb 31) (* msb 16777216.0) (lsh msb 24))
           (logior (lsh (aref data (1+ offset)) 16)
                   (lsh (aref data (+ offset 2)) 8)
                   (aref data (+ offset 3))))))
    (defsubst xcb:-unpack-u4-lsb (data offset)
      "Byte array => 4 bytes unsigned integer (LSB first, 32-bit)."
      (let ((msb (aref data (+ offset 3))))
        (+ (if (> msb 31) (* msb 16777216.0) (lsh msb 24))
           (logior (aref data offset)
                   (lsh (aref data (1+ offset)) 8)
                   (lsh (aref data (+ offset 2)) 16)))))
    (defsubst xcb:-unpack-u8 (data offset)
      "Byte array => 8 bytes unsigned integer (MSB first, 32-bit)."
      (+ (* (aref data offset) 72057594037927936.0)
         (* (aref data (1+ offset)) 281474976710656.0)
         (* (aref data (+ offset 2)) 1099511627776.0)
         (* (aref data (+ offset 3)) 4294967296.0)
         (* (aref data (+ offset 4)) 16777216.0)
         (logior (lsh (aref data (+ offset 5)) 16)
                 (lsh (aref data (+ offset 6)) 8)
                 (aref data (+ offset 7)))))
    (defsubst xcb:-unpack-u8-lsb (data offset)
      "Byte array => 8 bytes unsigned integer (LSB first, 32-bit)."
      (+ (* (aref data (+ offset 7)) 72057594037927936.0)
         (* (aref data (+ offset 6)) 281474976710656.0)
         (* (aref data (+ offset 5)) 1099511627776.0)
         (* (aref data (+ offset 4)) 4294967296.0)
         (* (aref data (+ offset 3)) 16777216.0)
         (logior (lsh (aref data (+ offset 2)) 16)
                 (lsh (aref data (1+ offset)) 8)
                 (aref data offset))))))

(defsubst xcb:-unpack-i4 (data offset)
  "Byte array => 4 bytes signed integer (MSB first)."
  (let ((value (xcb:-unpack-u4 data offset)))
    (if (< value 2147483648.)           ;treated as float for 32-bit
        value
      (- value 4294967296.))))          ;treated as float for 32-bit

(defsubst xcb:-unpack-i4-lsb (data offset)
  "Byte array => 4 bytes signed integer (LSB first)."
  (let ((value (xcb:-unpack-u4-lsb data offset)))
    (if (< value 2147483648.)           ;treated as float for 32-bit
        value
      (- value 4294967296.))))          ;treated as float for 32-bit

(defmacro xcb:-fieldref (field)
  "Evaluate a <fieldref> field."
  `(slot-value obj ,field))

(defmacro xcb:-paramref (field)
  "Evaluate a <paramref> field."
  `(slot-value ctx ,field))

(defsubst xcb:-popcount (mask)
  "Return the popcount of integer MASK."
  (apply #'+ (mapcar (lambda (i)
                       (logand (lsh mask i) 1))
                     ;; 32-bit number assumed (CARD32)
                     (eval-when-compile (number-sequence -31 0)))))

(defsubst xcb:-request-class->reply-class (request)
  "Return the reply class corresponding to the request class REQUEST."
  (intern-soft (concat (symbol-name request) "~reply")))

;;;; Basic types

;; typedef in C
(defmacro xcb:deftypealias (new-type old-type)
  "Define NEW-TYPE as an alias of type OLD-TYPE.

Also the fundamental type is stored in 'the xcb--typealias' variable
property (for internal use only)."
  `(progn
     ;; FIXME: `new-type' should probably just not be eval'd at all,
     ;; but that requires changing all callers not to quote their arg.
     (cl-deftype ,(eval new-type t) nil ,old-type)
     (put ,new-type 'xcb--typealias
          (or (get ,old-type 'xcb--typealias) ,old-type))))

;; 1/2/4 B signed/unsigned integer
(cl-deftype xcb:-i1 () t)
(cl-deftype xcb:-i2 () t)
(cl-deftype xcb:-i4 () t)
(cl-deftype xcb:-u1 () t)
(cl-deftype xcb:-u2 () t)
(cl-deftype xcb:-u4 () t)
;; 8 B unsigned integer
(cl-deftype xcb:-u8 () t)
;; <pad>
(cl-deftype xcb:-pad () t)
;; <pad> with align attribute
(cl-deftype xcb:-pad-align () t)
;; <fd>
(xcb:deftypealias 'xcb:fd 'xcb:-i4)
;; <list>
(cl-deftype xcb:-list () t)
;; <switch>
(cl-deftype xcb:-switch () t)
;; This type of data is not involved in marshalling/unmarshalling
(cl-deftype xcb:-ignore () t)
;; C types and types missing in XCB
(cl-deftype xcb:void () t)
(xcb:deftypealias 'xcb:char 'xcb:-u1)
(xcb:deftypealias 'xcb:BYTE 'xcb:-u1)
(xcb:deftypealias 'xcb:INT8 'xcb:-i1)
(xcb:deftypealias 'xcb:INT16 'xcb:-i2)
(xcb:deftypealias 'xcb:INT32 'xcb:-i4)
(xcb:deftypealias 'xcb:CARD8 'xcb:-u1)
(xcb:deftypealias 'xcb:CARD16 'xcb:-u2)
(xcb:deftypealias 'xcb:CARD32 'xcb:-u4)
(xcb:deftypealias 'xcb:CARD64 'xcb:-u8)
(xcb:deftypealias 'xcb:BOOL 'xcb:-u1)

;;;; Struct type

(eval-and-compile
  (defvar xcb:lsb t
    "Non-nil for LSB first (i.e., little-endian), nil otherwise.

Consider let-bind it rather than change its global value."))

(defclass xcb:--struct ()
  nil)

(cl-defmethod slot-unbound ((object xcb:--struct) class slot-name fn)
  (unless (eq fn #'oref-default)
    (xcb:-log "unbound-slot: %s" (list (eieio-class-name class)
                                       (eieio-object-name object)
			               slot-name fn))))

(defclass xcb:-struct (xcb:--struct)
  ((~lsb :initarg :~lsb
         :initform (symbol-value 'xcb:lsb) ;see `eieio-default-eval-maybe'
         :type xcb:-ignore))
  :documentation "Struct type.")

(cl-defmethod xcb:marshal ((obj xcb:-struct))
  "Return the byte-array representation of struct OBJ."
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        result name type value)
    (catch 'break
      (dolist (slot slots)
        (setq type (cl--slot-descriptor-type slot))
        (unless (eq type 'xcb:-ignore)
          (setq name (eieio-slot-descriptor-name slot))
          (setq value (slot-value obj name))
          (when (symbolp value)        ;see `eieio-default-eval-maybe'
            (setq value (symbol-value value)))
          (setq result
                (vconcat result (xcb:-marshal-field obj type value
                                                    (length result))))
          (when (eq type 'xcb:-switch) ;xcb:-switch always finishes a struct
            (throw 'break 'nil)))))
    result))

(cl-defmethod xcb:-marshal-field ((obj xcb:-struct) type value &optional pos)
  "Return the byte-array representation of a field in struct OBJ of type TYPE
and value VALUE.

The optional POS argument indicates current byte index of the field (used by
`xcb:-pad-align' type)."
  (pcase (or (get type 'xcb--typealias) type)
    (`xcb:-u1 (xcb:-pack-u1 value))
    (`xcb:-i1 (xcb:-pack-i1 value))
    (`xcb:-u2
     (if (slot-value obj '~lsb) (xcb:-pack-u2-lsb value) (xcb:-pack-u2 value)))
    (`xcb:-i2
     (if (slot-value obj '~lsb) (xcb:-pack-i2-lsb value) (xcb:-pack-i2 value)))
    (`xcb:-u4
     (if (slot-value obj '~lsb) (xcb:-pack-u4-lsb value) (xcb:-pack-u4 value)))
    (`xcb:-i4
     (if (slot-value obj '~lsb) (xcb:-pack-i4-lsb value) (xcb:-pack-i4 value)))
    (`xcb:-u8
     (if (slot-value obj '~lsb) (xcb:-pack-u8-lsb value) (xcb:-pack-u8 value)))
    (`xcb:void (vector value))
    (`xcb:-pad
     (unless (integerp value)
       (setq value (eval value `((obj . ,obj)))))
     (make-vector value 0))
    (`xcb:-pad-align
     ;; The length slot in xcb:-request is left out
     (let ((len (if (object-of-class-p obj 'xcb:-request) (+ pos 2) pos)))
       (when (vectorp value)
         ;; Alignment with offset.
         (setq len (- len (aref value 1))
               value (aref value 0)))
       (unless (integerp value)
         (setq value (eval value `((obj . ,obj)))))
       (make-vector (% (- value (% len value)) value) 0)))
    (`xcb:-list
     (let* ((list-name (plist-get value 'name))
            (list-type (plist-get value 'type))
            (list-size (plist-get value 'size))
            (data (slot-value obj list-name)))
       (unless (integerp list-size)
         (setq list-size (eval list-size `((obj . ,obj))))
         (unless list-size
           (setq list-size (length data)))) ;list-size can be nil
       (cl-assert (= list-size (length data)))
       (mapconcat (lambda (i) (xcb:-marshal-field obj list-type i)) data [])))
    (`xcb:-switch
     (let ((slots (eieio-class-slots (eieio-object-class obj)))
           (expression (plist-get value 'expression))
           (cases (plist-get value 'cases))
           result condition name-list flag slot-type)
       (unless (integerp expression)
         (setq expression (eval expression `((obj . ,obj)))))
       (cl-assert (integerp expression))
       (dolist (i cases)
         (setq condition (car i))
         (setq name-list (cdr i))
         (setq flag nil)
         (cl-assert (or (integerp condition) (listp condition)))
         (if (integerp condition)
             (setq flag (/= 0 (logand expression condition)))
           (if (eq 'logior (car condition))
               (setq flag (/= 0 (logand expression
                                        (apply #'logior (cdr condition)))))
             (setq flag (memq expression condition))))
         (when flag
           (dolist (name name-list)
             (catch 'break
               (dolist (slot slots) ;better way to find the slot type?
                 (when (eq name (eieio-slot-descriptor-name slot))
                   (setq slot-type (cl--slot-descriptor-type slot))
                   (throw 'break nil))))
             (unless (eq slot-type 'xcb:-ignore)
               (setq result
                     (vconcat result
                              (xcb:-marshal-field obj slot-type
                                                  (slot-value obj name)
                                                  (+ pos
                                                     (length result)))))))))
       result))
    ((guard (child-of-class-p type 'xcb:-struct))
     (xcb:marshal value))
    (x (error "[XCB] Unsupported type for marshalling: %s" x))))

(cl-defmethod xcb:unmarshal ((obj xcb:-struct) byte-array &optional ctx
                             total-length)
  "Fill in fields of struct OBJ according to its byte-array representation.

The optional argument CTX is for <paramref>."
  (unless total-length
    (setq total-length (length byte-array)))
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        (result 0)
        slot-name tmp type)
    (catch 'break
      (dolist (slot slots)
        (setq type (cl--slot-descriptor-type slot))
        (unless (eq type 'xcb:-ignore)
          (setq slot-name (eieio-slot-descriptor-name slot)
                tmp (xcb:-unmarshal-field obj type byte-array 0
                                          (eieio-oref-default obj slot-name)
                                          ctx total-length))
          (setf (slot-value obj slot-name) (car tmp))
          (setq byte-array (substring byte-array (cadr tmp)))
          (setq result (+ result (cadr tmp)))
          (when (eq type 'xcb:-switch) ;xcb:-switch always finishes a struct
            (throw 'break 'nil)))))
    result))

(cl-defmethod xcb:-unmarshal-field ((obj xcb:-struct) type data offset
                                    initform &optional ctx total-length)
  "Return the value of a field in struct OBJ of type TYPE, byte-array
representation DATA, and default value INITFORM.

The optional argument CTX is for <paramref>.

This method returns a list of two components, with the first being the result
and the second the consumed length."
  (pcase (or (get type 'xcb--typealias) type)
    (`xcb:-u1 (list (aref data offset) 1))
    (`xcb:-i1 (let ((result (aref data offset)))
                (list (if (< result 128) result (- result 255)) 1)))
    (`xcb:-u2 (list (if (slot-value obj '~lsb)
                        (xcb:-unpack-u2-lsb data offset)
                      (xcb:-unpack-u2 data offset))
                    2))
    (`xcb:-i2 (list (if (slot-value obj '~lsb)
                        (xcb:-unpack-i2-lsb data offset)
                      (xcb:-unpack-i2 data offset))
                    2))
    (`xcb:-u4 (list (if (slot-value obj '~lsb)
                        (xcb:-unpack-u4-lsb data offset)
                      (xcb:-unpack-u4 data offset))
                    4))
    (`xcb:-i4 (list (if (slot-value obj '~lsb)
                        (xcb:-unpack-i4-lsb data offset)
                      (xcb:-unpack-i4 data offset))
                    4))
    (`xcb:-u8 (list (if (slot-value obj '~lsb)
                        (xcb:-unpack-u8-lsb data offset)
                      (xcb:-unpack-u8 data offset))
                    8))
    (`xcb:void (list (aref data offset) 1))
    (`xcb:-pad
     (unless (integerp initform)
       (when (eq 'quote (car initform))
         (setq initform (cadr initform)))
       (setq initform (eval initform `((obj . ,obj) (ctx . ,ctx)))))
     (list initform initform))
    (`xcb:-pad-align
     (let ((len (- total-length (- (length data) offset))))
       (if (vectorp initform)
           ;; Alignment with offset.
           (setq len (- len (aref initform 1))
                 initform (aref initform 0))
         (unless (integerp initform)
           (when (eq 'quote (car initform))
             (setq initform (cadr initform)))
           (setq initform (eval initform `((obj . ,obj) (ctx . ,ctx))))))
       (list initform (% (- initform (% len initform)) initform))))
    (`xcb:-list
     (when (eq 'quote (car initform))   ;unquote the form
       (setq initform (cadr initform)))
     (let ((list-name (plist-get initform 'name))
           (list-type (plist-get initform 'type))
           (list-size (plist-get initform 'size)))
       (unless (integerp list-size)
         (setq list-size (eval list-size `((obj . ,obj) (ctx . ,ctx)))))
       (cl-assert (integerp list-size))
       (pcase list-type
         (`xcb:char                     ;as Latin-1 encoded string
          (setf (slot-value obj list-name)
                (decode-coding-string
                 (apply #'unibyte-string
                        (append (substring data offset
                                           (+ offset list-size))
                                nil))
                 'iso-latin-1)))
         (`xcb:void                     ;for further unmarshalling
          (setf (slot-value obj list-name)
                (substring data offset (+ offset list-size))))
         (x
          (let ((count 0)
                result tmp)
            (dotimes (_ list-size)
              (setq tmp (xcb:-unmarshal-field obj x data (+ offset count) nil
                                              nil total-length))
              (setq result (nconc result (list (car tmp))))
              (setq count (+ count (cadr tmp))))
            (setf (slot-value obj list-name) result)
            (setq list-size count))))   ;to byte length
       (list initform list-size)))
    (`xcb:-switch
     (let ((slots (eieio-class-slots (eieio-object-class obj)))
           (expression (plist-get initform 'expression))
           (cases (plist-get initform 'cases))
           (count 0)
           condition name-list flag slot-type tmp)
       (unless (integerp expression)
         (setq expression (eval expression `((obj . ,obj) (ctx . ,ctx)))))
       (cl-assert (integerp expression))
       (dolist (i cases)
         (setq condition (car i))
         (setq name-list (cdr i))
         (setq flag nil)
         (cl-assert (or (integerp condition) (listp condition)))
         (if (integerp condition)
             (setq flag (/= 0 (logand expression condition)))
           (if (eq 'logior (car condition))
               (setq flag (/= 0 (logand expression
                                        (apply #'logior (cdr condition)))))
             (setq flag (memq expression condition))))
         (when flag
           (dolist (name name-list)
             (catch 'break
               (dolist (slot slots) ;better way to find the slot type?
                 (when (eq name (eieio-slot-descriptor-name slot))
                   (setq slot-type (cl--slot-descriptor-type slot))
                   (throw 'break nil))))
             (unless (eq slot-type 'xcb:-ignore)
               (setq tmp (xcb:-unmarshal-field obj slot-type data offset
                                               (eieio-oref-default obj name)
                                               nil total-length))
               (setf (slot-value obj name) (car tmp))
               (setq count (+ count (cadr tmp)))
               (setq data (substring data (cadr tmp)))))))
       (list initform count)))
    ((and x (guard (child-of-class-p x 'xcb:-struct)))
     (let* ((struct-obj (make-instance x))
            (tmp (xcb:unmarshal struct-obj (substring data offset) obj
                                total-length)))
       (list struct-obj tmp)))
    (x (error "[XCB] Unsupported type for unmarshalling: %s" x))))

;;;; Types derived directly from `xcb:-struct'

(defclass xcb:-request (xcb:-struct)
  nil
  :documentation "X request type.")

(defclass xcb:-reply (xcb:-struct)
  ((~reply :initform 1 :type xcb:-u1))
  :documentation "X reply type.")

(defclass xcb:-event (xcb:-struct)
  ((~code :type xcb:-u1))
  :documentation "Event type.")
;; Implemented in 'xcb.el'
(cl-defgeneric xcb:-error-or-event-class->number ((obj xcb:connection) class))
;;
(cl-defmethod xcb:marshal ((obj xcb:-event) connection &optional sequence)
  "Return the byte-array representation of event OBJ.

This method is mainly designed for `xcb:SendEvent', where it's used to
generate synthetic events.  The CONNECTION argument is used to retrieve
the event number of extensions.  If SEQUENCE is non-nil, it is used as
the sequence number of the synthetic event (if the event uses sequence
number); otherwise, 0 is assumed.

This method auto-pads short results to 32 bytes."
  (let ((event-number
         (xcb:-error-or-event-class->number connection
                                            (eieio-object-class obj)))
        result)
    (when (consp event-number)
      (setq event-number (cdr event-number))
      (if (= 1 (length event-number))
          ;; XKB event.
          (setf (slot-value obj 'xkbType) (aref event-number 0))
        ;; Generic event.
        (setf (slot-value obj 'extensions) (aref event-number 0)
              (slot-value obj 'evtype) (aref event-number 1))))
    (when (slot-exists-p obj '~sequence)
      (setf (slot-value obj '~sequence) (or sequence 0)))
    (setq result (cl-call-next-method obj))
    (when (> 32 (length result))
      (setq result (vconcat result (make-vector (- 32 (length result)) 0))))
    result))

(defclass xcb:-generic-event (xcb:-event)
  ((~code :initform 35)
   (~extension :type xcb:CARD8)
   (~sequence :type xcb:CARD16)
   (~length :type xcb:CARD32)
   (~evtype :type xcb:CARD16))
  :documentation "Generic event type.")

(defclass xcb:-error (xcb:-struct)
  ((~error :initform 0 :type xcb:-u1)
   (~code :type xcb:-u1)
   (~sequence :type xcb:CARD16))
  :documentation "X error type.")

(defclass xcb:-union (xcb:-struct)
  ((~size :initarg :~size :type xcb:-ignore)) ;Size of the largest member.
  :documentation "Union type.")
;;
(cl-defmethod slot-unbound ((_object xcb:-union) _class _slot-name _fn)
  nil)
;;
(cl-defmethod xcb:marshal ((obj xcb:-union))
  "Return the byte-array representation of union OBJ.

This result is converted from the first bounded slot."
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        (size (slot-value obj '~size))
        result slot type name tmp)
    (while (and (not result) slots (> size (length result)))
      (setq slot (pop slots))
      (setq type (cl--slot-descriptor-type slot)
            name (eieio-slot-descriptor-name slot))
      (unless (or (not (slot-value obj name))
                  (eq type 'xcb:-ignore)
                  ;; Dealing with `xcb:-list' type
                  (and (eq type 'xcb:-list)
                       (not (slot-value obj (plist-get (slot-value obj name)
                                                       'name)))))
        (setq tmp (xcb:-marshal-field obj (cl--slot-descriptor-type slot)
                                      (slot-value obj name)))
        (when (> (length tmp) (length result))
          (setq result tmp))))
    (when (> size (length result))
      (setq result (vconcat result (make-vector (- size (length result)) 0))))
    result))
;;
(cl-defmethod xcb:unmarshal ((obj xcb:-union) byte-array &optional ctx
                             total-length)
  "Fill in every field in union OBJ, according to BYTE-ARRAY.

The optional argument CTX is for <paramref>."
  (unless total-length
    (setq total-length (length byte-array)))
  (let ((slots (eieio-class-slots (eieio-object-class obj)))
        slot-name tmp type)
    (dolist (slot slots)
      (setq type (cl--slot-descriptor-type slot))
      (unless (eq type 'xcb:-ignore)
        (setq slot-name (eieio-slot-descriptor-name slot)
              tmp (xcb:-unmarshal-field obj type byte-array 0
                                        (eieio-oref-default obj slot-name)
                                        ctx total-length))
        (setf (slot-value obj (eieio-slot-descriptor-name slot)) (car tmp))))
    (slot-value obj '~size)))



(provide 'xcb-types)

;;; xcb-types.el ends here
