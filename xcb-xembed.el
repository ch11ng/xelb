;;; xcb-xembed.el --- XEmbed protocol -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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

;; This library implements the XEmbed protocol.

;; Usage tips:
;; + Do not forget to call `xcb:xembed:init' for _every_ connection using this
;;   library.
;; + Use `xcb:xembed:SendEvent' instead of `xcb:SendEvent' to send XEmbed
;;   messages defined in this library.

;; References:
;; + Xembed protocol (https://specifications.freedesktop.org/
;;   xembed-spec/xembed-spec-0.5.html)

;;; Code:

(require 'xcb-icccm)

;; XEmbed atoms
(eval-and-compile
  (defconst xcb:xembed:-atoms
    '(_XEMBED_INFO _XEMBED)
    "XEmbed atoms.")

  (dolist (atom xcb:xembed:-atoms)
    (eval `(defvar ,(intern (concat "xcb:Atom:" (symbol-name atom))) nil))))

;; XEMBED message opcodes.
(defconst xcb:xembed:opcode:EMBEDDED-NOTIFY        0)
(defconst xcb:xembed:opcode:WINDOW-ACTIVATE        1)
(defconst xcb:xembed:opcode:WINDOW-DEACTIVATE      2)
(defconst xcb:xembed:opcode:REQUEST-FOCUS          3)
(defconst xcb:xembed:opcode:FOCUS-IN               4)
(defconst xcb:xembed:opcode:FOCUS-OUT              5)
(defconst xcb:xembed:opcode:FOCUS-NEXT             6)
(defconst xcb:xembed:opcode:FOCUS-PREV             7)
(defconst xcb:xembed:opcode:MODALITY-ON            10)
(defconst xcb:xembed:opcode:MODALITY-OFF           11)
(defconst xcb:xembed:opcode:REGISTER-ACCELERATOR   12)
(defconst xcb:xembed:opcode:UNREGISTER-ACCELERATOR 13)
(defconst xcb:xembed:opcode:ACTIVATE-ACCELERATOR   14)

(cl-defmethod xcb:xembed:init ((obj xcb:connection) &optional force)
  "Initialize the XEmbed module.

This method must be called before using any other method in this module."
  (when (or force (not xcb:Atom:_XEMBED_INFO))
    (xcb:icccm:intern-atoms obj xcb:xembed:-atoms force)))

;; Flags for _XEMBED_INFO.
(defconst xcb:xembed:MAPPED 1)

(defclass xcb:xembed:get-_XEMBED_INFO (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_XEMBED_INFO)
   (type :initform xcb:Atom:_XEMBED_INFO)))
(defclass xcb:xembed:get-_XEMBED_INFO~reply
  (xcb:icccm:-GetProperty-explicit~reply)
  ((version :type xcb:-ignore)
   (flags :type xcb:-ignore)))
(defclass xcb:xembed:set-_XEMBED_INFO (xcb:icccm:-ChangeProperty-explicit)
  ((property :initform xcb:Atom:_XEMBED_INFO)
   (type :initform xcb:Atom:_XEMBED_INFO)
   (format :initform 32)
   (version :initarg :version :type xcb:-ignore)
   (flags :initarg :flags :type xcb:-ignore)))

(defclass xcb:xembed:SendEvent (xcb:SendEvent)
  ((propagate :initform 0)
   (event-mask :initform xcb:EventMask:NoEvent))
  :documentation "Send XEmbed message.")

(defclass xcb:xembed:-ClientMessage
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)
   (type :initform xcb:Atom:_XEMBED)
   (time :initarg :time :type xcb:TIMESTAMP)               ;new slot
   (opcode :initarg :opcode :type xcb:CARD32)              ;new slot
   (detail :initarg :detail :initform 0 :type xcb:CARD32)) ;new slot
  :documentation "An XEmbed client message.")

(defclass xcb:xembed:EMBEDDED-NOTIFY (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:EMBEDDED-NOTIFY)
   (embedder :initarg :embedder :type xcb:WINDOW)
   (version :initarg :version :type xcb:CARD32)))

(defclass xcb:xembed:WINDOW-ACTIVATE (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:WINDOW-ACTIVATE)
   (pad~0 :initform 8 :type xcb:-pad)))

(defclass xcb:xembed:WINDOW-DEACTIVATE (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:WINDOW-DEACTIVATE)
   (pad~0 :initform 8 :type xcb:-pad)))

(defclass xcb:xembed:REQUEST-FOCUS (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:REQUEST-FOCUS)
   (pad~0 :initform 8 :type xcb:-pad)))

;; Details for xcb:xembed:FOCUS-IN.
(defconst xcb:xembed:FOCUS:CURRENT 0)
(defconst xcb:xembed:FOCUS:FIRST   1)
(defconst xcb:xembed:FOCUS:LAST    2)

;; Directions for focusing.
(defconst xcb:xembed:DIRECTION:DEFAULT    0)
(defconst xcb:xembed:DIRECTION:UP-DOWN    1)
(defconst xcb:xembed:DIRECTION:LEFT-RIGHT 2)

(defclass xcb:xembed:FOCUS-IN (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:FOCUS-IN)
   (direction :initarg :direction :initform 0 :type xcb:CARD32)
   (pad~0 :initform 4 :type xcb:-pad)))

(defclass xcb:xembed:FOCUS-OUT (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:FOCUS-OUT)
   (pad~0 :initform 8 :type xcb:-pad)))

(defclass xcb:xembed:FOCUS-NEXT (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:FOCUS-NEXT)
   (direction :initarg :direction :initform 0 :type xcb:CARD32)
   (pad~0 :initform 4 :type xcb:-pad)))

(defclass xcb:xembed:FOCUS-PREV (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:FOCUS-PREV)
   (direction :initarg :direction :initform 0 :type xcb:CARD32)
   (pad~0 :initform 4 :type xcb:-pad)))

;; Modifiers field for xcb:xembed:REGISTER-ACCELERATOR.
(defconst xcb:xembed:MODIFIER:SHIFT   1)
(defconst xcb:xembed:MODIFIER:CONTROL 2)
(defconst xcb:xembed:MODIFIER:ALT     4)
(defconst xcb:xembed:MODIFIER:SUPER   8)
(defconst xcb:xembed:MODIFIER:HYPER   16)

(defclass xcb:xembed:REGISTER-ACCELERATOR (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:REGISTER-ACCELERATOR)
   (keysym :initarg :keysym :type xcb:KEYSYM)
   (modifier :initarg :modifier :type xcb:CARD32)))

(defclass xcb:xembed:UNREGISTER-ACCELERATOR (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:UNREGISTER-ACCELERATOR)
   (pad~0 :initform 8 :type xcb:-pad)))

;; Flags for XEMBED-ACTIVATE-ACCELERATOR.
(defconst xcb:xembed:ACCELERATOR:OVERLOADED 1)

(defclass xcb:xembed:ACTIVATE-ACCELERATOR (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:ACTIVATE-ACCELERATOR)
   (flags :initarg :flags :type xcb:CARD32)
   (pad~0 :initform 4 :type xcb:-pad)))

(defclass xcb:xembed:MODALITY-ON (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:MODALITY-ON)
   (pad~0 :initform 8 :type xcb:-pad)))

(defclass xcb:xembed:MODALITY-OFF (xcb:xembed:-ClientMessage)
  ((opcode :initform xcb:xembed:opcode:MODALITY-OFF)
   (pad~0 :initform 8 :type xcb:-pad)))



(provide 'xcb-xembed)

;;; xcb-xembed.el ends here
