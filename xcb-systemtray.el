;;; xcb-systemtray.el --- System tray protocol  -*- lexical-binding: t -*-

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

;; This library implements the system tray protocol.

;; Usage tips:
;; + Do not forget to call `xcb:systemtray:init' for _every_ connection using
;;   this library.
;; + Use `xcb:systemtray:SendEvent' instead of `xcb:SendEvent' to send opcode
;;   messages defined in this library.
;; + Initializing this library auto loads and initializes 'xcb-ewmh'.

;; References:
;; + System Tray Protocol (https://specifications.freedesktop.org/
;;   systemtray-spec/systemtray-spec-0.3.html)

;;; Code:

(require 'xcb-ewmh)

;; System tray atoms.
(eval-and-compile
  (defconst xcb:systemtray:-atoms   ;_NET_SYSTEM_TRAY_Sn are left out.
    '(_NET_SYSTEM_TRAY_OPCODE
      _NET_SYSTEM_TRAY_ORIENTATION
      _NET_SYSTEM_TRAY_VISUAL
      _NET_SYSTEM_TRAY_MESSAGE_DATA)
    "Atoms involved in the system tray protocol.")

  (dolist (atom xcb:systemtray:-atoms)
    (eval `(defvar ,(intern (concat "xcb:Atom:" (symbol-name atom))) nil))))

;; Opcodes.
(defconst xcb:systemtray:opcode:REQUEST-DOCK 0)
(defconst xcb:systemtray:opcode:BEGIN-MESSAGE 1)
(defconst xcb:systemtray:opcode:CANCEL-MESSAGE 2)

(cl-defmethod xcb:systemtray:init ((obj xcb:connection) &optional force)
  "Initialize the system tray module.

This method must be called before using any other method in this module."
  (when (or force (not xcb:Atom:_NET_SYSTEM_TRAY_OPCODE))
    (xcb:ewmh:init obj)                 ;required.
    (let ((atoms xcb:systemtray:-atoms))
      (dotimes (i (x-display-screens))
        (push (intern (format "_NET_SYSTEM_TRAY_S%d" i)) atoms))
      (xcb:icccm:intern-atoms obj atoms force))))

(defclass xcb:systemtray:SendEvent (xcb:SendEvent)
  ((propagate :initform 0)
   (event-mask :initform xcb:EventMask:NoEvent))
  :documentation "Send system tray opcode message.")

(defclass xcb:systemtray:-ClientMessage
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)
   (type :initform xcb:Atom:_NET_SYSTEM_TRAY_OPCODE)
   (time :initarg :time :type xcb:TIMESTAMP)   ;new slot
   (opcode :initarg :opcode :type xcb:CARD32)) ;new slot
  :documentation "An system tray opcode message.")

(defclass xcb:systemtray:REQUEST-DOCK (xcb:systemtray:-ClientMessage)
  ((opcode :initform xcb:systemtray:opcode:REQUEST-DOCK)
   (id :initarg :id :type xcb:CARD32)
   (pad~0 :initform 8 :type xcb:-pad))
  :documentation "Dock a tray icon.")

(defclass xcb:systemtray:BEGIN-MESSAGE (xcb:systemtray:-ClientMessage)
  ((opcode :initform xcb:systemtray:opcode:BEGIN-MESSAGE)
   (timeout :initarg :timeout :type xcb:TIMESTAMP)
   (length :initarg :length :type xcb:CARD32)
   (id :initarg :id :type xcb:CARD32))
  :documentation "Begin balloon message.")

(defclass xcb:systemtray:MESSAGE-DATA
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 8)
   (type :initform xcb:Atom:_NET_SYSTEM_TRAY_MESSAGE_DATA)
   (data~ :initform '(name data type xcb:CARD8 size 20) :type xcb:-list)
   (data :initarg :data :type xcb:-ignore)))

(defclass xcb:systemtray:CANCEL-MESSAGE (xcb:systemtray:-ClientMessage)
  ((opcode :initform xcb:systemtray:opcode:CANCEL-MESSAGE)
   (id :initarg :id :type xcb:CARD32)
   (pad~0 :initform 8 :type xcb:-pad))
  :documentation "Cancel balloon message.")

;; Value of _NET_SYSTEM_TRAY_ORIENTATION.
(defconst xcb:systemtray:ORIENTATION:HORZ 0)
(defconst xcb:systemtray:ORIENTATION:VERT 1)

(defclass xcb:xembed:get-_NET_SYSTEM_TRAY_ORIENTATION
  (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_SYSTEM_TRAY_ORIENTATION)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:xembed:get-_NET_SYSTEM_TRAY_ORIENTATION~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:xembed:set-_NET_SYSTEM_TRAY_ORIENTATION
  (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_SYSTEM_TRAY_ORIENTATION)
   (type :initform xcb:Atom:CARDINAL)
   (format :initform 32)))

(defclass xcb:xembed:get-_NET_SYSTEM_TRAY_VISUAL
  (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_SYSTEM_TRAY_VISUAL)
   (type :initform xcb:Atom:VISUALID)))
(defclass xcb:xembed:get-_NET_SYSTEM_TRAY_VISUAL~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:xembed:set-_NET_SYSTEM_TRAY_VISUAL
  (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_SYSTEM_TRAY_VISUAL)
   (type :initform xcb:Atom:VISUALID)
   (format :initform 32)))



(provide 'xcb-systemtray)

;;; xcb-systemtray.el ends here
