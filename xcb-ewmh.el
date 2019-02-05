;;; xcb-ewmh.el --- Extended Window Manager Hints  -*- lexical-binding: t -*-

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

;; This library implements EWMH the same way as xcb/util-wm.

;; Usage tips:
;; + Do not forget to call `xcb:ewmh:init' for _every_ connection using
;;   this library.
;; + Use `xcb:ewmh:SendEvent' instead of `xcb:SendEvent' to send client
;;   messages defined in this library.
;; + Initializing this library auto loads and initializes 'xcb-icccm'.

;; References:
;; + EWMH (http://standards.freedesktop.org/wm-spec/wm-spec-latest.html)
;; + xcb/util-wm (git://anongit.freedesktop.org/xcb/util-wm)

;;; Code:

(require 'xcb)
(require 'xcb-icccm)

;;;; EWMH Atoms

(eval-and-compile
  (defconst xcb:ewmh:-atoms
    '( ;; Root Window Properties (and Related Messages)
      _NET_SUPPORTED
      _NET_CLIENT_LIST
      _NET_CLIENT_LIST_STACKING
      _NET_NUMBER_OF_DESKTOPS
      _NET_DESKTOP_GEOMETRY
      _NET_DESKTOP_VIEWPORT
      _NET_CURRENT_DESKTOP
      _NET_DESKTOP_NAMES
      _NET_ACTIVE_WINDOW
      _NET_WORKAREA
      _NET_SUPPORTING_WM_CHECK
      _NET_VIRTUAL_ROOTS
      _NET_DESKTOP_LAYOUT
      _NET_SHOWING_DESKTOP
      ;; Other Root Window Messages
      _NET_CLOSE_WINDOW
      _NET_MOVERESIZE_WINDOW
      _NET_WM_MOVERESIZE
      _NET_RESTACK_WINDOW
      _NET_REQUEST_FRAME_EXTENTS
      ;; Application Window Properties
      _NET_WM_NAME
      _NET_WM_VISIBLE_NAME
      _NET_WM_ICON_NAME
      _NET_WM_VISIBLE_ICON_NAME
      _NET_WM_DESKTOP
      _NET_WM_WINDOW_TYPE
      _NET_WM_STATE
      _NET_WM_ALLOWED_ACTIONS
      _NET_WM_STRUT
      _NET_WM_STRUT_PARTIAL
      _NET_WM_ICON_GEOMETRY
      _NET_WM_ICON
      _NET_WM_PID
      _NET_WM_HANDLED_ICONS
      _NET_WM_USER_TIME
      _NET_WM_USER_TIME_WINDOW
      _NET_FRAME_EXTENTS
      _NET_WM_OPAQUE_REGION
      _NET_WM_BYPASS_COMPOSITOR
      ;; Window Manager Protocols
      _NET_WM_PING
      _NET_WM_SYNC_REQUEST
      _NET_WM_SYNC_REQUEST_COUNTER
      _NET_WM_FULLSCREEN_MONITORS
      ;; Other Properties
      _NET_WM_FULL_PLACEMENT
      _NET_WM_CM_S0  ;_NET_WM_CM_Sn (n = 1, 2, ...) are left out here.
      ;; _NET_WM_WINDOW_TYPE hint
      _NET_WM_WINDOW_TYPE_DESKTOP
      _NET_WM_WINDOW_TYPE_DOCK
      _NET_WM_WINDOW_TYPE_TOOLBAR
      _NET_WM_WINDOW_TYPE_MENU
      _NET_WM_WINDOW_TYPE_UTILITY
      _NET_WM_WINDOW_TYPE_SPLASH
      _NET_WM_WINDOW_TYPE_DIALOG
      _NET_WM_WINDOW_TYPE_DROPDOWN_MENU
      _NET_WM_WINDOW_TYPE_POPUP_MENU
      _NET_WM_WINDOW_TYPE_TOOLTIP
      _NET_WM_WINDOW_TYPE_NOTIFICATION
      _NET_WM_WINDOW_TYPE_COMBO
      _NET_WM_WINDOW_TYPE_DND
      _NET_WM_WINDOW_TYPE_NORMAL
      ;; _NET_WM_STATE hint
      _NET_WM_STATE_MODAL
      _NET_WM_STATE_STICKY
      _NET_WM_STATE_MAXIMIZED_VERT
      _NET_WM_STATE_MAXIMIZED_HORZ
      _NET_WM_STATE_SHADED
      _NET_WM_STATE_SKIP_TASKBAR
      _NET_WM_STATE_SKIP_PAGER
      _NET_WM_STATE_HIDDEN
      _NET_WM_STATE_FULLSCREEN
      _NET_WM_STATE_ABOVE
      _NET_WM_STATE_BELOW
      _NET_WM_STATE_DEMANDS_ATTENTION
      _NET_WM_STATE_FOCUSED
      ;; _NET_WM_ACTION hint
      _NET_WM_ACTION_MOVE
      _NET_WM_ACTION_RESIZE
      _NET_WM_ACTION_MINIMIZE
      _NET_WM_ACTION_SHADE
      _NET_WM_ACTION_STICK
      _NET_WM_ACTION_MAXIMIZE_HORZ
      _NET_WM_ACTION_MAXIMIZE_VERT
      _NET_WM_ACTION_FULLSCREEN
      _NET_WM_ACTION_CHANGE_DESKTOP
      _NET_WM_ACTION_CLOSE
      _NET_WM_ACTION_ABOVE
      _NET_WM_ACTION_BELOW)
    "EWMH atoms.")

  (dolist (atom xcb:ewmh:-atoms)
    (eval `(defvar ,(intern (concat "xcb:Atom:" (symbol-name atom))) nil))))

(cl-defmethod xcb:ewmh:init ((obj xcb:connection) &optional force)
  "Initialize EWMH module.

This method must be called before using any other method in this module.

This method also initializes ICCCM module automatically."
  (when (or force (not xcb:Atom:_NET_SUPPORTED))
    (xcb:icccm:init obj)                ;required
    (let ((atoms xcb:ewmh:-atoms))
      (dotimes (i (1- (x-display-screens)))
        (push (intern (format "_NET_WM_CM_S%d" (1+ i))) atoms))
      (xcb:icccm:intern-atoms obj atoms force))))

;;;; Client message

(defclass xcb:ewmh:SendEvent (xcb:SendEvent)
  ((propagate :initform 0)
   (event-mask :initform (logior xcb:EventMask:SubstructureNotify
                                 xcb:EventMask:SubstructureRedirect)))
  :documentation "A fork of `xcb:SendEvent' to send EWMH client message.

Note that this only applies to \"sending a message to the root window\" in
EWMH")

(defclass xcb:ewmh:-ClientMessage (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)))

;;;; Abstract classes for getting/changing (UTF-8) string properties

(defclass xcb:ewmh:-GetProperty-utf8 (xcb:icccm:-GetProperty-text)
  ((type :initform xcb:Atom:UTF8_STRING))
  :documentation "Get an EWMH UTF-8 text property (request part).")
(defclass xcb:ewmh:-GetProperty-utf8~reply (xcb:icccm:-GetProperty-text~reply)
  nil
  :documentation "Get an EWMH UTF-8 text property (reply part).")
(defclass xcb:ewmh:-ChangeProperty-utf8 (xcb:icccm:-ChangeProperty-text)
  ((type :initform xcb:Atom:UTF8_STRING))
  :documentation "Change an EWMH UTF-8 text property.")

;;;; Root Window Properties (and Related Messages)

;; _NET_SUPPORTED
(defclass xcb:ewmh:get-_NET_SUPPORTED (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_SUPPORTED)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:ewmh:get-_NET_SUPPORTED~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_SUPPORTED (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_SUPPORTED)
   (type :initform xcb:Atom:ATOM)))

;; _NET_CLIENT_LIST
(defclass xcb:ewmh:get-_NET_CLIENT_LIST (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_CLIENT_LIST)
   (type :initform xcb:Atom:WINDOW)))
(defclass xcb:ewmh:get-_NET_CLIENT_LIST~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_CLIENT_LIST (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_CLIENT_LIST)
   (type :initform xcb:Atom:WINDOW)))

;; _NET_CLIENT_LIST_STACKING
(defclass xcb:ewmh:get-_NET_CLIENT_LIST_STACKING (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_CLIENT_LIST_STACKING)
   (type :initform xcb:Atom:WINDOW)))
(defclass xcb:ewmh:get-_NET_CLIENT_LIST_STACKING~reply
  (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_CLIENT_LIST_STACKING (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_CLIENT_LIST_STACKING)
   (type :initform xcb:Atom:WINDOW)))

;; _NET_NUMBER_OF_DESKTOPS
(defclass xcb:ewmh:get-_NET_NUMBER_OF_DESKTOPS (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_NUMBER_OF_DESKTOPS)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_NUMBER_OF_DESKTOPS~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_NUMBER_OF_DESKTOPS
  (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_NUMBER_OF_DESKTOPS)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_DESKTOP_GEOMETRY
(defclass xcb:ewmh:-_NET_DESKTOP_GEOMETRY (xcb:--struct)
  ((width :initarg :width :type xcb:-ignore)
   (height :initarg :height :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_DESKTOP_GEOMETRY (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_DESKTOP_GEOMETRY)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 2)))
(defclass xcb:ewmh:get-_NET_DESKTOP_GEOMETRY~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_DESKTOP_GEOMETRY)
  nil)
(defclass xcb:ewmh:set-_NET_DESKTOP_GEOMETRY
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_DESKTOP_GEOMETRY)
  ((property :initform xcb:Atom:_NET_DESKTOP_GEOMETRY)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:_NET_DESKTOP_GEOMETRY
  (xcb:ewmh:-ClientMessage xcb:ewmh:-_NET_DESKTOP_GEOMETRY)
  ((type :initform xcb:Atom:_NET_DESKTOP_GEOMETRY)))

;; _NET_DESKTOP_VIEWPORT
(defclass xcb:ewmh:get-_NET_DESKTOP_VIEWPORT (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_DESKTOP_VIEWPORT)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_DESKTOP_VIEWPORT~reply
  (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_DESKTOP_VIEWPORT (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_DESKTOP_VIEWPORT)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:_NET_DESKTOP_VIEWPORT (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_DESKTOP_VIEWPORT)
   (new-vx :initarg :new-vx :type xcb:CARD32)
   (new-vy :initarg :new-vy :type xcb:CARD32)))

;; _NET_CURRENT_DESKTOP
(defclass xcb:ewmh:get-_NET_CURRENT_DESKTOP (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_CURRENT_DESKTOP)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_CURRENT_DESKTOP~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_CURRENT_DESKTOP (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_CURRENT_DESKTOP)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:_NET_CURRENT_DESKTOP (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_CURRENT_DESKTOP)
   (new-index :initarg :new-index :type xcb:CARD32)
   (timestamp :initarg :timestamp :type xcb:CARD32)))

;; _NET_DESKTOP_NAMES
(defclass xcb:ewmh:get-_NET_DESKTOP_NAMES (xcb:ewmh:-GetProperty-utf8)
  ((property :initform xcb:Atom:_NET_DESKTOP_NAMES)))
(defclass xcb:ewmh:get-_NET_DESKTOP_NAMES~reply
  (xcb:ewmh:-GetProperty-utf8~reply)
  nil)
(defclass xcb:ewmh:set-_NET_DESKTOP_NAMES (xcb:ewmh:-ChangeProperty-utf8)
  ((property :initform xcb:Atom:_NET_DESKTOP_NAMES)))

;; _NET_ACTIVE_WINDOW
(defclass xcb:ewmh:get-_NET_ACTIVE_WINDOW (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_ACTIVE_WINDOW)
   (type :initform xcb:Atom:WINDOW)))
(defclass xcb:ewmh:get-_NET_ACTIVE_WINDOW~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_ACTIVE_WINDOW (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_ACTIVE_WINDOW)
   (type :initform xcb:Atom:WINDOW)))
(defclass xcb:ewmh:_NET_ACTIVE_WINDOW (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_ACTIVE_WINDOW)
   (source-indication :initarg :source-indication :type xcb:CARD32)
   (timestamp :initarg :timestamp :type xcb:CARD32)
   (current-active-window :initarg :current-active-window :type xcb:WINDOW)))

;; _NET_WORKAREA
(defclass xcb:ewmh:get-_NET_WORKAREA (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_WORKAREA)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WORKAREA~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WORKAREA (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_WORKAREA)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_SUPPORTING_WM_CHECK
(defclass xcb:ewmh:get-_NET_SUPPORTING_WM_CHECK (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_SUPPORTING_WM_CHECK)
   (type :initform xcb:Atom:WINDOW)))
(defclass xcb:ewmh:get-_NET_SUPPORTING_WM_CHECK~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_SUPPORTING_WM_CHECK
  (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_SUPPORTING_WM_CHECK)
   (type :initform xcb:Atom:WINDOW)))

;; _NET_VIRTUAL_ROOTS
(defclass xcb:ewmh:get-_NET_VIRTUAL_ROOTS (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_VIRTUAL_ROOTS)
   (type :initform xcb:Atom:WINDOW)))
(defclass xcb:ewmh:get-_NET_VIRTUAL_ROOTS~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_VIRTUAL_ROOTS (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_VIRTUAL_ROOTS)
   (type :initform xcb:Atom:WINDOW)))

;; _NET_DESKTOP_LAYOUT
;; Orientations
(defconst xcb:ewmh:_NET_WM_ORIENTATION_HORZ 0)
(defconst xcb:ewmh:_NET_WM_ORIENTATION_VERT 1)
;; Starting corners
(defconst xcb:ewmh:_NET_WM_TOPLEFT 0)
(defconst xcb:ewmh:_NET_WM_TOPRIGHT 1)
(defconst xcb:ewmh:_NET_WM_BOTTOMRIGHT 2)
(defconst xcb:ewmh:_NET_WM_BOTTOMLEFT 3)
;;
(defclass xcb:ewmh:-_NET_DESKTOP_LAYOUT (xcb:--struct)
  ((orientation :initarg :orientation :type xcb:-ignore)
   (columns :initarg :columns :type xcb:-ignore)
   (rows :initarg :rows :type xcb:-ignore)
   (starting-corner :initarg :starting-corner :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_DESKTOP_LAYOUT (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_DESKTOP_LAYOUT)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 4)))
(defclass xcb:ewmh:get-_NET_DESKTOP_LAYOUT~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_DESKTOP_LAYOUT)
  nil)
(defclass xcb:ewmh:set-_NET_DESKTOP_LAYOUT
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_DESKTOP_LAYOUT)
  ((property :initform xcb:Atom:_NET_DESKTOP_LAYOUT)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_SHOWING_DESKTOP
(defclass xcb:ewmh:get-_NET_SHOWING_DESKTOP (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_SHOWING_DESKTOP)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_SHOWING_DESKTOP~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_SHOWING_DESKTOP (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_SHOWING_DESKTOP)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:_NET_SHOWING_DESKTOP (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_SHOWING_DESKTOP)
   (show :initarg :show :type xcb:CARD32)))

;;;; Other Root Window Messages

;; _NET_CLOSE_WINDOW
(defclass xcb:ewmh:_NET_CLOSE_WINDOW (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_CLOSE_WINDOW)
   (timestamp :initarg :timestamp :type xcb:CARD32)
   (source-indication :initarg :source-indication :type xcb:CARD32)))

;; _NET_MOVERESIZE_WINDOW
(defclass xcb:ewmh:_NET_MOVERESIZE_WINDOW (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_MOVERESIZE_WINDOW)
   (gravity-and-flags :initarg :gravity-and-flags :type xcb:CARD32)
   (x :initarg :x :type xcb:CARD32)
   (y :initarg :y :type xcb:CARD32)
   (width :initarg :width :type xcb:CARD32)
   (height :initarg :height :type xcb:CARD32)))

;; _NET_WM_MOVERESIZE
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPLEFT 0)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOP 1)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_TOPRIGHT 2)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_RIGHT 3)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT 4)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOM 5)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT 6)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_LEFT 7)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_MOVE 8)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_KEYBOARD 9)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_MOVE_KEYBOARD 10)
(defconst xcb:ewmh:_NET_WM_MOVERESIZE_CANCEL 11)
;;
(defclass xcb:ewmh:_NET_WM_MOVERESIZE (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_WM_MOVERESIZE)
   (x-root :initarg :x-root :type xcb:CARD32)
   (y-root :initarg :y-root :type xcb:CARD32)
   (direction :initarg :direction :type xcb:CARD32)
   (button :initarg :button :type xcb:CARD32)
   (source-indication :initarg :source-indication :type xcb:CARD32)))

;; _NET_RESTACK_WINDOW
(defclass xcb:ewmh:_NET_RESTACK_WINDOW (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_RESTACK_WINDOW)
   (source-indication :initarg :source-indication :type xcb:CARD32)
   (sibling :initarg :sibling :type xcb:WINDOW)
   (detail :initarg :detail :type xcb:CARD32)))

;; _NET_REQUEST_FRAME_EXTENTS
(defclass xcb:ewmh:_NET_REQUEST_FRAME_EXTENTS (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_REQUEST_FRAME_EXTENTS)))

;;;; Application Window Properties

;; _NET_WM_NAME
(defclass xcb:ewmh:get-_NET_WM_NAME (xcb:ewmh:-GetProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_NAME)))
(defclass xcb:ewmh:get-_NET_WM_NAME~reply (xcb:ewmh:-GetProperty-utf8~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_NAME (xcb:ewmh:-ChangeProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_NAME)))

;; _NET_WM_VISIBLE_NAME
(defclass xcb:ewmh:get-_NET_WM_VISIBLE_NAME (xcb:ewmh:-GetProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_VISIBLE_NAME)))
(defclass xcb:ewmh:get-_NET_WM_VISIBLE_NAME~reply
  (xcb:ewmh:-GetProperty-utf8~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_VISIBLE_NAME (xcb:ewmh:-ChangeProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_VISIBLE_NAME)))

;; _NET_WM_ICON_NAME
(defclass xcb:ewmh:get-_NET_WM_ICON_NAME (xcb:ewmh:-GetProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_ICON_NAME)))
(defclass xcb:ewmh:get-_NET_WM_ICON_NAME~reply
  (xcb:ewmh:-GetProperty-utf8~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_ICON_NAME (xcb:ewmh:-ChangeProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_ICON_NAME)))

;; _NET_WM_VISIBLE_ICON_NAME
(defclass xcb:ewmh:get-_NET_WM_VISIBLE_ICON_NAME (xcb:ewmh:-GetProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_VISIBLE_ICON_NAME)))
(defclass xcb:ewmh:get-_NET_WM_VISIBLE_ICON_NAME~reply
  (xcb:ewmh:-GetProperty-utf8~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_VISIBLE_ICON_NAME
  (xcb:ewmh:-ChangeProperty-utf8)
  ((property :initform xcb:Atom:_NET_WM_VISIBLE_ICON_NAME)))

;; _NET_WM_DESKTOP
(defclass xcb:ewmh:get-_NET_WM_DESKTOP (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_WM_DESKTOP)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WM_DESKTOP~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_DESKTOP (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_WM_DESKTOP)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:_NET_WM_DESKTOP (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_WM_DESKTOP)
   (new-desktop :initarg :new-desktop :type xcb:CARD32)
   (source-indication :initarg :source-indication :type xcb:CARD32)))

;; _NET_WM_WINDOW_TYPE
(defclass xcb:ewmh:get-_NET_WM_WINDOW_TYPE (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_WM_WINDOW_TYPE)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:ewmh:get-_NET_WM_WINDOW_TYPE~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_WINDOW_TYPE (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_WM_WINDOW_TYPE)
   (type :initform xcb:Atom:ATOM)))

;; _NET_WM_STATE
(defconst xcb:ewmh:_NET_WM_STATE_REMOVE 0)
(defconst xcb:ewmh:_NET_WM_STATE_ADD 1)
(defconst xcb:ewmh:_NET_WM_STATE_TOGGLE 2)
;;
(defclass xcb:ewmh:get-_NET_WM_STATE (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_WM_STATE)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:ewmh:get-_NET_WM_STATE~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_STATE (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_WM_STATE)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:ewmh:_NET_WM_STATE (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_WM_STATE)
   (action :initarg :action :type xcb:CARD32)
   (first-property :initarg :first-property :type xcb:CARD32)
   (second-property :initarg :second-property :type xcb:CARD32)
   (source-indication :initarg :source-indication :type xcb:CARD32)))

;; _NET_WM_ALLOWED_ACTIONS
(defclass xcb:ewmh:get-_NET_WM_ALLOWED_ACTIONS (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_WM_ALLOWED_ACTIONS)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:ewmh:get-_NET_WM_ALLOWED_ACTIONS~reply
  (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_ALLOWED_ACTIONS (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_WM_ALLOWED_ACTIONS)
   (type :initform xcb:Atom:ATOM)))

;; _NET_WM_STRUT
(defclass xcb:ewmh:-_NET_WM_STRUT (xcb:--struct)
  ((left :initarg :left :type xcb:-ignore)
   (right :initarg :right :type xcb:-ignore)
   (top :initarg :top :type xcb:-ignore)
   (bottom :initarg :bottom :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_WM_STRUT (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_WM_STRUT)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 4)))
(defclass xcb:ewmh:get-_NET_WM_STRUT~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_WM_STRUT)
  nil)
(defclass xcb:ewmh:set-_NET_WM_STRUT
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_WM_STRUT)
  ((property :initform xcb:Atom:_NET_WM_STRUT)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_STRUT_PARTIAL
(defclass xcb:ewmh:-_NET_WM_STRUT_PARTIAL (xcb:--struct)
  ((left :initarg :left :type xcb:-ignore)
   (right :initarg :right :type xcb:-ignore)
   (top :initarg :top :type xcb:-ignore)
   (bottom :initarg :bottom :type xcb:-ignore)
   (left-start-y :initarg :left-start-y :type xcb:-ignore)
   (left-end-y :initarg :left-end-y :type xcb:-ignore)
   (right-start-y :initarg :right-start-y :type xcb:-ignore)
   (right-end-y :initarg :right-end-y :type xcb:-ignore)
   (top-start-x :initarg :top-start-x :type xcb:-ignore)
   (top-end-x :initarg :top-end-x :type xcb:-ignore)
   (bottom-start-x :initarg :bottom-start-x :type xcb:-ignore)
   (bottom-end-x :initarg :bottom-end-x :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_WM_STRUT_PARTIAL (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_WM_STRUT_PARTIAL)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 12)))
(defclass xcb:ewmh:get-_NET_WM_STRUT_PARTIAL~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_WM_STRUT_PARTIAL)
  nil)
(defclass xcb:ewmh:set-_NET_WM_STRUT_PARTIAL
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_WM_STRUT_PARTIAL)
  ((property :initform xcb:Atom:_NET_WM_STRUT_PARTIAL)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_ICON_GEOMETRY
(defclass xcb:ewmh:-_NET_WM_ICON_GEOMETRY (xcb:--struct)
  ((x :initarg :x :type xcb:-ignore)
   (y :initarg :y :type xcb:-ignore)
   (width :initarg :width :type xcb:-ignore)
   (height :initarg :height :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_WM_ICON_GEOMETRY (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_WM_ICON_GEOMETRY)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 4)))
(defclass xcb:ewmh:get-_NET_WM_ICON_GEOMETRY~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_WM_ICON_GEOMETRY)
  nil)
(defclass xcb:ewmh:set-_NET_WM_ICON_GEOMETRY
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_WM_ICON_GEOMETRY)
  ((property :initform xcb:Atom:_NET_WM_ICON_GEOMETRY)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_ICON
(defclass xcb:ewmh:-get-_NET_WM_ICON (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_WM_ICON)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:-get-_NET_WM_ICON~reply (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:-set-_NET_WM_ICON (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_WM_ICON)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_PID
(defclass xcb:ewmh:get-_NET_WM_PID (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_WM_PID)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WM_PID~reply (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_PID (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_WM_PID)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_HANDLED_ICONS
(defclass xcb:ewmh:get-_NET_WM_HANDLED_ICONS (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_WM_HANDLED_ICONS)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WM_HANDLED_ICONS~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_HANDLED_ICONS (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_WM_HANDLED_ICONS)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_USER_TIME
(defclass xcb:ewmh:get-_NET_WM_USER_TIME (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_WM_USER_TIME)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WM_USER_TIME~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_USER_TIME (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_WM_USER_TIME)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_USER_TIME_WINDOW
(defclass xcb:ewmh:get-_NET_WM_USER_TIME_WINDOW (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_WM_USER_TIME_WINDOW)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WM_USER_TIME_WINDOW~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_USER_TIME_WINDOW
  (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_WM_USER_TIME_WINDOW)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_FRAME_EXTENTS
(defclass xcb:ewmh:-_NET_FRAME_EXTENTS (xcb:--struct)
  ((left :initarg :left :type xcb:-ignore)
   (right :initarg :right :type xcb:-ignore)
   (top :initarg :top :type xcb:-ignore)
   (bottom :initarg :bottom :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_FRAME_EXTENTS (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_FRAME_EXTENTS)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 4)))
(defclass xcb:ewmh:get-_NET_FRAME_EXTENTS~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_FRAME_EXTENTS)
  nil)
(defclass xcb:ewmh:set-_NET_FRAME_EXTENTS
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_FRAME_EXTENTS)
  ((property :initform xcb:Atom:_NET_FRAME_EXTENTS)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_OPAQUE_REGION
(defclass xcb:ewmh:get-_NET_WM_OPAQUE_REGION (xcb:icccm:-GetProperty)
  ((property :initform xcb:Atom:_NET_WM_OPAQUE_REGION)
   (type :initform xcb:Atom:ATOM)))
(defclass xcb:ewmh:get-_NET_WM_OPAQUE_REGION~reply
  (xcb:icccm:-GetProperty~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_OPAQUE_REGION (xcb:icccm:-ChangeProperty)
  ((property :initform xcb:Atom:_NET_WM_OPAQUE_REGION)
   (type :initform xcb:Atom:ATOM)))

;; _NET_WM_BYPASS_COMPOSITOR
(defclass xcb:ewmh:get-_NET_WM_BYPASS_COMPOSITOR
  (xcb:icccm:-GetProperty-single)
  ((property :initform xcb:Atom:_NET_WM_BYPASS_COMPOSITOR)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:get-_NET_WM_BYPASS_COMPOSITOR~reply
  (xcb:icccm:-GetProperty-single~reply)
  nil)
(defclass xcb:ewmh:set-_NET_WM_BYPASS_COMPOSITOR
  (xcb:icccm:-ChangeProperty-single)
  ((property :initform xcb:Atom:_NET_WM_BYPASS_COMPOSITOR)
   (type :initform xcb:Atom:CARDINAL)))

;;;; Window Manager Protocols

;; _NET_WM_PING
(defclass xcb:ewmh:_NET_WM_PING (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:WM_PROTOCOLS)
   (protocol :initform xcb:Atom:_NET_WM_PING :type xcb:CARD32)
   (timestamp :initarg :timestamp :type xcb:CARD32)
   (client-window :initarg :client-window :type xcb:WINDOW)))

;; _NET_WM_SYNC_REQUEST
(defclass xcb:ewmh:_NET_WM_SYNC_REQUEST (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:WM_PROTOCOLS)
   (protocol :initform xcb:Atom:_NET_WM_SYNC_REQUEST :type xcb:CARD32)
   (timestamp :initarg :timestamp :type xcb:CARD32)
   (low :initarg :low :type xcb:CARD32)
   (high :initarg :high :type xcb:CARD32)))

;; _NET_WM_SYNC_REQUEST_COUNTER
(defclass xcb:ewmh:-_NET_WM_SYNC_REQUEST_COUNTER (xcb:--struct)
  ((low :initarg :low :type xcb:-ignore)
   (high :initarg :hight :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_WM_SYNC_REQUEST_COUNTER
  (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_WM_SYNC_REQUEST_COUNTER)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 2)))
(defclass xcb:ewmh:get-_NET_WM_SYNC_REQUEST_COUNTER~reply
  (xcb:icccm:-GetProperty-explicit~reply
   xcb:ewmh:-_NET_WM_SYNC_REQUEST_COUNTER)
  nil)
(defclass xcb:ewmh:set-_NET_WM_SYNC_REQUEST_COUNTER
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_WM_SYNC_REQUEST_COUNTER)
  ((property :initform xcb:Atom:_NET_WM_SYNC_REQUEST_COUNTER)
   (type :initform xcb:Atom:CARDINAL)))

;; _NET_WM_FULLSCREEN_MONITORS
(defclass xcb:ewmh:-_NET_WM_FULLSCREEN_MONITORS (xcb:--struct)
  ((top :initarg :top :type xcb:-ignore)
   (bottom :initarg :bottom :type xcb:-ignore)
   (left :initarg :left :type xcb:-ignore)
   (right :initarg :right :type xcb:-ignore)))
;;
(defclass xcb:ewmh:get-_NET_WM_FULLSCREEN_MONITORS
  (xcb:icccm:-GetProperty-explicit)
  ((property :initform xcb:Atom:_NET_WM_FULLSCREEN_MONITORS)
   (type :initform xcb:Atom:CARDINAL)
   (long-length :initform 4)))
(defclass xcb:ewmh:get-_NET_WM_FULLSCREEN_MONITORS~reply
  (xcb:icccm:-GetProperty-explicit~reply xcb:ewmh:-_NET_WM_FULLSCREEN_MONITORS)
  nil)
(defclass xcb:ewmh:set-_NET_WM_FULLSCREEN_MONITORS
  (xcb:icccm:-ChangeProperty-explicit xcb:ewmh:-_NET_WM_FULLSCREEN_MONITORS)
  ((property :initform xcb:Atom:_NET_WM_FULLSCREEN_MONITORS)
   (type :initform xcb:Atom:CARDINAL)))
(defclass xcb:ewmh:_NET_WM_FULLSCREEN_MONITORS
  (xcb:ewmh:-ClientMessage xcb:ewmh:-_NET_WM_FULLSCREEN_MONITORS)
  ((type :initform xcb:Atom:_NET_WM_FULLSCREEN_MONITORS)))

;;;; Other Properties

;;;; Misc.

(defconst xcb:ewmh:source-indication:none 0)
(defconst xcb:ewmh:source-indication:normal 1)
(defconst xcb:ewmh:source-indication:other 2)



(provide 'xcb-ewmh)

;;; xcb-ewmh.el ends here
