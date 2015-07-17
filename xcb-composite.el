;;; -*- lexical-binding: t -*-
;; This file was generated from `composite.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:composite:-extension-xname "Composite")
(defconst xcb:composite:-extension-name "Composite")
(defconst xcb:composite:-major-version 0)
(defconst xcb:composite:-minor-version 4)

(require 'xcb-xproto)

(require 'xcb-xfixes)

(defconst xcb:composite:Redirect:Automatic 0)
(defconst xcb:composite:Redirect:Manual 1)

(defclass xcb:composite:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (client-major-version :initarg :client-major-version :type xcb:CARD32)
   (client-minor-version :initarg :client-minor-version :type xcb:CARD32)))
(defclass xcb:composite:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (major-version :initarg :major-version :type xcb:CARD32)
   (minor-version :initarg :minor-version :type xcb:CARD32)
   (pad~1 :initform 16 :type xcb:-pad)))

(defclass xcb:composite:RedirectWindow
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (update :initarg :update :type xcb:CARD8)
   (pad~0 :initform 3 :type xcb:-pad)))

(defclass xcb:composite:RedirectSubwindows
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (update :initarg :update :type xcb:CARD8)
   (pad~0 :initform 3 :type xcb:-pad)))

(defclass xcb:composite:UnredirectWindow
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (update :initarg :update :type xcb:CARD8)
   (pad~0 :initform 3 :type xcb:-pad)))

(defclass xcb:composite:UnredirectSubwindows
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (update :initarg :update :type xcb:CARD8)
   (pad~0 :initform 3 :type xcb:-pad)))

(defclass xcb:composite:CreateRegionFromBorderClip
  (xcb:-request)
  ((~opcode :initform 5 :type xcb:-u1)
   (region :initarg :region :type xcb:composite:REGION)
   (window :initarg :window :type xcb:WINDOW)))

(defclass xcb:composite:NameWindowPixmap
  (xcb:-request)
  ((~opcode :initform 6 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (pixmap :initarg :pixmap :type xcb:PIXMAP)))

(defclass xcb:composite:GetOverlayWindow
  (xcb:-request)
  ((~opcode :initform 7 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)))
(defclass xcb:composite:GetOverlayWindow~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (overlay-win :initarg :overlay-win :type xcb:WINDOW)
   (pad~1 :initform 20 :type xcb:-pad)))

(defclass xcb:composite:ReleaseOverlayWindow
  (xcb:-request)
  ((~opcode :initform 8 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)))



(provide 'xcb-composite)
