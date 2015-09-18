;;; -*- lexical-binding: t -*-
;; Copyright (C) 2015 Free Software Foundation, Inc.
;; This file was generated from `xinerama.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:xinerama:-extension-xname "XINERAMA")
(defconst xcb:xinerama:-extension-name "Xinerama")
(defconst xcb:xinerama:-major-version 1)
(defconst xcb:xinerama:-minor-version 1)

(require 'xcb-xproto)

(defclass xcb:xinerama:ScreenInfo
  (xcb:-struct)
  ((x-org :initarg :x-org :type xcb:INT16)
   (y-org :initarg :y-org :type xcb:INT16)
   (width :initarg :width :type xcb:CARD16)
   (height :initarg :height :type xcb:CARD16)))

(defclass xcb:xinerama:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (major :initarg :major :type xcb:CARD8)
   (minor :initarg :minor :type xcb:CARD8)))
(defclass xcb:xinerama:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (major :initarg :major :type xcb:CARD16)
   (minor :initarg :minor :type xcb:CARD16)))

(defclass xcb:xinerama:GetState
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)))
(defclass xcb:xinerama:GetState~reply
  (xcb:-reply)
  ((state :initarg :state :type xcb:BYTE)
   (window :initarg :window :type xcb:WINDOW)))

(defclass xcb:xinerama:GetScreenCount
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)))
(defclass xcb:xinerama:GetScreenCount~reply
  (xcb:-reply)
  ((screen-count :initarg :screen-count :type xcb:BYTE)
   (window :initarg :window :type xcb:WINDOW)))

(defclass xcb:xinerama:GetScreenSize
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (screen :initarg :screen :type xcb:CARD32)))
(defclass xcb:xinerama:GetScreenSize~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (width :initarg :width :type xcb:CARD32)
   (height :initarg :height :type xcb:CARD32)
   (window :initarg :window :type xcb:WINDOW)
   (screen :initarg :screen :type xcb:CARD32)))

(defclass xcb:xinerama:IsActive
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)))
(defclass xcb:xinerama:IsActive~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (state :initarg :state :type xcb:CARD32)))

(defclass xcb:xinerama:QueryScreens
  (xcb:-request)
  ((~opcode :initform 5 :type xcb:-u1)))
(defclass xcb:xinerama:QueryScreens~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (number :initarg :number :type xcb:CARD32)
   (pad~1 :initform 20 :type xcb:-pad)
   (screen-info :initarg :screen-info :type xcb:-ignore)
   (screen-info~ :initform
		 '(name screen-info type xcb:xinerama:ScreenInfo size
			(xcb:-fieldref 'number))
		 :type xcb:-list)))



(provide 'xcb-xinerama)
