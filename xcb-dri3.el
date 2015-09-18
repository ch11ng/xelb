;;; -*- lexical-binding: t -*-
;; Copyright (C) 2015 Free Software Foundation, Inc.
;; This file was generated from `dri3.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:dri3:-extension-xname "DRI3")
(defconst xcb:dri3:-extension-name "DRI3")
(defconst xcb:dri3:-major-version 1)
(defconst xcb:dri3:-minor-version 0)

(require 'xcb-xproto)

(defclass xcb:dri3:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (major-version :initarg :major-version :type xcb:CARD32)
   (minor-version :initarg :minor-version :type xcb:CARD32)))
(defclass xcb:dri3:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (major-version :initarg :major-version :type xcb:CARD32)
   (minor-version :initarg :minor-version :type xcb:CARD32)))

(defclass xcb:dri3:Open
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)
   (provider :initarg :provider :type xcb:CARD32)))
(defclass xcb:dri3:Open~reply
  (xcb:-reply)
  ((nfd :initarg :nfd :type xcb:CARD8)
   (device-fd :type xcb:-fd)
   (pad~0 :initform 24 :type xcb:-pad)))

(defclass xcb:dri3:PixmapFromBuffer
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (pixmap :initarg :pixmap :type xcb:PIXMAP)
   (drawable :initarg :drawable :type xcb:DRAWABLE)
   (size :initarg :size :type xcb:CARD32)
   (width :initarg :width :type xcb:CARD16)
   (height :initarg :height :type xcb:CARD16)
   (stride :initarg :stride :type xcb:CARD16)
   (depth :initarg :depth :type xcb:CARD8)
   (bpp :initarg :bpp :type xcb:CARD8)
   (pixmap-fd :type xcb:-fd)))

(defclass xcb:dri3:BufferFromPixmap
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (pixmap :initarg :pixmap :type xcb:PIXMAP)))
(defclass xcb:dri3:BufferFromPixmap~reply
  (xcb:-reply)
  ((nfd :initarg :nfd :type xcb:CARD8)
   (size :initarg :size :type xcb:CARD32)
   (width :initarg :width :type xcb:CARD16)
   (height :initarg :height :type xcb:CARD16)
   (stride :initarg :stride :type xcb:CARD16)
   (depth :initarg :depth :type xcb:CARD8)
   (bpp :initarg :bpp :type xcb:CARD8)
   (pixmap-fd :type xcb:-fd)
   (pad~0 :initform 12 :type xcb:-pad)))

(defclass xcb:dri3:FenceFromFD
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)
   (fence :initarg :fence :type xcb:CARD32)
   (initially-triggered :initarg :initially-triggered :type xcb:BOOL)
   (pad~0 :initform 3 :type xcb:-pad)
   (fence-fd :type xcb:-fd)))

(defclass xcb:dri3:FDFromFence
  (xcb:-request)
  ((~opcode :initform 5 :type xcb:-u1)
   (drawable :initarg :drawable :type xcb:DRAWABLE)
   (fence :initarg :fence :type xcb:CARD32)))
(defclass xcb:dri3:FDFromFence~reply
  (xcb:-reply)
  ((nfd :initarg :nfd :type xcb:CARD8)
   (fence-fd :type xcb:-fd)
   (pad~0 :initform 24 :type xcb:-pad)))



(provide 'xcb-dri3)
