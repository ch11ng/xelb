;;; -*- lexical-binding: t -*-
;; This file was generated from `xevie.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:xevie:-extension-xname "XEVIE")
(defconst xcb:xevie:-extension-name "Xevie")
(defconst xcb:xevie:-major-version 1)
(defconst xcb:xevie:-minor-version 0)

(defclass xcb:xevie:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (client-major-version :initarg :client-major-version :type xcb:CARD16)
   (client-minor-version :initarg :client-minor-version :type xcb:CARD16)))
(defclass xcb:xevie:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (server-major-version :initarg :server-major-version :type xcb:CARD16)
   (server-minor-version :initarg :server-minor-version :type xcb:CARD16)
   (pad~1 :initform 20 :type xcb:-pad)))

(defclass xcb:xevie:Start
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (screen :initarg :screen :type xcb:CARD32)))
(defclass xcb:xevie:Start~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (pad~1 :initform 24 :type xcb:-pad)))

(defclass xcb:xevie:End
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (cmap :initarg :cmap :type xcb:CARD32)))
(defclass xcb:xevie:End~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (pad~1 :initform 24 :type xcb:-pad)))

(defconst xcb:xevie:Datatype:Unmodified 0)
(defconst xcb:xevie:Datatype:Modified 1)

(defclass xcb:xevie:Event
  (xcb:-struct)
  ((pad~0 :initform 32 :type xcb:-pad)))

(defclass xcb:xevie:Send
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (event :initarg :event :type xcb:xevie:Event)
   (data-type :initarg :data-type :type xcb:CARD32)
   (pad~0 :initform 64 :type xcb:-pad)))
(defclass xcb:xevie:Send~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (pad~1 :initform 24 :type xcb:-pad)))

(defclass xcb:xevie:SelectInput
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)
   (event-mask :initarg :event-mask :type xcb:CARD32)))
(defclass xcb:xevie:SelectInput~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (pad~1 :initform 24 :type xcb:-pad)))



(provide 'xcb-xevie)
