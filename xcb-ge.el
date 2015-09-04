;;; -*- lexical-binding: t -*-
;; This file was generated from `ge.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:ge:-extension-xname "Generic Event Extension")
(defconst xcb:ge:-extension-name "GenericEvent")
(defconst xcb:ge:-major-version 1)
(defconst xcb:ge:-minor-version 0)

(defclass xcb:ge:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (client-major-version :initarg :client-major-version :type xcb:CARD16)
   (client-minor-version :initarg :client-minor-version :type xcb:CARD16)))
(defclass xcb:ge:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (major-version :initarg :major-version :type xcb:CARD16)
   (minor-version :initarg :minor-version :type xcb:CARD16)
   (pad~1 :initform 20 :type xcb:-pad)))



(provide 'xcb-ge)
