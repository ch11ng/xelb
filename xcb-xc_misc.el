;;; -*- lexical-binding: t -*-
;; This file was generated from `xc_misc.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:xc_misc:-extension-xname "XC-MISC")
(defconst xcb:xc_misc:-extension-name "XCMisc")
(defconst xcb:xc_misc:-major-version 1)
(defconst xcb:xc_misc:-minor-version 1)

(defclass xcb:xc_misc:GetVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (client-major-version :initarg :client-major-version :type xcb:CARD16)
   (client-minor-version :initarg :client-minor-version :type xcb:CARD16)))
(defclass xcb:xc_misc:GetVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (server-major-version :initarg :server-major-version :type xcb:CARD16)
   (server-minor-version :initarg :server-minor-version :type xcb:CARD16)))

(defclass xcb:xc_misc:GetXIDRange
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)))
(defclass xcb:xc_misc:GetXIDRange~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (start-id :initarg :start-id :type xcb:CARD32)
   (count :initarg :count :type xcb:CARD32)))

(defclass xcb:xc_misc:GetXIDList
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (count :initarg :count :type xcb:CARD32)))
(defclass xcb:xc_misc:GetXIDList~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (ids-len :initarg :ids-len :type xcb:CARD32)
   (pad~1 :initform 20 :type xcb:-pad)
   (ids :initarg :ids :type xcb:-ignore)
   (ids~ :initform
	 '(name ids type xcb:CARD32 size
		(xcb:-fieldref 'ids-len))
	 :type xcb:-list)))



(provide 'xcb-xc_misc)
