;;; -*- lexical-binding: t -*-
;; Copyright (C) 2015 Free Software Foundation, Inc.
;; This file was generated from `bigreq.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:bigreq:-extension-xname "BIG-REQUESTS")
(defconst xcb:bigreq:-extension-name "BigRequests")
(defconst xcb:bigreq:-major-version 0)
(defconst xcb:bigreq:-minor-version 0)

(defclass xcb:bigreq:Enable
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)))
(defclass xcb:bigreq:Enable~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (maximum-request-length :initarg :maximum-request-length :type xcb:CARD32)))



(provide 'xcb-bigreq)
