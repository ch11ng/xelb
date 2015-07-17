;;; -*- lexical-binding: t -*-
;; This file was generated from `xtest.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:xtest:-extension-xname "XTEST")
(defconst xcb:xtest:-extension-name "Test")
(defconst xcb:xtest:-major-version 2)
(defconst xcb:xtest:-minor-version 2)

(require 'xcb-xproto)

(defclass xcb:xtest:GetVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (major-version :initarg :major-version :type xcb:CARD8)
   (pad~0 :initform 1 :type xcb:-pad)
   (minor-version :initarg :minor-version :type xcb:CARD16)))
(defclass xcb:xtest:GetVersion~reply
  (xcb:-reply)
  ((major-version :initarg :major-version :type xcb:CARD8)
   (minor-version :initarg :minor-version :type xcb:CARD16)))

(defconst xcb:xtest:Cursor:None 0)
(defconst xcb:xtest:Cursor:Current 1)

(defclass xcb:xtest:CompareCursor
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (window :initarg :window :type xcb:WINDOW)
   (cursor :initarg :cursor :type xcb:CURSOR)))
(defclass xcb:xtest:CompareCursor~reply
  (xcb:-reply)
  ((same :initarg :same :type xcb:BOOL)))

(defclass xcb:xtest:FakeInput
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (type :initarg :type :type xcb:BYTE)
   (detail :initarg :detail :type xcb:BYTE)
   (pad~0 :initform 2 :type xcb:-pad)
   (time :initarg :time :type xcb:CARD32)
   (root :initarg :root :type xcb:WINDOW)
   (pad~1 :initform 8 :type xcb:-pad)
   (rootX :initarg :rootX :type xcb:INT16)
   (rootY :initarg :rootY :type xcb:INT16)
   (pad~2 :initform 7 :type xcb:-pad)
   (deviceid :initarg :deviceid :type xcb:CARD8)))

(defclass xcb:xtest:GrabControl
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (impervious :initarg :impervious :type xcb:BOOL)
   (pad~0 :initform 3 :type xcb:-pad)))



(provide 'xcb-xtest)
