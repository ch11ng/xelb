;;; -*- lexical-binding: t -*-
;; This file was generated from `record.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:record:-extension-xname "RECORD")
(defconst xcb:record:-extension-name "Record")
(defconst xcb:record:-major-version 1)
(defconst xcb:record:-minor-version 13)

(xcb:deftypealias 'xcb:record:CONTEXT 'xcb:-u4)

(defclass xcb:record:Range8
  (xcb:-struct)
  ((first :initarg :first :type xcb:CARD8)
   (last :initarg :last :type xcb:CARD8)))

(defclass xcb:record:Range16
  (xcb:-struct)
  ((first :initarg :first :type xcb:CARD16)
   (last :initarg :last :type xcb:CARD16)))

(defclass xcb:record:ExtRange
  (xcb:-struct)
  ((major :initarg :major :type xcb:record:Range8)
   (minor :initarg :minor :type xcb:record:Range16)))

(defclass xcb:record:Range
  (xcb:-struct)
  ((core-requests :initarg :core-requests :type xcb:record:Range8)
   (core-replies :initarg :core-replies :type xcb:record:Range8)
   (ext-requests :initarg :ext-requests :type xcb:record:ExtRange)
   (ext-replies :initarg :ext-replies :type xcb:record:ExtRange)
   (delivered-events :initarg :delivered-events :type xcb:record:Range8)
   (device-events :initarg :device-events :type xcb:record:Range8)
   (errors :initarg :errors :type xcb:record:Range8)
   (client-started :initarg :client-started :type xcb:BOOL)
   (client-died :initarg :client-died :type xcb:BOOL)))

(xcb:deftypealias 'xcb:record:ElementHeader 'xcb:CARD8)

(defconst xcb:record:HType:FromServerTime 1)
(defconst xcb:record:HType:FromClientTime 2)
(defconst xcb:record:HType:FromClientSequence 4)

(xcb:deftypealias 'xcb:record:ClientSpec 'xcb:CARD32)

(defconst xcb:record:CS:CurrentClients 1)
(defconst xcb:record:CS:FutureClients 2)
(defconst xcb:record:CS:AllClients 3)

(defclass xcb:record:ClientInfo
  (xcb:-struct)
  ((client-resource :initarg :client-resource :type xcb:record:ClientSpec)
   (num-ranges :initarg :num-ranges :type xcb:CARD32)
   (ranges :initarg :ranges :type xcb:-ignore)
   (ranges~ :initform
	    '(name ranges type xcb:record:Range size
		   (xcb:-fieldref 'num-ranges))
	    :type xcb:-list)))

(defclass xcb:record:BadContext
  (xcb:-error)
  ((invalid-record :initarg :invalid-record :type xcb:CARD32)))

(defclass xcb:record:QueryVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (major-version :initarg :major-version :type xcb:CARD16)
   (minor-version :initarg :minor-version :type xcb:CARD16)))
(defclass xcb:record:QueryVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (major-version :initarg :major-version :type xcb:CARD16)
   (minor-version :initarg :minor-version :type xcb:CARD16)))

(defclass xcb:record:CreateContext
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)
   (element-header :initarg :element-header :type xcb:record:ElementHeader)
   (pad~0 :initform 3 :type xcb:-pad)
   (num-client-specs :initarg :num-client-specs :type xcb:CARD32)
   (num-ranges :initarg :num-ranges :type xcb:CARD32)
   (client-specs :initarg :client-specs :type xcb:-ignore)
   (client-specs~ :initform
		  '(name client-specs type xcb:record:ClientSpec size
			 (xcb:-fieldref 'num-client-specs))
		  :type xcb:-list)
   (ranges :initarg :ranges :type xcb:-ignore)
   (ranges~ :initform
	    '(name ranges type xcb:record:Range size
		   (xcb:-fieldref 'num-ranges))
	    :type xcb:-list)))

(defclass xcb:record:RegisterClients
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)
   (element-header :initarg :element-header :type xcb:record:ElementHeader)
   (pad~0 :initform 3 :type xcb:-pad)
   (num-client-specs :initarg :num-client-specs :type xcb:CARD32)
   (num-ranges :initarg :num-ranges :type xcb:CARD32)
   (client-specs :initarg :client-specs :type xcb:-ignore)
   (client-specs~ :initform
		  '(name client-specs type xcb:record:ClientSpec size
			 (xcb:-fieldref 'num-client-specs))
		  :type xcb:-list)
   (ranges :initarg :ranges :type xcb:-ignore)
   (ranges~ :initform
	    '(name ranges type xcb:record:Range size
		   (xcb:-fieldref 'num-ranges))
	    :type xcb:-list)))

(defclass xcb:record:UnregisterClients
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)
   (num-client-specs :initarg :num-client-specs :type xcb:CARD32)
   (client-specs :initarg :client-specs :type xcb:-ignore)
   (client-specs~ :initform
		  '(name client-specs type xcb:record:ClientSpec size
			 (xcb:-fieldref 'num-client-specs))
		  :type xcb:-list)))

(defclass xcb:record:GetContext
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)))
(defclass xcb:record:GetContext~reply
  (xcb:-reply)
  ((enabled :initarg :enabled :type xcb:BOOL)
   (element-header :initarg :element-header :type xcb:record:ElementHeader)
   (pad~0 :initform 3 :type xcb:-pad)
   (num-intercepted-clients :initarg :num-intercepted-clients :type xcb:CARD32)
   (pad~1 :initform 16 :type xcb:-pad)
   (intercepted-clients :initarg :intercepted-clients :type xcb:-ignore)
   (intercepted-clients~ :initform
			 '(name intercepted-clients type xcb:record:ClientInfo size
				(xcb:-fieldref 'num-intercepted-clients))
			 :type xcb:-list)))

(defclass xcb:record:EnableContext
  (xcb:-request)
  ((~opcode :initform 5 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)))
(defclass xcb:record:EnableContext~reply
  (xcb:-reply)
  ((category :initarg :category :type xcb:CARD8)
   (element-header :initarg :element-header :type xcb:record:ElementHeader)
   (client-swapped :initarg :client-swapped :type xcb:BOOL)
   (pad~0 :initform 2 :type xcb:-pad)
   (xid-base :initarg :xid-base :type xcb:CARD32)
   (server-time :initarg :server-time :type xcb:CARD32)
   (rec-sequence-num :initarg :rec-sequence-num :type xcb:CARD32)
   (pad~1 :initform 8 :type xcb:-pad)
   (data :initarg :data :type xcb:-ignore)
   (data~ :initform
	  '(name data type xcb:BYTE size
		 (*
		  (xcb:-fieldref 'length)
		  4))
	  :type xcb:-list)))

(defclass xcb:record:DisableContext
  (xcb:-request)
  ((~opcode :initform 6 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)))

(defclass xcb:record:FreeContext
  (xcb:-request)
  ((~opcode :initform 7 :type xcb:-u1)
   (context :initarg :context :type xcb:record:CONTEXT)))

(defconst xcb:record:error-number-class-alist
  '((0 . xcb:record:BadContext))
  "(error-number . error-class) alist")



(provide 'xcb-record)
