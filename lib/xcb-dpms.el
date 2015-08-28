;;; -*- lexical-binding: t -*-
;; This file was generated from `dpms.xml' by `el_client.el'.

(require 'xcb-types)

(defconst xcb:dpms:-extension-xname "DPMS")
(defconst xcb:dpms:-extension-name "DPMS")
(defconst xcb:dpms:-major-version 0)
(defconst xcb:dpms:-minor-version 0)

(defclass xcb:dpms:GetVersion
  (xcb:-request)
  ((~opcode :initform 0 :type xcb:-u1)
   (client-major-version :initarg :client-major-version :type xcb:CARD16)
   (client-minor-version :initarg :client-minor-version :type xcb:CARD16)))
(defclass xcb:dpms:GetVersion~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (server-major-version :initarg :server-major-version :type xcb:CARD16)
   (server-minor-version :initarg :server-minor-version :type xcb:CARD16)))

(defclass xcb:dpms:Capable
  (xcb:-request)
  ((~opcode :initform 1 :type xcb:-u1)))
(defclass xcb:dpms:Capable~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (capable :initarg :capable :type xcb:BOOL)
   (pad~1 :initform 23 :type xcb:-pad)))

(defclass xcb:dpms:GetTimeouts
  (xcb:-request)
  ((~opcode :initform 2 :type xcb:-u1)))
(defclass xcb:dpms:GetTimeouts~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (standby-timeout :initarg :standby-timeout :type xcb:CARD16)
   (suspend-timeout :initarg :suspend-timeout :type xcb:CARD16)
   (off-timeout :initarg :off-timeout :type xcb:CARD16)
   (pad~1 :initform 18 :type xcb:-pad)))

(defclass xcb:dpms:SetTimeouts
  (xcb:-request)
  ((~opcode :initform 3 :type xcb:-u1)
   (standby-timeout :initarg :standby-timeout :type xcb:CARD16)
   (suspend-timeout :initarg :suspend-timeout :type xcb:CARD16)
   (off-timeout :initarg :off-timeout :type xcb:CARD16)))

(defclass xcb:dpms:Enable
  (xcb:-request)
  ((~opcode :initform 4 :type xcb:-u1)))

(defclass xcb:dpms:Disable
  (xcb:-request)
  ((~opcode :initform 5 :type xcb:-u1)))

(defconst xcb:dpms:DPMSMode:On 0)
(defconst xcb:dpms:DPMSMode:Standby 1)
(defconst xcb:dpms:DPMSMode:Suspend 2)
(defconst xcb:dpms:DPMSMode:Off 3)

(defclass xcb:dpms:ForceLevel
  (xcb:-request)
  ((~opcode :initform 6 :type xcb:-u1)
   (power-level :initarg :power-level :type xcb:CARD16)))

(defclass xcb:dpms:Info
  (xcb:-request)
  ((~opcode :initform 7 :type xcb:-u1)))
(defclass xcb:dpms:Info~reply
  (xcb:-reply)
  ((pad~0 :initform 1 :type xcb:-pad)
   (power-level :initarg :power-level :type xcb:CARD16)
   (state :initarg :state :type xcb:BOOL)
   (pad~1 :initform 21 :type xcb:-pad)))



(provide 'xcb-dpms)
