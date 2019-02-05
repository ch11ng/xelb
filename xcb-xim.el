;;; xcb-xim.el --- XIM Protocol  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements the X Input Method Protocol.

;; Please note that the byte order of an XIM packet can be different from that
;; of X packets.  Moreover, if you are writing an XIM server, the byte order is
;; actually specified by the client.  Therefore we provide a different global
;; variable `xim:lsb' to indicate the byte order of classes in this library.
;; You should let-bind it whenever creating new objects.

;; Todo:
;; + Add extension support.

;; References:
;; + XIM (http://www.x.org/releases/X11R7.7/doc/libX11/XIM/xim.txt)

;;; Code:

(require 'xcb-types)
(require 'xcb-xlib)

;;;; Protocol number

(defconst xim:opcode:connect 1)
(defconst xim:opcode:connect-reply 2)
(defconst xim:opcode:disconnect 3)
(defconst xim:opcode:disconnect-reply 4)

(defconst xim:opcode:auth-required 10)
(defconst xim:opcode:auth-reply 11)
(defconst xim:opcode:auth-next 12)
(defconst xim:opcode:auth-setup 13)
(defconst xim:opcode:auth-ng 14)

(defconst xim:opcode:error 20)

(defconst xim:opcode:open 30)
(defconst xim:opcode:open-reply 31)
(defconst xim:opcode:close 32)
(defconst xim:opcode:close-reply 33)
(defconst xim:opcode:register-triggerkeys 34)
(defconst xim:opcode:trigger-notify 35)
(defconst xim:opcode:trigger-notify-reply 36)
(defconst xim:opcode:set-event-mask 37)
(defconst xim:opcode:encoding-negotiation 38)
(defconst xim:opcode:encoding-negotiation-reply 39)
(defconst xim:opcode:query-extension 40)
(defconst xim:opcode:query-extension-reply 41)
(defconst xim:opcode:set-im-values 42)
(defconst xim:opcode:set-im-values-reply 43)
(defconst xim:opcode:get-im-values 44)
(defconst xim:opcode:get-im-values-reply 45)

(defconst xim:opcode:create-ic 50)
(defconst xim:opcode:create-ic-reply 51)
(defconst xim:opcode:destroy-ic 52)
(defconst xim:opcode:destroy-ic-reply 53)
(defconst xim:opcode:set-ic-values 54)
(defconst xim:opcode:set-ic-values-reply 55)
(defconst xim:opcode:get-ic-values 56)
(defconst xim:opcode:get-ic-values-reply 57)
(defconst xim:opcode:set-ic-focus 58)
(defconst xim:opcode:unset-ic-focus 59)
(defconst xim:opcode:forward-event 60)
(defconst xim:opcode:sync 61)
(defconst xim:opcode:sync-reply 62)
(defconst xim:opcode:commit 63)
(defconst xim:opcode:reset-ic 64)
(defconst xim:opcode:reset-ic-reply 65)

(defconst xim:opcode:geometry 70)
(defconst xim:opcode:str-conversion 71)
(defconst xim:opcode:str-conversion-reply 72)
(defconst xim:opcode:preedit-start 73)
(defconst xim:opcode:preedit-start-reply 74)
(defconst xim:opcode:preedit-draw 75)
(defconst xim:opcode:preedit-caret 76)
(defconst xim:opcode:preedit-caret-reply 77)
(defconst xim:opcode:preedit-done 78)
(defconst xim:opcode:status-start 79)
(defconst xim:opcode:status-draw 80)
(defconst xim:opcode:status-done 81)
(defconst xim:opcode:preeditstate 82)

;;;; Basic requests packet format

(eval-and-compile
  (defvar xim:lsb xcb:lsb "Non-nil for LSB first, nil otherwise.

Consider let-bind it rather than change its global value."))

(defclass xim:-struct (xcb:-struct)
  ((~lsb :initform (symbol-value 'xim:lsb)))
  :documentation "Struct type for XIM.")

(defclass xim:-request (xim:-struct)
  ((~major-opcode :type xcb:CARD8)
   (~minor-opcode :initform 0 :type xcb:CARD8)
   (~length :initform 0 :type xcb:CARD16))
  :documentation "XIM request type.")

(cl-defmethod xcb:marshal ((obj xim:-request))
  "Return the byte-array representation of XIM request OBJ."
  (let ((result (cl-call-next-method obj)))
    (vconcat (substring result 0 2)
             (funcall (if (slot-value obj '~lsb)
                          #'xcb:-pack-u2-lsb
                        #'xcb:-pack-u2)
                      (1- (/ (length result) 4)))
             (substring result 4))))

;;;; Data types

(xcb:deftypealias 'xim:BITMASK16 'xcb:CARD16)

(xcb:deftypealias 'xim:BITMASK32 'xcb:CARD32)

(defsubst xim:PADDING (N)
  "Pad N to 4 bytes."
  (% (- 4 (% N 4)) 4))

(xcb:deftypealias 'xim:LPCE 'xcb:char)

(defclass xim:STRING (xim:-struct)
  ((length :initarg :length :type xcb:-u2)
   (string :initarg :string :type xcb:-ignore)
   (string~ :initform '(name string type xim:LPCE size (xcb:-fieldref 'length))
            :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

(defclass xim:STR (xim:-struct)
  ((length :initarg :length :type xcb:-u1)
   (name :initarg :name :type xcb:-ignore)
   (name~ :initform '(name name type xcb:char size (xcb:-fieldref 'length))
          :type xcb:-list)))

(defclass xim:XIMATTR (xim:-struct)
  ((id :initarg :id :type xcb:CARD16)
   (type :initarg :type :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (attribute :initarg :attribute :type xcb:-ignore)
   (attribute~ :initform '(name attribute type xcb:char
                                size (xcb:-fieldref 'length))
               :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

(defclass xim:XICATTR (xim:XIMATTR)
  nil)

(defconst xim:ATTRIBUTE-VALUE-TYPE:separator-of-nestedlist 0)
(defconst xim:ATTRIBUTE-VALUE-TYPE:byte-data 1)
(defconst xim:ATTRIBUTE-VALUE-TYPE:word-data 2)
(defconst xim:ATTRIBUTE-VALUE-TYPE:long-data 3)
(defconst xim:ATTRIBUTE-VALUE-TYPE:char-data 4)
(defconst xim:ATTRIBUTE-VALUE-TYPE:window 5)
(defconst xim:ATTRIBUTE-VALUE-TYPE:xim-styles 10)
(defconst xim:ATTRIBUTE-VALUE-TYPE:x-rectangle 11)
(defconst xim:ATTRIBUTE-VALUE-TYPE:x-point 12)
(defconst xim:ATTRIBUTE-VALUE-TYPE:x-font-set 13)
(defconst xim:ATTRIBUTE-VALUE-TYPE:xim-hot-key-triggers 15)
(defconst xim:ATTRIBUTE-VALUE-TYPE:xim-string-conversion 17)
(defconst xim:ATTRIBUTE-VALUE-TYPE:xim-preedit-state 18)
(defconst xim:ATTRIBUTE-VALUE-TYPE:xim-reset-state 19)
(defconst xim:ATTRIBUTE-VALUE-TYPE:xim-nested-list #x7FFF)

(defclass xim:XIMStyles (xim:-struct)
  ((number :initarg :number :type xcb:-u2)
   (pad~0 :initform 2 :type xcb:-pad)
   (styles :initarg :styles :type xcb:-ignore)
   (styles~ :initform '(name styles type xcb:CARD32
                             size (/ (xcb:-fieldref 'number) 4))
            :type xcb:-list)))

;; Auto set the number slot
(cl-defmethod xcb:marshal ((obj xim:XIMStyles))
  (setf (slot-value obj 'number) (* 4 (length (slot-value obj 'styles))))
  (cl-call-next-method obj))

(defclass xim:XFontSet (xim:-struct)
  ((length :initarg :length :type xcb:-u2)
   (base-font-name :initarg :base-font-name :type xcb:-ignore)
   (base-font-name~ :initform '(name base-font-name type xim:LPCE
                                     size (xcb:-fieldref 'length))
                    :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

(defclass xim:XIMHotKeyTriggers (xim:-struct)
  ((number :type xcb:-u4)
   (triggers :type xcb:-ignore)
   (triggers~ :initform '(name triggers type xim:XIMTRIGGERKEY
                               size (xcb:-fieldref 'number))
              :type xcb:-list)
   (states :type xcb:-ignore)
   (states~ :initform '(name states type xim:XIMHOTKEYSTATE
                             size (xcb:-fieldref 'number))
            :type xcb:-list)))

(defclass xim:XIMTRIGGERKEY (xim:-struct)
  ((keysym :initarg :keysym :type xcb:CARD32)
   (modifier :initarg :modifier :type xcb:CARD32)
   (modifier-mask :initarg :modifier-mask :type xcb:CARD32)))

(defclass xim:ENCODINGINFO (xim:-struct)
  ((length :initarg :length :type xcb:-u2)
   (encoding-info :initarg :encoding-info :type xcb:-ignore)
   (encoding-info~ :initform '(name encoding-info type xcb:char
                                    size (xcb:-fieldref 'length))
                   :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

(defclass xim:EXT (xim:-struct)
  ((major-opcode :initarg :major-opcode :type xcb:CARD8)
   (minor-opcode :initarg :minor-opcode :type xcb:CARD8)
   (length :initarg :length :type xcb:-u2)
   (name :initarg :name :type xcb:-ignore)
   (name~ :initform '(name name type xcb:char size (xcb:-fieldref 'length))
          :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(defclass xim:XIMATTRIBUTE (xim:-struct)
  ((id :initarg :id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (value :initarg :value :type xcb:-ignore)
   (value~ :initform '(name value type xcb:void size (xcb:-fieldref 'length))
           :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(cl-defmethod xcb:marshal ((obj xim:XIMATTRIBUTE))
  (let ((value (slot-value obj 'value)))
    (when (eieio-object-p value)
      (setq value (xcb:marshal value))
      (setf (slot-value obj 'length) (length value)
            (slot-value obj 'value) value))
    (cl-call-next-method obj)))

(defclass xim:XICATTRIBUTE (xim:XIMATTRIBUTE)
  nil)

(defclass xim:XIMSTRCONVTEXT (xim:-struct)
  ((feedback :initarg :feedback :type xcb:CARD16)
   (string-length :initarg :string-length :type xcb:-u2)
   (string :initarg :string :type xcb:-ignore)
   (string~ :initform '(name string type xcb:char
                             size (xcb:-fieldref 'string-length))
            :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (xcb:-fieldref 'string-length))
          :type xcb:-pad)
   (feedbacks-length :initarg :feedbacks-length :type xcb:-u2)
   (pad~1 :initform 2 :type xcb:-pad)
   (feedbacks :initarg :feedbacks :type xcb:-ignore)
   (feedbacks~ :initform '(name feedbacks type xcb:void
                                size (xcb:-fieldref 'feedbacks-length))
               :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:XIMSTRCONVTEXT))
  (let ((feedbacks (mapconcat 'xcb:marshal (slot-value obj 'feedbacks) [])))
    (setf (slot-value obj 'feedbacks-length) (length feedbacks)
          (slot-value obj 'feedbacks) feedbacks)
    (cl-call-next-method obj)))

(cl-defmethod xcb:unmarshal ((obj xim:XIMSTRCONVTEXT) byte-array)
  (let ((retval (cl-call-next-method obj byte-array))
        (data (slot-value obj 'feedbacks))
        feedback feedbacks)
    (while (< 0 (length data))
      (setq feedback (make-instance 'xim:XIMSTRCONVFEEDBACK)
            data (substring data (xcb:unmarshal feedback data))
            feedbacks (nconc feedbacks (list feedback))))
    (setf (slot-value obj 'feedbacks) feedbacks)
    retval))

(defconst xim:string-conversion:left-edge #x0000001)
(defconst xim:string-conversion:right-edge #x0000002)
(defconst xim:string-conversion:top-edge #x0000004)
(defconst xim:string-conversion:bottom-edge #x0000008)
(defconst xim:string-conversion:convealed #x0000010)
(defconst xim:string-conversion:wrapped #x0000020)

(xcb:deftypealias 'xim:XIMFEEDBACK 'xcb:CARD32)

;; FIXME: different from Xlib:XIM*
(defconst xim:reverse #x000001)
(defconst xim:underline #x000002)
(defconst xim:highlight #x000004)
(defconst xim:primary #x000008)
(defconst xim:secondary #x000010)
(defconst xim:tertiary #x000020)
(defconst xim:visible-to-forward #x000040)
(defconst xim:visible-to-backward #x000080)
(defconst xim:visible-center #x000100)

(xcb:deftypealias 'xim:XIMHOTKEYSTATE 'xcb:CARD32)

(defconst xim:hot-key-state:on #x0000001)
(defconst xim:hot-key-state:off #x0000002)

(xcb:deftypealias 'xim:XIMPREEDITSTATE 'xcb:CARD32)

(defconst xim:preedit:enable #x0000001)
(defconst xim:preedit:disable #x0000002)

(xcb:deftypealias 'xim:XIMRESETSTATE 'xcb:CARD32)

(defconst xim:initial-state #x0000001)
(defconst xim:preserve-state #x0000002)

;;;; Error notification

(defclass xim:error (xim:-request)
  ((~major-opcode :initform xim:opcode:error)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (flag :initarg :flag :type xim:BITMASK16)
   (error-code :initarg :error-code :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (type :initarg :type :type xcb:CARD16)
   (detail :initarg :detail :type xcb:-ignore)
   (detail~ :initform '(name detail type xcb:char
                             size (xcb:-fieldref 'length)) :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(defconst xim:error-flag:invalid-both 0)
(defconst xim:error-flag:invalid-im-id 1)
(defconst xim:error-flag:invalid-ic-id 2)

(defconst xim:error-code:bad-alloc 1)
(defconst xim:error-code:bad-style 2)
(defconst xim:error-code:bad-client-window 3)
(defconst xim:error-code:bad-focus-window 4)
(defconst xim:error-code:bad-area 5)
(defconst xim:error-code:bad-spot-location 6)
(defconst xim:error-code:bad-colormap 7)
(defconst xim:error-code:bad-atom 8)
(defconst xim:error-code:bad-pixel 9)
(defconst xim:error-code:bad-pixmap 10)
(defconst xim:error-code:bad-name 11)
(defconst xim:error-code:bad-cursor 12)
(defconst xim:error-code:bad-protocol 13)
(defconst xim:error-code:bad-foreground 14)
(defconst xim:error-code:bad-background 15)
(defconst xim:error-code:locale-not-supported 16)
(defconst xim:error-code:bad-something 999)

;;;; Connection establishment

(defclass xim:connect (xim:-request)
  ((~major-opcode :initform xim:opcode:connect)
   (byte-order :initarg :byte-order :type xcb:-u1)
   (pad~0 :initform 1 :type xcb:-pad)
   (major-version :initarg :major-version :type xcb:CARD16)
   (minor-version :initarg :minor-version :type xcb:CARD16)
   (number :initarg :number :type xcb:CARD16)
   (auth-names :initarg :auth-names :type xcb:-ignore)
   (auth-names~ :initform '(name auth-names type xim:STRING
                                 size (xcb:-fieldref 'number))
                :type xcb:-list)))

(defconst xim:connect-byte-order:msb-first #x42)
(defconst xim:connect-byte-order:lsb-first #x6c)

(defclass xim:auth-required (xim:-request)
  ((~major-opcode :initform xim:opcode:auth-required)
   (index :initarg :index :type xcb:CARD8)
   (pad~0 :initform 3 :type xcb:-pad)
   (length :initarg :length :type xcb:-u2)
   (pad~1 :initform 2 :type xcb:-pad)
   (data :initarg :data :type xcb:-ignore)
   (data~ :initform '(name data type xcb:void size (xcb:-fieldref 'length))
          :type xcb:-list)
   (pad~1 :initform '(xim:PADDING (slot-value length)) :type xcb:-pad)))

(defclass xim:auth-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:auth-reply)
   (length :initarg :length :type xcb:-u2)
   (pad~0 :initform 2 :type xcb:-pad)
   (data :initarg :data :type xcb:-ignore)
   (data~ :initform '(name data type xcb:void size (xcb:-fieldref 'length))
          :type xcb:-list)
   (pad~1 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(defclass xim:auth-next (xim:-request)
  ((~major-opcode :initform xim:opcode:auth-next)
   (length :initarg :length :type xcb:-u2)
   (pad~0 :initform 2 :type xcb:-pad)
   (data :initarg :data :type xcb:-ignore)
   (data~ :initform '(name data type xcb:void size (xcb:-fieldref 'length))
          :type xcb:-list)
   (pad~1 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(defclass xim:auth-setup (xim:-request)
  ((~major-opcode :initform xim:opcode:auth-setup)
   (number :initarg :number :type xcb:CARD16)
   (pad~0 :initform 2 :type xcb:-pad)
   (names :initarg :names :type xcb:-ignore)
   (names~ :initform '(name names type xim:STRING size (xcb:-fieldref 'number))
           :type xcb:-list)))

(defclass xim:auth-ng (xim:-request)
  ((~major-opcode :initform xim:opcode:auth-ng)))

(defclass xim:connect-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:connect-reply)
   ;; Default to version 1.0
   (major-version :initarg :major-version :initform 1 :type xcb:CARD16)
   (minor-version :initarg :minor-version :initform 0 :type xcb:CARD16)))

(defclass xim:disconnect (xim:-request)
  ((~major-opcode :initform xim:opcode:disconnect)))

(defclass xim:disconnect-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:disconnect-reply)))

(defclass xim:open (xim:-request)
  ((~major-opcode :initform xim:opcode:open)
   (locale-name :initarg :locale-name :type xim:STR)
   (pad~0 :initform '(xim:PADDING (1+ (slot-value (xcb:-fieldref 'locale-name)
                                                  'length)))
          :type xcb:-pad)))

(defclass xim:open-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:open-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (im-attrs-length :initarg :im-attrs-length :type xcb:-u2)
   (im-attrs :initarg :im-attrs :type xcb:-ignore)
   (im-attrs~ :initform '(name im-attrs type xcb:void
                               size (xcb:-fieldref 'im-attrs-length))
              :type xcb:-list)
   (ic-attrs-length :initarg :ic-attrs-length :type xcb:-u2)
   (pad~0 :initform 2 :type xcb:-pad)
   (ic-attrs :initarg :ic-attrs :type xcb:-ignore)
   (ic-attrs~ :initform '(name ic-attrs type xcb:void
                               size (xcb:-fieldref 'ic-attrs-length))
              :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:open-reply))
  (let ((im-attrs (mapconcat #'xcb:marshal (slot-value obj 'im-attrs) []))
        (ic-attrs (mapconcat #'xcb:marshal (slot-value obj 'ic-attrs) [])))
    (setf (slot-value obj 'im-attrs-length) (length im-attrs)
          (slot-value obj 'im-attrs) im-attrs
          (slot-value obj 'ic-attrs-length) (length ic-attrs)
          (slot-value obj 'ic-attrs) ic-attrs)
    (cl-call-next-method obj)))

(cl-defmethod xcb:unmarshal ((obj xim:open-reply) byte-array)
  (let ((retval (cl-call-next-method obj byte-array))
        (im-data (slot-value obj 'im-attrs))
        (ic-data (slot-value obj 'ic-attrs))
        im-attr im-attrs ic-attr ic-attrs)
    (while (< 0 (length im-data))
      (setq im-attr (make-instance 'xim:XIMATTR)
            im-data (substring im-data (xcb:unmarshal im-attr im-data))
            im-attrs (nconc im-attrs (list im-attr))))
    (while (< 0 (length ic-data))
      (setq ic-attr (make-instance 'xim:XICATTR)
            ic-data (substring ic-data (xcb:unmarshal ic-attr ic-data))
            ic-attrs (nconc ic-attrs (list ic-attr))))
    (setf (slot-value obj 'im-attrs) im-attrs
          (slot-value obj 'ic-attrs) ic-attrs)
    retval))

(defclass xim:close (xim:-request)
  ((~major-opcode :initform xim:opcode:close)
   (im-id :initarg :im-id :type xcb:CARD16)
   (pad~0 :initform 2 :type xcb:-pad)))

(defclass xim:close-reply (xim:close)
  ((~major-opcode :initform xim:opcode:close-reply)))

;;;; Event flow control

(defclass xim:set-event-mask (xim:-request)
  ((~major-opcode :initform xim:opcode:set-event-mask)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (forward-event-mask :initarg :forward-event-mask :type xcb:-u4)
   (synchronous-event-mask :initarg :synchronous-event-mask :type xcb:-u4)))

(defclass xim:register-triggerkeys (xim:-request)
  ((~major-opcode :initform xim:opcode:register-triggerkeys)
   (im-id :initarg :im-id :type xcb:CARD16)
   (pad~0 :initform 2 :type xcb:-pad)
   (on-keys-length :initarg :on-keys-length :type xcb:-u4)
   (on-keys :initarg :on-keys :type xcb:-ignore)
   (on-keys~ :initform '(name on-keys type xim:XIMTRIGGERKEY
                              size (/ (xcb:-fieldref 'on-keys-length) 12))
             :type xcb:-list)
   (off-keys-length :initarg :off-keys-length :type xcb:-u4)
   (off-keys :initarg :off-keys :type xcb:-ignore)
   (off-keys~ :initform '(name off-keys type xim:XIMTRIGGERKEY
                               size (/ (xcb:-fieldref 'off-keys-length) 12))
              :type xcb:-list)))

(defclass xim:trigger-nofity (xim:-request)
  ((~major-opcode :initform xim:opcode:trigger-notify)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (flag :initarg :flag :type xcb:CARD32)
   (index :initarg :index :type xcb:CARD32)
   (client-select-event-mask :initarg :client-select-event-mask
                             :type xcb:-u4)))

(defconst xim:trigger-nofity-flag:on-keys 0)
(defconst xim:trigger-nofity-flag:off-keys 1)

(defclass xim:trigger-nofity-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:trigger-notify-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Encoding negotiation

(defclass xim:encoding-negotiation (xim:-request)
  ((~major-opcode :initform xim:opcode:encoding-negotiation)
   (im-id :initarg :im-id :type xcb:CARD16)
   (names-length :initarg :names-length :type xcb:-u2)
   (names :initarg :names :type xcb:-ignore)
   (names~ :initform '(name names type xcb:void
                            size (xcb:-fieldref 'names-length))
           :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (xcb:-fieldref 'names-length))
          :type xcb:-pad)
   (encodings-length :initarg :encoding-length :type xcb:-u2)
   (pad~1 :initform 2 :type xcb:-pad)
   (encodings :initarg :encodings :type xcb:-ignore)
   (encodings~ :initform '(name encodings type xcb:void
                                size (xcb:-fieldref 'encodings-length))
               :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:encoding-negotiation))
  (let ((names (mapconcat #'xcb:marshal (slot-value obj 'names) []))
        (encodings (mapconcat #'xcb:marshal (slot-value obj 'encodings) [])))
    (setf (slot-value obj 'names-length) (length names)
          (slot-value obj 'names) names
          (slot-value obj 'encodings-length) (length encodings)
          (slot-value obj 'encodings) encodings)
    (cl-call-next-method obj)))

(cl-defmethod xcb:unmarshal ((obj xim:encoding-negotiation) byte-array)
  (let ((retval (cl-call-next-method obj byte-array))
        (names-data (slot-value obj 'names))
        (encodings-data (slot-value obj 'encodings))
        name names encoding encodings)
    (while (< 0 (length names-data))
      (setq name (make-instance 'xim:STR)
            names-data (substring names-data (xcb:unmarshal name names-data))
            names (nconc names (list name))))
    (while (< 0 (length encodings-data))
      (setq encoding (make-instance 'xim:ENCODINGINFO)
            encodings-data
            (substring encodings-data (xcb:unmarshal encoding encodings-data))
            encodings (nconc encodings (list encoding))))
    (setf (slot-value obj 'names) names
          (slot-value obj 'encodings) encodings)
    retval))

(defclass xim:encoding-negotiation-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:encoding-negotiation-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (category :initarg :category :type xcb:CARD16)
   (index :initarg :index :type xcb:INT16)
   (pad~0 :initform 2 :type xcb:-pad)))

(defconst xim:encoding-negotiation-reply-category:name 0)
(defconst xim:encoding-negotiation-reply-category:data 1)

;;;; Query the supported extension protocol list

(defclass xim:query-extension (xim:-request)
  ((~major-opcode :initform xim:opcode:query-extension)
   (im-id :initarg :im-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (extensions :initarg :extensions :type xcb:-ignore)
   (extensions~ :initform '(name extensions type xcb:void
                                 size (xcb:-fieldref 'length))
                :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(cl-defmethod xcb:marshal ((obj xim:query-extension))
  (let ((extensions (mapconcat #'xcb:marshal (slot-value obj 'extensions) [])))
    (setf (slot-value obj 'length) (length extensions)
          (slot-value obj 'extensions) extensions)
    (cl-call-next-method obj)))

(cl-defmethod xcb:unmarshal ((obj xim:query-extension) byte-array)
  (let ((retval (cl-call-next-method obj byte-array))
        (data (slot-value obj 'extensions))
        extension extensions)
    (while (< 0 (length data))
      (setq extension (make-instance 'xim:STR)
            data (substring data (xcb:unmarshal extension data))
            extensions (nconc extensions (list extension))))
    (setf (slot-value obj 'extensions) extensions)
    retval))

(defclass xim:query-extension-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:query-extension-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (extensions :initarg :extensions :type xcb:-ignore)
   (extensions~ :initform '(name extensions type xcb:void
                                 size (xcb:-fieldref 'length))
                :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:query-extension-reply))
  (let ((extensions (mapconcat 'xcb:marshal (slot-value obj 'extensions) [])))
    (setf (slot-value obj 'length) (length extensions)
          (slot-value obj 'extensions) extensions)
    (cl-call-next-method obj)))

(cl-defmethod xcb:unmarshal ((obj xim:query-extension-reply) byte-array)
  (let ((retval (cl-call-next-method obj byte-array))
        (data (slot-value obj 'extensions))
        extension extensions)
    (while (< 0 (length data))
      (setq extension (make-instance 'xim:EXT)
            data (substring data (xcb:unmarshal extension data))
            extensions (nconc extensions (list extension))))
    (setf (slot-value obj 'extensions) extensions)
    retval))

;;;; Setting IM values

(defclass xim:set-im-values (xim:-request)
  ((~major-opcode :initform xim:opcode:set-im-values)
   (im-id :initarg :im-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (im-attributes :initarg :im-attributes :type xcb:-ignore)
   (im-attributes~ :initform '(name im-attributes type xcb:void
                                    size (xcb:-fieldref 'length))
                   :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:set-im-values))
  (let ((im-attributes (mapconcat #'xcb:marshal
                                  (slot-value obj 'im-attributes) [])))
    (setf (slot-value obj 'length) (length im-attributes)
          (slot-value obj 'im-attributes) im-attributes)
    (cl-call-next-method obj)))

(defclass xim:set-im-values-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:set-im-values-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (pad~0 :initform 2 :type xcb:-pad)))

;;;; Getting IM values

(defclass xim:get-im-values (xim:-request)
  ((~major-opcode :initform xim:opcode:get-im-values)
   (im-id :initarg :im-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (im-attributes-id :initarg :im-attributes-id :type xcb:-ignore)
   (im-attributes-id~ :initform '(name im-attributes-id type xcb:CARD16
                                       size (/ (xcb:-fieldref 'length) 2))
                      :type xcb:-list)))

(defclass xim:get-im-values-reply (xim:set-im-values)
  ((~major-opcode :initform xim:opcode:get-im-values-reply)))

;;;; Creating an IC

(defclass xim:create-ic (xim:-request)
  ((~major-opcode :initform xim:opcode:create-ic)
   (im-id :initarg :im-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (ic-attributes :initarg :ic-attributes :type xcb:-ignore)
   (ic-attributes~ :initform '(name ic-attributes type xcb:void
                                    size (xcb:-fieldref 'length))
                   :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:create-ic))
  (let ((ic-attributes (mapconcat #'xcb:marshal
                                  (slot-value obj 'ic-attributes) [])))
    (setf (slot-value obj 'length) (length ic-attributes)
          (slot-value obj 'ic-attributes) ic-attributes)
    (cl-call-next-method obj)))

(cl-defmethod xcb:unmarshal ((obj xim:create-ic) byte-array)
  (let ((retval (cl-call-next-method obj byte-array))
        (data (slot-value obj 'ic-attributes))
        ic-attribute ic-attributes)
    (while (< 0 (length data))
      (setq ic-attribute (make-instance 'xim:XICATTRIBUTE)
            data (substring data (xcb:unmarshal ic-attribute data))
            ic-attributes (nconc ic-attributes (list ic-attribute))))
    (setf (slot-value obj 'ic-attributes) ic-attributes)
    retval))

(defclass xim:create-ic-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:create-ic-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Destroy the IC

(defclass xim:destroy-ic (xim:-request)
  ((~major-opcode :initform xim:opcode:destroy-ic)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

(defclass xim:destroy-ic-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:destroy-ic-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Setting IC values

(defclass xim:set-ic-values (xim:-request)
  ((~major-opcode :initform xim:opcode:set-ic-values)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (pad~0 :initform 2 :type xcb:-pad)
   (ic-attributes :initarg :ic-attributes :type xcb:-ignore)
   (ic-attributes~ :initform '(name ic-attributes type xcb:void
                                    size (xcb:-fieldref 'length))
                   :type xcb:-list)))

(cl-defmethod xcb:marshal ((obj xim:set-ic-values))
  (let ((ic-attributes (mapconcat 'xcb:marshal
                                  (slot-value obj 'ic-attributes) [])))
    (setf (slot-value obj 'length) (length ic-attributes)
          (slot-value obj 'ic-attributes) ic-attributes)
    (cl-call-next-method obj)))

(defclass xim:set-ic-values-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:set-ic-values-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Getting IC values

(defclass xim:get-ic-values (xim:-request)
  ((~major-opcode :initform xim:opcode:get-ic-values)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (ic-attributes-id :initarg :ic-attributes-id :type xcb:-ignore)
   (ic-attributes-id~ :initform '(name ic-attributes-id type xcb:CARD16
                                       size (/ (xcb:-fieldref 'length) 2))
                      :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

(defclass xim:get-ic-values-reply (xim:set-ic-values)
  ((~major-opcode :initform xim:opcode:get-ic-values-reply)))

;;;; Setting IC focus

(defclass xim:set-ic-focus (xim:-request)
  ((~major-opcode :initform xim:opcode:set-ic-focus)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Unsetting IC focus

(defclass xim:unset-ic-focus (xim:-request)
  ((~major-opcode :initform xim:opcode:unset-ic-focus)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Filtering events

(defclass xim:forward-event (xim:-request)
  ((~major-opcode :initform xim:opcode:forward-event)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (flag :initarg :flag :type xim:BITMASK16)
   (serial-number :initarg :serial-number :type xcb:CARD16)
   (event :initarg :event :type xcb:-ignore)
   (event~ :initform '(name event type xcb:void size 32) :type xcb:-list)))

(defconst xim:forward-event-flag:synchronous 1)
(defconst xim:forward-event-flag:request-filtering 2)
(defconst xim:forward-event-flag:request-lookupstring 4)

;;;; Synchronizing with the IM server

(defclass xim:sync (xim:-request)
  ((~major-opcode :initform xim:opcode:sync)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

(defclass xim:sync-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:sync-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;;;; Sending a committed string

(defclass xim:commit (xim:-request)
  ((~major-opcode :initform xim:opcode:commit)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (flag :initarg :flag :type xim:BITMASK16)))

(defconst xim:commit-flag:synchronous 1)
;; FIXME: different from Xlib:XLookup*
(defconst xim:commit-flag:x-lookup-chars 2)
(defconst xim:commit-flag:x-lookup-key-sym 4)
(defconst xim:commit-flag:x-lookup-both 6)

(defclass xim:commit-x-lookup-key-sym (xim:commit)
  ((flag :initform xim:commit-flag:x-lookup-key-sym)
   (pad~0 :initform 2 :type xcb:-pad)
   (key-sym :initarg :key-sym :type xcb:KEYSYM)))

(defclass xim:commit-x-lookup-chars (xim:commit)
  ((flag :initform xim:commit-flag:x-lookup-chars)
   (length :initarg :length :type xcb:-u2)
   (string :initarg :string :type xcb:-ignore)
   (string~ :initform '(name string type xcb:BYTE size (xcb:-fieldref 'length))
            :type xcb:-list)
   (pad~1 :initform '(xim:PADDING (xcb:-fieldref 'length)) :type xcb:-pad)))

(defclass xim:commit-x-lookup-both (xim:commit-x-lookup-key-sym
                                    xim:commit-x-lookup-chars)
  ((flag :initform xim:commit-flag:x-lookup-both)
   (pad~1 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

;;;; Reset IC

(defclass xim:reset-ic (xim:-request)
  ((~major-opcode :initform xim:opcode:reset-ic)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

(defclass xim:reset-ic-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:reset-ic-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (length :initarg :length :type xcb:-u2)
   (string :initarg :string :type xcb:-ignore)
   (string~ :initform '(name string type xcb:BYTE size (xcb:-fieldref 'length))
            :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'length)))
          :type xcb:-pad)))

;;;; Callbacks

;; Negotiating geometry
(defclass xim:geometry (xim:-request)
  ((~major-opcode :initform xim:opcode:geometry)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;; Converting a string
(defclass xim:str-conversion (xim:-request)
  ((~major-opcode :initform xim:opcode:str-conversion)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (position :initarg :position :type xcb:CARD16)
   (pad~0 :initform 2 :type xcb:-pad)
   (direction :initarg :direction :type xcb:CARD32)
   (factor :initarg :factor :type xcb:CARD16)
   (operation :initarg :operation :type xcb:CARD16)
   (length :initarg :length :type xcb:INT16)))

(defconst xim:caret-direction:forward-char 0)
(defconst xim:caret-direction:backward-char 1)
(defconst xim:caret-direction:forward-word 2)
(defconst xim:caret-direction:backward-word 3)
(defconst xim:caret-direction:caret-up 4)
(defconst xim:caret-direction:caret-down 5)
(defconst xim:caret-direction:next-line 6)
(defconst xim:caret-direction:previous-line 7)
(defconst xim:caret-direction:line-start 8)
(defconst xim:caret-direction:line-end 9)
(defconst xim:caret-direction:absolute-position 10)
(defconst xim:caret-direction:dont-change 11)

(defconst xim:string-conversion-operation:substitution 1)
(defconst xim:string-conversion-operation:retrieval 2)

(defclass xim:str-conversion-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:str-conversion-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (feedback :initarg :feedback :type xcb:CARD32)
   (text :initarg :text :type xim:XIMSTRCONVTEXT)))

;; Preedit callbacks
(defclass xim:preedit-start (xim:-request)
  ((~major-opcode :initform xim:opcode:preedit-start)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

(defclass xim:preedit-start-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:preedit-start-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (return-value :initarg :return-value :type xcb:INT32)))

(defclass xim:preedit-draw (xim:-request)
  ((~major-opcode :initform xim:opcode:preedit-draw)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (caret :initarg :caret :type xcb:INT32)
   (chg-first :initarg :chg-first :type xcb:INT32)
   (chg-length :initarg :chg-length :type xcb:INT32)
   (status :initarg :status :type xim:BITMASK32)
   (string-length :initarg :string-length :type xcb:-u2)
   (string :initarg :string :type xcb:-ignore)
   (string~ :initform '(name string type xcb:char
                             size (xcb:-fieldref 'string-length))
            :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'string-length)))
          :type xcb:-pad)
   (feedback-length :initarg :feedback-length :type xcb:-u2)
   (pad~1 :initform 2 :type xcb:-pad)
   (feedback :initarg :feedback :type xcb:-ignore)
   (feedback~ :initform '(name feedback type xim:XIMFEEDBACK
                               size (/ (xcb:-fieldref 'feedback-length) 4))
              :type xcb:-list)))

(defconst xim:preedit-draw-status:no-string 1)
(defconst xim:preedit-draw-status:no-feedback 2)

(defclass xim:preedit-caret (xim:-request)
  ((~major-opcode :initform xim:opcode:preedit-caret)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (position :initarg :position :type xcb:INT32)
   (direction :initarg :direction :type xcb:CARD32)
   (style :initarg :style :type xcb:CARD32)))

(defconst xim:preedit-caret-style:invisible 0)
(defconst xim:preedit-caret-style:primary 1)
(defconst xim:preedit-caret-style:secondary 2)

(defclass xim:preedit-caret-reply (xim:-request)
  ((~major-opcode :initform xim:opcode:preedit-caret-reply)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (position :initarg :position :type xcb:CARD32)))

(defclass xim:preedit-done (xim:-request)
  ((~major-opcode :initform xim:opcode:preedit-done)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

;; Preedit state notify
(defclass xim:preeditstate (xim:-request)
  ((~major-opcode :initform xim:opcode:preeditstate)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (state :initarg :state :type xim:BITMASK32)))

(defconst xim:preeditstate:unknown 0)
(defconst xim:preeditstate:enable 1)
(defconst xim:preeditstate:disable 2)

;; Status callbacks
(defclass xim:status-start (xim:-request)
  ((~major-opcode :initform xim:opcode:status-start)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))

(defclass xim:status-draw (xim:-request)
  ((~major-opcode :initform xim:opcode:status-draw)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)
   (type :initarg :type :type xcb:CARD32)))

(defconst xim:status-draw-type:text 0)
(defconst xim:status-draw-type:bitmap 1)

(defclass xim:status-draw-text (xim:status-draw)
  ((type :initarg :type :initform xim:status-draw-type:text)
   (status :initarg :status :type xim:BITMASK32)
   (string-length :initarg :string-length :type xcb:-u2)
   (string :initarg :string :type xcb:-ignore)
   (string~ :initform '(name string type xcb:char
                             size (xcb:-fieldref 'string-lessp))
            :type xcb:-list)
   (pad~0 :initform '(xim:PADDING (+ 2 (xcb:-fieldref 'string-length)))
          :type xcb:-pad)
   (feedback-length :initarg :feedback-length :type xcb:-u2)
   (pad~1 :initform 2 :type xcb:-pad)
   (feedback :initarg :feedback :type xcb:-ignore)
   (feedback~ :initform '(name feedback type xim:XIMFEEDBACK
                               size (/ (xcb:-fieldref 'feedback-length) 4))
              :type xcb:-list)))

(defclass xim:status-draw-bitmap (xim:status-draw)
  ((type :initarg :type :initform xim:status-draw-type:bitmap)
   (pixmap-data :initarg :pixmap-data :type xcb:PIXMAP)))

(defclass xim:status-done (xim:-request)
  ((~major-opcode :initform xim:opcode:status-done)
   (im-id :initarg :im-id :type xcb:CARD16)
   (ic-id :initarg :ic-id :type xcb:CARD16)))



(provide 'xcb-xim)

;;; xcb-xim.el ends here
