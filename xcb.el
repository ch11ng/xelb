;;; xcb.el --- X Protocol Emacs Lisp Binding (XELB)  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Keywords: unix

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library mainly provides methods for `xcb:connection', a opaque class
;; encapsulating all information concerning an X connection. The most frequently
;; used methods are:
;; + Open/Close connection
;;   - `xcb:connect'
;;   - `xcb:connect-to-socket'
;;   - `xcb:disconnect'
;; + Request/Reply/Error (asynchronous)
;;   - `xcb:+request'
;;   - `xcb:+request-checked'
;;   - `xcb:+request-unchecked'
;;   - `xcb:+reply'
;;   - `xcb:request-check'
;; + Request/Reply/Error (synchronous)
;;   - `xcb:+request+reply'
;;   - `xcb:+request-checked+request-check'
;;   - `xcb:+request-unchecked+reply'
;; + Event handling
;;   - `xcb:+event'
;; + Misc.
;;   - `xcb:flush'
;;   - `xcb:generate-id'
;; Please refer to their documentations for more details.

;; Todo:
;; + Authentication support when connecting to X server.
;; + Use XC-MISC extension for `xcb:generate-id' when IDs are used up.

;; References:
;; + X protocol (http://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.txt)

;;; Code:

(require 'xcb-xproto)

(defvar xcb:debug-on nil "non-nil to turn on debug.")

(defmacro xcb:-log (format-string &rest args)
  "Print debug info."
  (when xcb:debug-on
    `(message (concat "[XELB LOG] " ,format-string) ,@args)))

(defvar xcb:connection-timeout 10 "Connection timeout.")

;;;; X connection related

(defclass xcb:connection ()
  ((process :initarg :process)
   (connected :initform nil)  ;non-nil indicates connected to X server
   (display :initarg :display)
   (auth-info :initarg :auth-info)
   (socket :initarg :socket)
   (lock :initform nil)
   (setup-data :initform nil)           ;X connection setup data
   (request-cache :initform [])         ;cache for outgoing requests
   (message-cache :initform [])         ;cache for incoming messages
   (event-lock :initform nil)
   (event-queue :initform nil)
   (error-plist :initform nil)
   (reply-plist :initform nil)
   (event-plist :initform nil)
   (extension-plist :initform nil)
   (extension-opcode-plist :initform nil)
   (extension-first-error-alist :initform nil)
   (extension-first-event-alist :initform nil)
   (request-sequence :initform 0)
   (error-sequence :initform 0)
   (reply-sequence :initform 0)
   (xid :initform 0)            ;last used X resource ID
   (extra-plist :initform nil)) ;for storing extra data (e.g. by extensions)
  :documentation "X connection.")

(defclass xcb:auth-info ()
  ((name :initarg :name :initform "" :type string)
   (data :initarg :data :initform "" :type string))
  :documentation "X connection authentication info.")

(defun xcb:connect (&optional display screen)
  "Connect to X server with display DISPLAY on screen SCREEN."
  (xcb:connect-to-display-with-auth-info display nil screen))

(defun xcb:connect-to-display-with-auth-info (&optional display auth screen)
  "Connect to X server with display DISPLAY, authentication info AUTH on screen
SCREEN."
  (unless display (setq display x-display-name))
  (unless display (error "[XELB] No X display available"))
  (let* ((tmp (xcb:parse-display display))
         (host (cdr (assoc 'host tmp)))
         (host (if (string= "" host) 'local host))
         (dpy (cdr (assoc 'display tmp)))
         (screen (if screen screen (cdr (assoc 'screen tmp))))
         (process (make-network-process :name "XELB"
                                        :host host
                                        :service (+ 6000 dpy)))
         (auth-info (if auth auth (make-instance 'xcb:auth-info)))
         (connection (make-instance 'xcb:connection
                                    :process process
                                    :display display :auth-info auth-info)))
    (xcb:-connect connection)
    connection))

(defun xcb:parse-display (name)
  "Parse X Display name NAME."
  (let ((host (replace-regexp-in-string "\\(.*\\):.*" "\\1" name))
        (display
         (replace-regexp-in-string ".*:\\([^\\.]+\\)\\(\\..*\\)?" "\\1" name))
        (screen
         (replace-regexp-in-string ".*:[^\\.]+\\.?\\(.*\\)?" "\\1" name)))
    (setq display (string-to-int display))
    (setq screen (if (string= "" screen) 0 (string-to-int screen)))
    `((host . ,host) (display . ,display) (screen . ,screen))))

(defun xcb:connect-to-socket (&optional socket auth-info)
  "Connect to X server with socket SOCKET and authentication info AUTH-INFO."
  (unless (or socket x-display-name) (error "[XELB] No X display available"))
  (let (display)
    (if socket
        ;; As there is no general way to deduce the display name from an X11
        ;; socket, we assume a standard SOCKET name and hope for the best.
        (setq display
              (concat ":"               ;local
                      (replace-regexp-in-string "^.*?\\([0-9.]+\\)$" "\\1"
                                                socket)))
      (setq display x-display-name
            socket (concat "/tmp/.X11-unix/X"
                           (replace-regexp-in-string
                            ".*:\\([^\\.]+\\)\\(\\..*\\)?" "\\1"
                            x-display-name))))
    (let* ((process (make-network-process :name "XELB" :remote socket))
           (auth (if auth-info auth-info (make-instance 'xcb:auth-info)))
           (connection (make-instance 'xcb:connection
                                      :process process :display display
                                      :auth-info auth :socket socket)))
      (xcb:-connect connection)
      connection)))

(cl-defmethod xcb:-connect ((obj xcb:connection))
  "Connect to X server."
  (let* ((process (slot-value obj 'process))
         (auth-info (slot-value obj 'auth-info))
         (aname (slot-value auth-info 'name))
         (adata (slot-value auth-info 'data)))
    (set-process-plist process
                       (plist-put (process-plist process) 'connection obj))
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process 'xcb:-connection-filter)
    (process-send-string                ;send setup packet
     process
     (apply 'unibyte-string
            (append ;convert vector to string
             (xcb:marshal
              (make-instance 'xcb:SetupRequest
                             :byte-order (if xcb:lsb #x6c #x42)
                             :protocol-major-version 11
                             :protocol-minor-version 0
                             :authorization-protocol-name-len (length aname)
                             :authorization-protocol-data-len (length adata)
                             :authorization-protocol-name aname
                             :authorization-protocol-data adata))
             nil)))
    ;; Wait for setup data ready
    (with-timeout (xcb:connection-timeout (xcb:disconnect obj)
                               (error "[XELB] Connection timeout"))
      (while (not (slot-value obj 'setup-data))
        (accept-process-output process 1)))))

(defun xcb:-connection-filter (process message)
  "Filter function for an X connection.

Concurrency is disabled as it breaks the orders of errors, replies and events."
  (let* ((connection (plist-get (process-plist process) 'connection))
         (cache (vconcat (slot-value connection 'message-cache) message))
         (cache-length (length cache)))
    (setf (slot-value connection 'message-cache) cache)
    (catch 'return
      ;; Queue message when locked
      (when (slot-value connection 'lock)
        (throw 'return 'lock))
      ;; Start parsing message
      (setf (slot-value connection 'lock) t)
      ;; Connection setup
      (unless (slot-value connection 'connected)
        (when (<= 8 (length cache)) ;at least setup header is available
          (let ((data-len (+ 8 (* 4 (funcall (if xcb:lsb 'xcb:-unpack-u2-lsb
                                               'xcb:-unpack-u2)
                                             cache 6))))
                obj)
            (when (>= (length cache) data-len)
              (xcb:-log "Setup response: %s" cache)
              (pcase (elt cache 0)
                (0                      ;failed
                 (setq obj (make-instance 'xcb:SetupFailed))
                 (xcb:unmarshal obj cache)
                 (setq cache (substring cache data-len))
                 (error "[XELB] Connection failed: %s"
                        (slot-value obj 'reason)))
                (1                      ;success
                 (setq obj (make-instance 'xcb:Setup))
                 (xcb:unmarshal obj cache)
                 (setq cache (substring cache data-len))
                 (setf (slot-value connection 'setup-data) obj)
                 (setf (slot-value connection 'connected) t))
                (2                      ;authentication
                 (setq obj (make-instance 'xcb:SetupAuthenticate))
                 (xcb:unmarshal obj cache)
                 (setq cache (substring cache data-len))
                 (error "[XELB] Authentication not supported: %s"
                        (slot-value obj 'reason)))
                (x (error "Unrecognized setup status: %d" x))))))
        (setf (slot-value connection 'lock) nil)
        (throw 'return 'setup))
      ;; Process error/reply/event
      (catch 'break
        (while (<= 32 (length cache))
          (pcase (elt cache 0)
            (0                          ;error
             (xcb:-log "Error received: %s" (substring cache 0 32))
             (let ((sequence (funcall (if xcb:lsb 'xcb:-unpack-u2-lsb
                                        'xcb:-unpack-u2)
                                      cache 2))
                   (plist (slot-value connection 'error-plist))
                   struct)
               (when (plist-member plist sequence)
                 (setq struct (plist-get plist sequence))
                 (setf (slot-value connection 'error-plist)
                       (plist-put plist sequence
                                  (push `(,(elt cache 1) .
                                          ,(substring cache 0 32))
                                        struct))))
               (setq cache (substring cache 32))
               (setf (slot-value connection 'error-sequence) sequence)))
            (1                          ;reply
             (let* ((reply-words (funcall (if xcb:lsb 'xcb:-unpack-u4-lsb
                                            'xcb:-unpack-u4)
                                          cache 4))
                    (reply-length (+ 32 (* 4 reply-words)))
                    struct sequence plist)
               (when (< (length cache) reply-length) ;too short, do next time
                 (throw 'break nil))
               (xcb:-log "Reply received: %s" (substring cache 0 reply-length))
               (setq sequence (funcall (if xcb:lsb 'xcb:-unpack-u2-lsb
                                         'xcb:-unpack-u2)
                                       cache 2))
               (setq plist (slot-value connection 'reply-plist))
               (setq struct (plist-get plist sequence))
               (when struct
                 (setf (slot-value connection 'reply-plist)
                       (plist-put plist sequence
                                  (if (symbolp struct)
                                      ;; Single reply or
                                      ;; first reply for multiple replies
                                      (list struct
                                            (substring cache 0 reply-length))
                                    ;; Multiple replies
                                    `(,(car struct) ,@(cdr struct)
                                      ,(substring cache 0 reply-length))))))
               (setq cache (substring cache reply-length))
               (setf (slot-value connection 'reply-sequence) sequence)))
            (x                          ;event
             (xcb:-log "Event received: %s" (substring cache 0 32))
             (let (synthetic listener)
               (when (/= 0 (logand x #x80)) ;synthetic event
                 (setq synthetic t
                       x (logand x #x7f))) ;low 7 bits is the event number
               (setq listener
                     (plist-get (slot-value connection 'event-plist) x))
               (when listener
                 (with-slots (event-queue) connection
                   (setf event-queue (nconc event-queue
                                            `([,listener
                                               ,(substring cache 0 32)
                                               ,synthetic]))))))
             (setq cache (substring cache 32))))))
      (setf (slot-value connection 'lock) nil))
    (unless (slot-value connection 'lock)
      (with-slots (message-cache) connection
        (let ((current-cache-length (length message-cache)))
          (setf message-cache
                (substring message-cache (- cache-length (length cache))))
          (when (/= current-cache-length cache-length)
            (xcb:-connection-filter process []))))
      (with-slots (event-lock event-queue) connection
        (unless event-lock
          (setf event-lock t)
          (let (event data synthetic)
            (while (setq event (pop event-queue))
              (setq data (elt event 1)
                    synthetic (elt event 2))
              (dolist (listener (elt event 0))
                (funcall listener data synthetic))))
          (setf event-lock nil))))))

(cl-defmethod xcb:disconnect ((obj xcb:connection))
  "Disconnect from X server."
  (delete-process (slot-value obj 'process))
  ;; Reset every slot to its default value
  (let ((slots (eieio-class-slots 'xcb:connection)))
    (dolist (slot slots)
      (setf (slot-value obj (cl--slot-descriptor-name slot))
            (cl--slot-descriptor-initform slot)))))

;;;; Other routines

(cl-defmethod xcb:get-setup ((obj xcb:connection))
  "Get the setup info of X connection OBJ."
  (slot-value obj 'setup-data))

(cl-defmethod xcb:get-socket ((obj xcb:connection))
  "Get the socket of X connection OBJ."
  (slot-value obj 'socket))

(cl-defmethod xcb:get-maximum-request-length ((obj xcb:connection))
  "Get maximum request length from setup data."
  (slot-value (xcb:get-setup obj) 'maximum-request-length))

(cl-defmethod xcb:+event ((obj xcb:connection) event listener)
  "attach EVENT LISTENER

Note that event listeners attached this way are shared with the super- and sub-
classes of EVENT (since they have the same event number)."
  (let* ((event-number (xcb:-error-or-event-class->number obj event))
         (plist (slot-value obj 'event-plist))
         (listeners (plist-get plist event-number)))
    (setf (slot-value obj 'event-plist)
          (plist-put plist event-number (append listeners (list listener))))))

(cl-defmethod xcb:flush ((obj xcb:connection))
  "Flush request data to X server."
  (let ((cache (slot-value obj 'request-cache)))
    (when (< 0 (length cache))
      (setf (slot-value obj 'request-cache) []) ;should be cleared ASAP
      (process-send-string (slot-value obj 'process)
                           (apply 'unibyte-string (append cache nil))))))

(cl-defmethod xcb:get-extension-data ((obj xcb:connection) namespace)
  "Fetch the extension data from X server (block until data is retrieved)."
  (let* ((plist (slot-value obj 'extension-plist))
         (data (plist-get plist namespace)))
    (if (object-p data)
        data
      (when (not data)                  ;the request has not been made
        (xcb:prefetch-extension-data obj namespace))
      (setq data
            (xcb:+reply obj
                (plist-get (slot-value obj 'extension-plist) namespace)))
      (when (cadr data)                 ;has error
        (error "[XELB] %s" (cadr data)))
      (setq data (car data))
      (setf (slot-value obj 'extension-plist) (plist-put plist namespace data))
      ;; Cache major opcode, first event and first error if possible
      (with-slots (present major-opcode first-event first-error) data
        (when (= 1 present)
          (setf (slot-value obj 'extension-opcode-plist)
                (plist-put (slot-value obj 'extension-opcode-plist)
                           namespace major-opcode)
                (slot-value obj 'extension-first-event-alist)
                (nconc (slot-value obj 'extension-first-event-alist)
                       `((,namespace . ,first-event)))
                (slot-value obj 'extension-first-error-alist)
                (nconc (slot-value obj 'extension-first-error-alist)
                       `((,namespace . ,first-error))))))
      data)))

(cl-defmethod xcb:prefetch-extension-data ((obj xcb:connection) namespace)
  "Prefetch the extension data from X server."
  (when (not (plist-get (slot-value obj 'extension-plist) namespace))
    (let* ((extension-xname
            (symbol-value (intern-soft (concat (symbol-name namespace)
                                               ":-extension-xname"))))
           (sequence
            (xcb:+request obj (make-instance 'xcb:QueryExtension
                                             :name-len (length extension-xname)
                                             :name extension-xname))))
      (setf (slot-value obj 'extension-plist)
            (plist-put (slot-value obj 'extension-plist) namespace sequence))
      (xcb:flush obj))))

(cl-defmethod xcb:generate-id ((obj xcb:connection))
  "Generate new X ID."
  (let* ((setup (xcb:get-setup obj))
         (base (slot-value setup 'resource-id-base))
         (mask (slot-value setup 'resource-id-mask))
         (increment (logand mask (- mask)))
         (xid (+ (slot-value obj 'xid) increment)))
    (when (> xid mask)
      (error "[XELB] Unable to allocate new X resource ID"))
    (setf (slot-value obj 'xid) xid)
    (logior base xid)))

;;;; Request related

(cl-defmethod xcb:-cache-request ((obj xcb:connection) request)
  "Send (or cache) a request and return the sequence number."
  (let* ((namespace
          (intern (replace-regexp-in-string
                   ":[^:]+$" "" (symbol-name
                                 (eieio-object-class request)))))
         (extension-opcode
          (plist-get (slot-value obj 'extension-opcode-plist) namespace))
         (msg (xcb:marshal request))
         (len (+ 2 (length msg))))
    (when extension-opcode
      (setq msg (vconcat (vector extension-opcode) msg))
      (setq len (1+ len)))
    (when (> 2 (length msg))   ;for short message (e.g. GetInputFocus)
      (setq msg (vconcat msg [0]))
      (setq len (1+ len)))
    (setq msg
          (vconcat (substring msg 0 2)
                   (funcall (if (slot-value request '~lsb) 'xcb:-pack-u2-lsb
                              'xcb:-pack-u2)
                            (ceiling len 4))
                   (substring msg 2)
                   (make-vector (% (- 4 (% len 4)) 4) 0))) ;required sometimes
    (when (< (xcb:get-maximum-request-length obj)
             (+ (length msg) (length (slot-value obj 'request-cache)))) ;flush on cache full
      (xcb:flush obj)
      (setq cache []))
    (xcb:-log "Cache request: %s" request)
    (with-slots (request-cache request-sequence) obj
      (setf request-cache (vconcat request-cache msg)
            request-sequence (1+ request-sequence))
      request-sequence)))

(cl-defmethod xcb:-+request ((obj xcb:connection) request)
  (let* ((sequence (xcb:-cache-request obj request))
         (sequence-lsw (logand #xFFFF sequence))
         (class (eieio-object-class request)))
    (when (fboundp (xcb:-request-class->reply-class class))
      ;; This request has a reply
      (setf (slot-value obj 'reply-plist) ;require reply
            (plist-put (slot-value obj 'reply-plist) sequence-lsw class))
      (setf (slot-value obj 'error-plist) ;require error
            (plist-put (slot-value obj 'error-plist) sequence-lsw nil)))
    sequence))

(defmacro xcb:+request (obj request)
  "Make a request.

If the request has a reply, then errors will also be available (if any).
Otherwise no error will ever be reported."
  (declare (indent 2))
  `(xcb:-+request ,obj ,request))

(cl-defmethod xcb:-+request-checked ((obj xcb:connection) request)
  (when (fboundp
         (xcb:-request-class->reply-class (eieio-object-class request)))
    (error "This method shall not be called with request that has a reply"))
  (let* ((sequence (xcb:-cache-request obj request))
         (sequence-lsw (logand #xFFFF sequence)))
    (setf (slot-value obj 'error-plist)
          (plist-put (slot-value obj 'error-plist) sequence-lsw nil))
    sequence))

(defmacro xcb:+request-checked (obj request)
  "Make a request (which have no reply) and check for errors."
  (declare (indent 2))
  `(xcb:-+request-checked ,obj ,request))

(cl-defmethod xcb:-+request-unchecked ((obj xcb:connection) request)
  (unless (fboundp
           (xcb:-request-class->reply-class (eieio-object-class request)))
    (error "This method shall not be called with request that has no reply"))
  (let* ((sequence (xcb:-cache-request obj request))
         (sequence-lsw (logand #xFFFF sequence)))
    (setf (slot-value obj 'reply-plist)
          (plist-put (slot-value obj 'reply-plist)
                     sequence-lsw (eieio-object-class request)))
    sequence))

(defmacro xcb:+request-unchecked (obj request)
  "Make a request (which have at least a reply) and discard any error."
  (declare (indent 2))
  `(xcb:-+request-unchecked ,obj ,request))

(cl-defmethod xcb:-+reply ((obj xcb:connection) sequence &optional multiple)
  (unless (plist-member (slot-value obj 'reply-plist) sequence)
    (error "This method is intended for requests with replies"))
  (xcb:flush obj)                      ;or we may have to wait forever
  (if multiple
      ;; Multiple replies
      (when (and (>= sequence (slot-value obj 'reply-sequence))
                 (>= sequence (slot-value obj 'error-sequence)))
        (xcb:aux:sync obj))
    ;; Single reply
    (let ((process (slot-value obj 'process)))
      ;; Wait until the request processed
      (with-timeout (xcb:connection-timeout (error "[XELB] Retrieve reply timeout"))
        (while (and (> sequence (slot-value obj 'reply-sequence))
                    (> sequence (slot-value obj 'error-sequence)))
          (accept-process-output process 1)))))
  (let* ((reply-plist (slot-value obj 'reply-plist))
         (reply-data (plist-get reply-plist sequence))
         (error-plist (slot-value obj 'error-plist))
         (error-data (plist-get error-plist sequence))
         class-name reply replies error errors)
    (if (symbolp reply-data)
        (setq replies nil)              ;no reply
      (setq class-name (xcb:-request-class->reply-class (car reply-data)))
      (if multiple
          ;; Multiple replies
          (dolist (i (cdr reply-data))
            (setq reply (make-instance class-name
                                       :length (/ (- (length i) 32) 4)))
            (xcb:unmarshal reply i)
            (setq replies (nconc replies (list reply))))
        ;; Single reply
        (setq reply-data (cadr reply-data)
              replies (make-instance class-name
                                     :length (/ (- (length reply-data) 32) 4)))
        (xcb:unmarshal replies reply-data)))
    (setq errors
          (mapcar (lambda (i)
                    (setq error (make-instance
                                 (xcb:-error-number->class obj (car i))))
                    (xcb:unmarshal error (cdr i))
                    i)
                  error-data))
    (cl-remf (slot-value obj 'reply-plist) sequence)
    (cl-remf (slot-value obj 'error-plist) sequence)
    (list replies errors)))

(defmacro xcb:+reply (obj sequence &optional multiple)
  "Return the reply of a request of which the sequence number is SEQUENCE.

If MULTIPLE is nil, the return value is the only reply, or it returns a list of
all replies.

WARNING: for requests that have multiple replies, you MUST supply a non-nil
MULTIPLE value, or some replies may be lost!"
  (declare (indent 2))
  `(xcb:-+reply ,obj ,sequence ,multiple))

(cl-defmethod xcb:-request-check ((obj xcb:connection) sequence)
  (when (plist-member (slot-value obj 'reply-plist) sequence)
    (error "This method is intended for requests with no reply"))
  (xcb:flush obj)                      ;or we may have to wait forever
  (let ((error-plist (slot-value obj 'error-plist))
        error-obj tmp)
    (unless (plist-member error-plist sequence)
      (error "This method shall be called after `xcb:+request-checked'"))
    (when (> sequence (slot-value obj 'error-sequence))
      (xcb:aux:sync obj))         ;wait until the request is processed
    (setq error-obj
          (mapcar (lambda (i)
                    (setq tmp (cdr i)
                          i (make-instance
                             (xcb:-error-number->class obj (car i))))
                    (xcb:unmarshal i tmp)
                    i)
                  (plist-get error-plist sequence)))
    (cl-remf (slot-value obj 'error-plist) sequence)
    error-obj))

(defmacro xcb:request-check (obj sequence)
  "Return the error of the request of which the sequence number is SEQUENCE.

The sequence number shall be returned by `xcb:+request-checked'."
  (declare (indent 2))
  `(xcb:-request-check ,obj ,sequence))

(defmacro xcb:+request+reply (obj request &optional multiple)
  "Make a request and return its replies and errors.

If MULTIPLE is nil, the return value is a list of which the car is the only
reply and the cadr a list of errors. Otherwise, the car of the result is a
list of replies.

WARNING: for requests that have multiple replies, you MUST supply a non-nil
MULTIPLE value, or some replies may be lost!"
  (declare (indent 2))
  `(xcb:-+reply ,obj (xcb:-+request ,obj ,request) ,multiple))

(defmacro xcb:+request-checked+request-check (obj request)
  "Make a request (which has no reply) and return the errors."
  (declare (indent 2))
  `(xcb:-request-check ,obj (xcb:-+request-checked ,obj ,request)))

(defmacro xcb:+request-unchecked+reply (obj request &optional multiple)
  "Make a request (that has at least one reply) and only return the reply.

If MULTIPLE is nil, the return value is the only reply, or it returns a list of
all replies.

WARNING: for requests that have multiple replies, you MUST supply a non-nil
MULTIPLE value, or some replies may be lost!"
  (declare (indent 2))
  `(car (xcb:-+reply ,obj (xcb:-+request-unchecked ,obj ,request) ,multiple)))

;;;; Misc.

(cl-defmethod xcb:aux:sync ((obj xcb:connection))
  "Force sync with X server.

Sync by sending a GetInputFocus request and waiting until it's processed."
  (let ((sequence (xcb:-cache-request obj (make-instance 'xcb:GetInputFocus)))
        (process (slot-value obj 'process)))
    (xcb:flush obj)
    ;; Wait until request processed
    (with-timeout (xcb:connection-timeout (error "[XELB] Sync timeout"))
      (while (> sequence (slot-value obj 'reply-sequence))
        (accept-process-output process 1)))))

(cl-defmethod xcb:-error-or-event-class->number ((obj xcb:connection) class)
  "Return the error/event number of a error/event class CLASS."
  (unless (symbolp class) (setq class (eieio-class-name class)))
  (let* ((is-error (child-of-class-p class 'xcb:-error))
         (prefix (replace-regexp-in-string ":[^:]+$" ":" (symbol-name class)))
         (first (when (string= prefix "xcb:") 0))
         (alist (intern-soft (concat prefix (if is-error
                                                "error-number-class-alist"
                                              "event-number-class-alist"))))
         result parents)
    (unless first
      (setq first (cdr (assoc (intern (substring prefix 0 -1))
                              (slot-value obj
                                          (if is-error
                                              'extension-first-error-alist
                                            'extension-first-event-alist))))))
    (when alist
      (setq result (+ (or first 0) (car (rassoc class (symbol-value alist))))))
    (if result
        result
      ;; Fallback to use the error/event number of one superclass. Thus if the
      ;; error/event number of a subclass differs from that of its parent, it
      ;; must be explicitly pointed out.
      (setq parents (eieio-class-parents class))
      (while (and parents (not result))
        (setq result (xcb:-error-or-event-class->number obj (pop parents))))
      result)))

(cl-defmethod xcb:-event-number->class ((obj xcb:connection) number)
  "Return the event class that has the event number NUMBER.

Note that when multiple events have the same number, only the top-most
superclass will be returned."
  (if (or (< number 64) (> number 127))
      ;; Xproto event
      (cdr (assoc number xcb:event-number-class-alist))
    ;; Extension event
    (let ((first-event number)
          namespace index)
      (while (and (not namespace) (>= first-event 64))
        (setq namespace
              (car (rassoc first-event
                           (slot-value obj 'extension-first-event-alist)))
              first-event (1- first-event)))
      (setq index (- number first-event 1))
      (setq alist (intern-soft (concat (symbol-name namespace)
                                       ":event-number-class-alist")))
      (cdr (assoc index (symbol-value alist))))))

(cl-defmethod xcb:-error-number->class ((obj xcb:connection) number)
  "Return the error class that has the error number NUMBER.

Note that when multiple errors have the same number, only the top-most
superclass will be returned."
  (if (or (< number 128) (> number 255))
      ;; Xproto error
      (cdr (assoc number xcb:error-number-class-alist))
    ;; Extension error
    (let ((first-error number)
          namespace index)
      (while (and (not namespace) (>= first-error 128))
        (setq namespace
              (car (rassoc first-error
                           (slot-value obj 'extension-first-error-alist)))
              first-error (1- first-error)))
      (setq index (- number first-error 1))
      (setq alist (intern-soft (concat (symbol-name namespace)
                                       ":error-number-class-alist")))
      (cdr (assoc index (symbol-value alist))))))



(provide 'xcb)

;;; xcb.el ends here
