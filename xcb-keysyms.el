;;; xcb-keysyms.el --- Conversion between  -*- lexical-binding: t -*-
;;;                    X keysyms, X keycodes and Emacs key event.

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

;; This library mainly deals with the conversion between X keycodes, X keysyms
;; and Emacs key events, roughly corresponding to the xcb/util-keysyms project.

;; Usage tips:
;; + Do not forget to call `xcb:keysyms:init' for _every_ connection using
;;   this library.
;; + xcb:keysyms:*-mask correctly relate Emacs modifier keys to X ones,
;;   thus shall be used in preference to 'xcb:ModMask:*' or
;;   'xcb:KeyButMask:Mod*'.

;; Todo:
;; + Is xcb:ModMask:Control/xcb:ModMask:Shift always equivalent to
;;   control/shift in Emacs?

;; References:
;; + X protocol (http://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.txt)
;; + xcb/util-keysyms (git://anongit.freedesktop.org/xcb/util-keysyms)

;;; Code:

(require 'xcb)

(defvar xcb:keysyms:meta-mask nil "META key mask.")
(defvar xcb:keysyms:control-mask xcb:ModMask:Control "CONTROL key mask.")
(defvar xcb:keysyms:shift-mask xcb:ModMask:Shift "SHIFT key mask.")
(defvar xcb:keysyms:hyper-mask nil "HYPER key mask.")
(defvar xcb:keysyms:super-mask nil "SUPER key mask.")
(defvar xcb:keysyms:alt-mask nil "ALT key mask.")
(defvar xcb:keysyms:lock-mask xcb:ModMask:Lock "LOCK key mask.")
(defvar xcb:keysyms:num-lock-mask nil "NUM LOCK key mask.")
(defvar xcb:keysyms:mode-switch-mask nil "MODE SWITCH key mask.")

(cl-defmethod xcb:keysyms:init ((obj xcb:connection))
  "Initialize keysyms module.

This method must be called before using any other method in this module."
  (with-slots (min-keycode max-keycode) (xcb:get-setup obj)
    (xcb:keysyms:update-keyboard-mapping obj
                                         min-keycode
                                         (1+ (- max-keycode min-keycode)))
    (unless xcb:keysyms:meta-mask     ;avoid duplicated initialization
      (xcb:keysyms:update-modifier-mapping obj))))

(cl-defmethod xcb:keysyms:update-keyboard-mapping ((obj xcb:connection)
                                                   first-keycode count)
  "Update keyboard mapping (from FIRST-KEYCODE to FIRST-KEYCODE + COUNT - 1)."
  (let* ((reply (xcb:+request-unchecked+reply obj
                    (make-instance 'xcb:GetKeyboardMapping
                                   :first-keycode first-keycode :count count)))
         (keysyms-per-keycode (slot-value reply 'keysyms-per-keycode))
         (keysyms (slot-value reply 'keysyms))
         (result (plist-get (slot-value obj 'extra-plist) 'keysyms))
         keycode index row-index keysym)
    (dotimes (i count)
      (setq keycode (+ i first-keycode)
            index (* i keysyms-per-keycode)
            row-index 0)
      (setq keysym (nth (+ index row-index) keysyms))
      (setq result (assq-delete-all keycode result))
      (while (and (/= keysym 0) (< row-index keysyms-per-keycode))
        (setq result (append result `((,keycode . ,keysym)))
              row-index (1+ row-index)
              keysym (nth (+ index row-index) keysyms))))
    (setf (slot-value obj 'extra-plist)
          (plist-put (slot-value obj 'extra-plist) 'keysyms result))))

(cl-defmethod xcb:keysyms:update-modifier-mapping ((obj xcb:connection))
  "Differentiate xcb:ModMask:1 ~ xcb:ModMask:5."
  ;; Determine MODE SWITCH and NUM LOCK
  (let* ((reply (xcb:+request-unchecked+reply obj
                    (make-instance 'xcb:GetModifierMapping)))
         (keycodes-per-modifier (slot-value reply 'keycodes-per-modifier))
         (keycodes (slot-value reply 'keycodes))
         (mode-masks (list xcb:ModMask:Shift xcb:ModMask:Lock
                           xcb:ModMask:Control xcb:ModMask:1 xcb:ModMask:2
                           xcb:ModMask:3 xcb:ModMask:4 xcb:ModMask:5))
         events keycode keysym)
    (setq xcb:keysyms:mode-switch-mask nil
          xcb:keysyms:num-lock-mask nil)
    (cl-assert (= (length keycodes) (* 8 keycodes-per-modifier)))
    (dotimes (i 8)
      (setq events nil)
      (dotimes (_ keycodes-per-modifier)
        (when (and (/= (setq keycode (pop keycodes)) 0)
                   (setq keysym (xcb:keysyms:keycode->keysym obj keycode 0)))
          (setq events
                (nconc events
                       (list (xcb:keysyms:keysym->event keysym nil t))))))
      (cond ((memq 'mode-switch* events)
             (setq xcb:keysyms:mode-switch-mask (elt mode-masks i)))
            ((memq 'kp-numlock events)
             (setq xcb:keysyms:num-lock-mask (elt mode-masks i))))))
  ;; Determine remaining keys
  (let* ((frame (unless (frame-parameter nil 'window-id)
                  (catch 'break
                    (dolist (i (frame-list))
                      (when (frame-parameter i 'window-id)
                        (throw 'break i))))))
         (id (string-to-number (frame-parameter frame 'window-id)))
         (root
          (slot-value (car (slot-value (xcb:get-setup obj) 'roots)) 'root))
         (keycode (xcb:keysyms:keysym->keycode obj ?a))
         (fake-event (make-instance 'xcb:SendEvent
                                    :propagate 0 :destination id
                                    :event-mask xcb:EventMask:NoEvent
                                    :event nil))
         (key-press (make-instance 'xcb:KeyPress
                                   :detail keycode :time xcb:Time:CurrentTime
                                   :root root :event id :child 0
                                   :root-x 0 :root-y 0 :event-x 0 :event-y 0
                                   :state nil :same-screen 1))
         event)
    (dolist (i (list xcb:ModMask:1 xcb:ModMask:2 xcb:ModMask:3
                     xcb:ModMask:4 xcb:ModMask:5))
      (unless (or (equal i xcb:keysyms:mode-switch-mask) ;already determined
                  (equal i xcb:keysyms:num-lock-mask))
        (setf (slot-value key-press 'state) i
              (slot-value fake-event 'event) (xcb:marshal key-press obj))
        (run-with-idle-timer 0 nil (lambda ()
                                     (xcb:+request obj fake-event)
                                     (xcb:flush obj)))
        (catch 'break
          (with-timeout (1)             ;FIXME
            (while t
              (setq event (read-event))
              (when (and (integerp event) (= ?a (event-basic-type event)))
                (pcase event
                  (?\M-a (setq xcb:keysyms:meta-mask i))
                  (?\A-a (setq xcb:keysyms:alt-mask i))
                  (?\s-a (setq xcb:keysyms:super-mask i))
                  (?\H-a (setq xcb:keysyms:hyper-mask i)))
                (throw 'break nil)))))))))

(cl-defmethod xcb:keysyms:keycode->keysym ((obj xcb:connection)
                                           keycode modifiers)
  "Get the keysym from KeyPress event

SHIFT LOCK is ignored."
  (let* ((keysyms (plist-get (slot-value obj 'extra-plist) 'keysyms))
         (group (delq nil (mapcar (lambda (i)
                                    (when (= keycode (car i)) (cdr i)))
                                  keysyms)))
         (group (pcase (length group)
                  (1 (append group '(0) group '(0)))
                  (2 (append group group))
                  (3 (append group '(0)))
                  (_
                   (list (elt group 0) (elt group 1)
                         (elt group 2) (elt group 3)))))
         (group (if (and xcb:keysyms:mode-switch-mask ;not initialized
                         (/= 0
                             (logand modifiers xcb:keysyms:mode-switch-mask)))
                    (cddr group) (list (elt group 0) (elt group 1))))
         (mask (logior (if (= 0 (logand modifiers xcb:keysyms:shift-mask)) 0 1)
                       (if (= 0 (logand modifiers xcb:keysyms:lock-mask))
                           0 2))))
    (if (and xcb:keysyms:num-lock-mask  ;not initialized
             (/= 0 (logand modifiers xcb:keysyms:num-lock-mask))
             (<= #xff80 (elt group 1)) (>= #xffbe (elt group 1))) ;keypad
        (if (= mask 1) (elt group 0) (elt group 1))
      (pcase mask
        (0 (elt group 0))               ;SHIFT off, CAPS LOCK off
        (1 (elt group 1))               ;SHIFT on, CAPS LOCK off
        (2 (upcase (elt group 0)))      ;SHIFT off, CAPS LOCK on
        (3 (upcase (elt group 1)))))))  ;SHIFT on, CAPS LOCK on

(cl-defmethod xcb:keysyms:keysym->keycode ((obj xcb:connection) keysym)
  "Convert X keysym to (first match) keycode"
  (car (rassoc keysym (plist-get (slot-value obj 'extra-plist) 'keysyms))))

;; This list is largely base on 'lispy_function_keys' in 'keyboard.c'.
;; Emacs has a built-in variable `x-keysym-table' providing Latin-1 and legacy
;; keysyms, which seems not very useful here.
;; FIXME: shall we also include 'iso_lispy_function_keys' there?
(defconst xcb:keysyms:-function-keys
  `(                                    ;#xff00 - #xff0f
    ,@(make-list 8 nil) backspace tab linefeed clear nil return nil nil
                                        ;#xff10 - #xff1f
    nil nil nil pause nil nil nil nil nil nil nil escape nil nil nil nil
                                        ;#xff20 - #xff2f
    nil kanji muhenkan henkan romaji hiragana katakana hiragana-katakana
    zenkaku hankaku zenkaku-hankaku touroku massyo kana-lock kana-shift
    eisu-shift
                                        ;#xff30 - #xff3f
    eisu-toggle ,@(make-list 15 nil)
                                        ;#xff40 - #xff4f
    ,@(make-list 16 nil)
                                        ;#xff50 - #xff5f
    home left up right down prior next end begin ,@(make-list 7 nil)
                                        ;#xff60 - #xff6f
    select print execute insert nil undo redo menu find cancel help break
    nil nil nil nil
                                        ;#xff70 - #xff7f
    ;; nil nil nil nil backtab ,@(make-list 10 nil) kp-numlock
    nil nil nil nil backtab ,@(make-list 9 nil) mode-switch* kp-numlock
                                        ;#xff80 - #xff8f
    kp-space ,@(make-list 8 nil) kp-tab nil nil nil kp-enter nil nil
                                        ;#xff90 - #xff9f
    nil kp-f1 kp-f2 kp-f3 kp-f4 kp-home kp-left kp-up kp-right kp-down
    kp-prior kp-next kp-end kp-begin kp-insert kp-delete
                                        ;#xffa0 - #xffaf
    ,@(make-list 10 nil)
    kp-multiply kp-add kp-separator kp-subtract kp-decimal kp-divide
                                        ;#xffb0 - #xffbf
    kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9 nil nil nil kp-equal
    f1 f2
                                        ;#xffc0 - #xffcf
    f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18
                                        ;#xffd0 - #xffdf
    f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34
                                        ;#xffe0 - #xffef
    ;; f35 ,@(make-list 15 nil)
    f35 lshift* rshift* lcontrol* rcontrol* caps-lock* shift-lock*
    lmeta* rmeta* lalt* ralt* lsuper* rsuper* lhyper* rhyper*
                                        ;#xff00 - #xffff
    ,@(make-list 15 nil) delete)
  "Emacs event representations of X function keys (keysym #xff00 to #xffff).")

(defun xcb:keysyms:event->keysym (event)
  "Translate Emacs key event EVENT to X Keysym.

This function returns nil when it fails to convert an event."
  (let ((modifiers (event-modifiers event))
        (event (event-basic-type event))
        keysym)
    (if (not (integerp event))
        (setq keysym
              (pcase event
                (`mouse-1 xcb:ButtonIndex:1)
                (`mouse-2 xcb:ButtonIndex:2)
                (`mouse-3 xcb:ButtonIndex:3)
                (`mouse-4 xcb:ButtonIndex:4)
                (`mouse-5 xcb:ButtonIndex:5)
                ;; Function keys
                (_ (cl-position event xcb:keysyms:-function-keys))))
      (if (and (<= #x20 event) (>= #xff event)) ;Latin-1
          (setq keysym event)
        (when (and (<= #x100 event) (>= #x10ffff event)) ;Unicode
          (setq keysym (+ #x1000000 event)))))
    (when keysym
      (when (and (not (integerp event)) (< 5 keysym))
        (setq keysym (logior keysym #xff00)))
      `(,keysym
        ;; state for KeyPress event
        ,(apply #'logior
                (mapcar (lambda (i)
                          (pcase i
                            (`meta xcb:keysyms:meta-mask)
                            (`control xcb:keysyms:control-mask)
                            (`shift xcb:keysyms:shift-mask)
                            (`hyper xcb:keysyms:hyper-mask)
                            (`super xcb:keysyms:super-mask)
                            (`alt xcb:keysyms:alt-mask)
                            (`down 0)
                            ;; FIXME: more?
                            (_ 0)))
                        modifiers))))))

(defun xcb:keysyms:keysym->event (keysym &optional mask allow-modifiers)
  "Translate X Keysym KEYSYM into Emacs key event.

One may use MASK to provide modifier keys.  If ALLOW-MODIFIERS is non-nil,
this function will also return symbols for pure modifiers keys."
  (let ((event (cond ((and (<= #x20 keysym) (>= #xff keysym))
                      keysym)
                     ((and (<= #xff00 keysym) (>= #xffff keysym))
                      (elt xcb:keysyms:-function-keys (logand keysym #xff)))
                     ((and (<= #x1000100 keysym) (>= #x110ffff keysym))
                      (- keysym #x1000000))
                     ((and (<= 1 keysym) (>= 5 keysym)) ;ButtonPress assuemd
                      (intern-soft (format "down-mouse-%d" keysym))))))
    (when (and (not allow-modifiers)
               (memq event
                     '(lshift* rshift* lcontrol* rcontrol*
                               caps-lock* shift-lock* lmeta* rmeta* lalt* ralt*
                               lsuper* rsuper* lhyper* rhyper*
                               mode-switch* kp-numlock)))
      (setq event nil))
    (when event
      (if (not mask)
          event
        (setq event (list event))
        (when (/= 0 (logand mask xcb:keysyms:meta-mask))
          (push 'meta event))
        (when (/= 0 (logand mask xcb:keysyms:control-mask))
          (push 'control event))
        (when (and (/= 0 (logand mask xcb:keysyms:shift-mask))
                   ;; Emacs only set shift bit for letters
                   (integerp (car (last event)))
                   (<= ?A (car (last event))) (>= ?Z (car (last event))))
          (push 'shift event))
        (when (and xcb:keysyms:hyper-mask
                   (/= 0 (logand mask xcb:keysyms:hyper-mask)))
          (push 'hyper event))
        (when (/= 0 (logand mask xcb:keysyms:super-mask))
          (push 'super event))
        (when (and xcb:keysyms:alt-mask
                   (/= 0 (logand mask xcb:keysyms:alt-mask)))
          (push 'alt event))
        (event-convert-list event)))))

(provide 'xcb-keysyms)

;;; xcb-keysyms.el ends here
