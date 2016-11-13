;;; xcb-keysyms.el --- Conversion between  -*- lexical-binding: t -*-
;;;                    X keysyms, X keycodes and Emacs key event.

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

;; References:
;; + X protocol (http://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.txt)
;; + xcb/util-keysyms (git://anongit.freedesktop.org/xcb/util-keysyms)

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'xcb)
(require 'xcb-xkb)

;; These variables are shared by all connections.
(defvar xcb:keysyms:meta-mask 0 "META key mask.")
(defvar xcb:keysyms:control-mask xcb:ModMask:Control "CONTROL key mask.")
(defvar xcb:keysyms:shift-mask xcb:ModMask:Shift "SHIFT key mask.")
(defvar xcb:keysyms:hyper-mask 0 "HYPER key mask.")
(defvar xcb:keysyms:super-mask 0 "SUPER key mask.")
(defvar xcb:keysyms:alt-mask 0 "ALT key mask.")
(defvar xcb:keysyms:lock-mask xcb:ModMask:Lock "LOCK key mask.")
(defvar xcb:keysyms:shift-lock-mask 0 "SHIFT-LOCK key mask.")
(defvar xcb:keysyms:num-lock-mask 0 "NUM-LOCK key mask.")
;; Internal state / local data.
(defvar xcb:keysyms:-opcode nil)
(defvar xcb:keysyms:-device nil)
(defvar xcb:keysyms:-keytypes nil)
(defvar xcb:keysyms:-keycodes nil)
(defvar xcb:keysyms:-min-keycode nil)
(defvar xcb:keysyms:-max-keycode nil)

(cl-defmethod xcb:keysyms:init ((obj xcb:connection))
  "Initialize keysyms module.

This method must be called before using any other method in this module."
  (cond
   ;; Avoid duplicated initializations.
   (xcb:keysyms:-opcode)
   ((= 0 (slot-value (xcb:get-extension-data obj 'xcb:xkb)
                     'present))
    (error "[XCB] XKB extension is not supported by the server"))
   ((not (slot-value (xcb:+request-unchecked+reply obj
                         (make-instance 'xcb:xkb:UseExtension
                                        :wantedMajor 1
                                        :wantedMinor 0))
                     'supported))
    (error "[XCB] XKB extension version 1.0 is not supported by the server"))
   (t
    ;; Save the major opcode of XKB.
    (setq xcb:keysyms:-opcode
          (slot-value (xcb:get-extension-data obj 'xcb:xkb) 'major-opcode))
    ;; Set per-client flags.
    (xcb:keysyms:-set-per-client-flags obj xcb:xkb:ID:UseCoreKbd)
    ;; Update data.
    (xcb:keysyms:-update-keytypes obj xcb:xkb:ID:UseCoreKbd)
    (xcb:keysyms:-update-keycodes obj xcb:xkb:ID:UseCoreKbd)
    (xcb:keysyms:-update-modkeys obj xcb:xkb:ID:UseCoreKbd)
    ;; Attach event listeners.
    (xcb:+event obj 'xcb:xkb:NewKeyboardNotify
                `(lambda (data _)
                   (xcb:keysyms:-on-NewKeyboardNotify ,obj data)))
    (xcb:+event obj 'xcb:xkb:MapNotify
                `(lambda (data _)
                   (xcb:keysyms:-on-MapNotify ,obj data)))
    ;; Select XKB MapNotify and NewKeyboardNotify events.
    (let ((map (logior xcb:xkb:MapPart:KeyTypes
                       xcb:xkb:MapPart:KeySyms
                       xcb:xkb:MapPart:ModifierMap))
          (new-keyboard (logior xcb:xkb:NKNDetail:DeviceID
                                xcb:xkb:NKNDetail:Keycodes)))
      (xcb:+request obj
          (make-instance 'xcb:xkb:SelectEvents
                         :deviceSpec xcb:xkb:ID:UseCoreKbd
                         :affectWhich (logior
                                       xcb:xkb:EventType:NewKeyboardNotify
                                       xcb:xkb:EventType:MapNotify)
                         :clear 0
                         :selectAll 0
                         :affectMap map
                         :map map
                         :affectNewKeyboard new-keyboard
                         :newKeyboardDetails new-keyboard)))
    (xcb:flush obj))))

(cl-defmethod xcb:keysyms:-set-per-client-flags ((obj xcb:connection) device)
  "Set per-client flags."
  (let ((per-client-flags (logior
                           ;; Instead of compatibility state.
                           xcb:xkb:PerClientFlag:GrabsUseXKBState
                           ;; Instead of grab state.
                           xcb:xkb:PerClientFlag:LookupStateWhenGrabbed
                           ;; Use XKB state in 'SendEvent'.
                           xcb:xkb:PerClientFlag:SendEventUsesXKBState)))
    ;; The reply is not used.
    (xcb:+request-unchecked+reply obj
        (make-instance 'xcb:xkb:PerClientFlags
                       :deviceSpec device
                       :change per-client-flags
                       :value per-client-flags
                       :ctrlsToChange 0
                       :autoCtrls 0
                       :autoCtrlsValues 0))))

(cl-defmethod xcb:keysyms:-on-NewKeyboardNotify ((obj xcb:connection) data)
  "Handle 'NewKeyboardNotify' event."
  (let ((obj1 (make-instance 'xcb:xkb:NewKeyboardNotify)))
    (xcb:unmarshal obj1 data)
    (with-slots (deviceID requestMajor requestMinor changed) obj1
      (if (= 0 (logand changed xcb:xkb:NKNDetail:DeviceID))
          ;; Device is not changed; ensure it's a keycode change from
          ;; this device.
          (when (and (/= 0 (logand changed xcb:xkb:NKNDetail:Keycodes))
                     (= deviceID xcb:keysyms:-device)
                     ;; Also, according to the specification this can
                     ;; only happen when a GetKbdByName request issued.
                     ;; The two checks below avoid false positive caused
                     ;; by requests such as SetMap (e.g. XCape).
                     (= requestMajor xcb:keysyms:-opcode)
                     (= requestMinor
                        (eieio-oref-default 'xcb:xkb:GetKbdByName '~opcode)))
            ;; (xcb:keysyms:-update-keytypes obj deviceID)
            (xcb:keysyms:-update-keycodes obj deviceID)
            (xcb:keysyms:-update-modkeys obj deviceID))
        ;; Device changed; update the per-client flags and local data.
        (xcb:keysyms:-set-per-client-flags obj deviceID)
        (xcb:keysyms:-update-keytypes obj deviceID)
        (xcb:keysyms:-update-keycodes obj deviceID)
        (xcb:keysyms:-update-modkeys obj deviceID)))))

(cl-defmethod xcb:keysyms:-on-MapNotify ((obj xcb:connection) data)
  "Handle 'MapNotify' event."
  (let ((obj1 (make-instance 'xcb:xkb:MapNotify)))
    (xcb:unmarshal obj1 data)
    (with-slots (deviceID changed firstType nTypes firstKeySym nKeySyms) obj1
      ;; Ensure this event is for the current device.
      (when (= deviceID xcb:keysyms:-device)
        (when (/= 0 (logand changed xcb:xkb:MapPart:KeyTypes))
          (xcb:keysyms:-update-keytypes obj deviceID firstType nTypes))
        (when (/= 0 (logand changed xcb:xkb:MapPart:KeySyms))
          (xcb:keysyms:-update-keycodes obj deviceID firstKeySym nKeySyms))
        (when (/= 0 (logand changed xcb:xkb:MapPart:ModifierMap))
          (xcb:keysyms:-update-modkeys obj deviceID))))))

(cl-defmethod xcb:keysyms:-update-keytypes ((obj xcb:connection) device
                                            &optional first-keytype count)
  "Update key types.

FIRST-KEYTYPE and count specify the range of key types to update."
  (let (full partial)
    (if (and first-keytype count)
        (setq full 0
              partial xcb:xkb:MapPart:KeyTypes)
      (setq full xcb:xkb:MapPart:KeyTypes
            partial 0
            first-keytype 0
            count 0))
    (with-slots (deviceID present firstType nTypes totalTypes types-rtrn)
        (xcb:+request-unchecked+reply obj
            (make-instance 'xcb:xkb:GetMap
                           :deviceSpec device
                           :full full
                           :partial partial
                           :firstType first-keytype
                           :nTypes count
                           :firstKeySym 0
                           :nKeySyms 0
                           :firstKeyAction 0
                           :nKeyActions 0
                           :firstKeyBehavior 0
                           :nKeyBehaviors 0
                           :virtualMods 0
                           :firstKeyExplicit 0
                           :nKeyExplicit 0
                           :firstModMapKey 0
                           :nModMapKeys 0
                           :firstVModMapKey 0
                           :nVModMapKeys 0))
      (cl-assert (/= 0 (logand present xcb:xkb:MapPart:KeyTypes)))
      (when (/= 0 full)
        (setq xcb:keysyms:-device deviceID
              xcb:keysyms:-keytypes (make-vector totalTypes nil)))
      (setq xcb:keysyms:-keytypes
            (vconcat (substring xcb:keysyms:-keytypes 0 firstType)
                     types-rtrn
                     (substring xcb:keysyms:-keytypes (min (+ firstType nTypes)
                                                           totalTypes)))))))

(cl-defmethod xcb:keysyms:-update-keycodes ((obj xcb:connection) device
                                            &optional first-keycode count)
  "Update keycode-keysym mapping.

FIRST-KEYCODE and COUNT specify the keycode range to update."
  (let (full partial)
    (if (and first-keycode count)
        (setq full 0
              partial xcb:xkb:MapPart:KeySyms)
      (setq full xcb:xkb:MapPart:KeySyms
            partial 0
            first-keycode 0
            count 0))
    (with-slots (deviceID minKeyCode maxKeyCode present
                          firstKeySym nKeySyms syms-rtrn)
        (xcb:+request-unchecked+reply obj
            (make-instance 'xcb:xkb:GetMap
                           :deviceSpec device
                           :full full
                           :partial partial
                           :firstType 0
                           :nTypes 0
                           :firstKeySym first-keycode
                           :nKeySyms count
                           :firstKeyAction 0
                           :nKeyActions 0
                           :firstKeyBehavior 0
                           :nKeyBehaviors 0
                           :virtualMods 0
                           :firstKeyExplicit 0
                           :nKeyExplicit 0
                           :firstModMapKey 0
                           :nModMapKeys 0
                           :firstVModMapKey 0
                           :nVModMapKeys 0))
      (cl-assert (/= 0 (logand present xcb:xkb:MapPart:KeySyms)))
      (when (or (/= 0 full)
                ;; Unlikely?
                (/= xcb:keysyms:-min-keycode minKeyCode)
                (/= xcb:keysyms:-max-keycode maxKeyCode))
        (setq xcb:keysyms:-min-keycode minKeyCode
              xcb:keysyms:-max-keycode maxKeyCode
              xcb:keysyms:-keycodes (make-vector (- xcb:keysyms:-max-keycode
                                                    xcb:keysyms:-min-keycode
                                                    -1)
                                                 nil)))
      (setq xcb:keysyms:-keycodes
            (vconcat
             (substring xcb:keysyms:-keycodes 0 (- firstKeySym
                                                   xcb:keysyms:-min-keycode))
             syms-rtrn
             (substring xcb:keysyms:-keycodes
                        (- (min (+ firstKeySym nKeySyms)
                                xcb:keysyms:-max-keycode)
                           xcb:keysyms:-min-keycode)))))))

(cl-defmethod xcb:keysyms:-update-modkeys ((obj xcb:connection) _device)
  "Update modifier keys."
  ;; Reference: 'x_find_modifier_meanings' in 'xterm.c'.
  (with-slots (keycodes-per-modifier keycodes)
      (xcb:+request-unchecked+reply obj
          (make-instance 'xcb:GetModifierMapping))
    (setq xcb:keysyms:meta-mask 0
          xcb:keysyms:hyper-mask 0
          xcb:keysyms:super-mask 0
          xcb:keysyms:alt-mask 0
          xcb:keysyms:shift-lock-mask 0
          xcb:keysyms:num-lock-mask 0)
    (dolist (row (number-sequence 3 7))
      (let ((mask (lsh 1 row))
            (col 0)
            found-alt-or-meta keycode keysym)
        (while (< col keycodes-per-modifier)
          (setq keycode (elt keycodes (+ (* row keycodes-per-modifier) col)))
          (when (/= keycode 0)
            (setq keysym (car (xcb:keysyms:keycode->keysym obj keycode 0)))
            (when (/= keysym 0)
              (pcase (xcb:keysyms:keysym->event obj keysym nil t)
                ((or `lmeta* `rmeta*)
                 (setq found-alt-or-meta t
                       xcb:keysyms:meta-mask (logior xcb:keysyms:meta-mask
                                                     mask)))
                ((or `lalt* `ralt*)
                 (setq found-alt-or-meta t
                       xcb:keysyms:alt-mask (logior xcb:keysyms:alt-mask
                                                    mask)))
                ((or `lhyper* `rhyper*)
                 (unless found-alt-or-meta
                   (setq xcb:keysyms:hyper-mask (logior xcb:keysyms:hyper-mask
                                                        mask)))
                 (setq col keycodes-per-modifier))
                ((or `lsuper* `rsuper*)
                 (unless found-alt-or-meta
                   (setq xcb:keysyms:super-mask (logior xcb:keysyms:super-mask
                                                        mask)))
                 (setq col keycodes-per-modifier))
                (`shift-lock*
                 (unless found-alt-or-meta
                   (setq xcb:keysyms:lock-mask (logior xcb:keysyms:lock-mask
                                                       mask)))
                 (setq col keycodes-per-modifier))
                (`kp-numlock
                 (setq xcb:keysyms:num-lock-mask
                       (logior xcb:keysyms:num-lock-mask mask))))))
          (cl-incf col)))))
  ;; Meta fallbacks to Alt.
  (unless (/= 0 xcb:keysyms:meta-mask)
    (setq xcb:keysyms:meta-mask xcb:keysyms:alt-mask
          xcb:keysyms:alt-mask 0))
  ;; A key cannot be both Meta and Alt.
  (when (and (/= 0 xcb:keysyms:meta-mask)
             (/= 0 xcb:keysyms:alt-mask)
             (/= 0 (logand xcb:keysyms:meta-mask xcb:keysyms:alt-mask)))
    (setq xcb:keysyms:alt-mask (logand xcb:keysyms:alt-mask
                                       (lognot xcb:keysyms:meta-mask)))))

(cl-defmethod xcb:keysyms:keycode->keysym ((_obj xcb:connection) keycode
                                           modifiers)
  "Convert keycode to (keysym . mod-mask).

Return (0 . 0) when conversion fails."
  (let ((preserve 0)
        group group-info group-number index keytype)
    ;; Reference: `XkbTranslateKeyCode' in 'XKBBind.c'.
    (catch 'return
      ;; Check keycode range.
      (unless (<= xcb:keysyms:-min-keycode keycode xcb:keysyms:-max-keycode)
        (throw 'return '(0 . 0)))
      ;; Retrieve KeySymMap and group info.
      (setq keycode (aref xcb:keysyms:-keycodes
                          (- keycode xcb:keysyms:-min-keycode))
            group-info (slot-value keycode 'groupInfo)
            group-number (logand group-info #xF)) ; See <XKBstr.h>.
      ;; Check group number.
      (when (= group-number 0)
        (throw 'return '(0 . 0)))
      (setq group (logand (lsh modifiers -13) #b11)) ;The 13, 14 bits.
      ;; Wrap group.
      (when (>= group group-number)
        (pcase (logand group-info #xC0) ;See <XKBstr.h>.
          (`xcb:xkb:GroupsWrap:RedirectIntoRange
           (setq group (logand #xFF (lsh group-info -4))) ;See <XKBstr.h>.
           ;; Check if i's also out of range.
           (when (>= group group-number)
             (setq group 0)))
          (`xcb:xkb:GroupsWrap:ClampIntoRange
           (setq group (1- group-number)))
          (_
           (setq group (% group group-number)))))
      ;; Calculate the index of keysym.
      (setq index (* group (slot-value keycode 'width)))
      ;; Get key type.
      (setq keytype (aref xcb:keysyms:-keytypes
                          (elt (slot-value keycode 'kt-index) group)))
      ;; Find the shift level and preserved modifiers.
      (with-slots (mods-mask hasPreserve map (preserve* preserve)) keytype
        (catch 'break
          (dolist (entry map)
            (with-slots (active (mods-mask* mods-mask) level) entry
              (when (and (= 1 active)
                         (= (logand modifiers mods-mask) mods-mask*))
                (cl-incf index level)
                (when (= 1 hasPreserve)
                  (setq preserve (slot-value (elt preserve*
                                                  (cl-position entry map))
                                             'mask)))
                (throw 'break nil)))))
        ;; FIXME: Use of preserved modifiers (e.g. capitalize the keysym
        ;;        when LOCK is preserved)?
        (cons (elt (slot-value keycode 'syms) index)
              (logand mods-mask (lognot preserve)))))))

(cl-defmethod xcb:keysyms:keysym->keycode ((_obj xcb:connection) keysym)
  "Convert keysym to (the first matching) keycode.

Return 0 if conversion fails."
  (let ((index 0)
        (continue t))
    ;; Traverse all keycodes, column by column.
    ;; Reference: `XKeysymToKeycode' in 'XKBBind.c'.
    (catch 'break
      (when (= 0 keysym)
        (throw 'break 0))
      (while continue
        (setq continue nil)
        (dotimes (i (- xcb:keysyms:-max-keycode xcb:keysyms:-min-keycode -1))
          (with-slots (nSyms syms) (aref xcb:keysyms:-keycodes i)
            (when (< index nSyms)
              (setq continue t)
              (when (= keysym (elt syms index))
                (throw 'break (+ i xcb:keysyms:-min-keycode))))))
        (cl-incf index))
      0)))

;; This list is largely base on 'lispy_function_keys' in 'keyboard.c'.
(defconst xcb:keysyms:-function-keys
  `[                                    ;#xff00 - #xff0f
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
    lmeta* rmeta* lalt* ralt* lsuper* rsuper* lhyper* rhyper* nil
                                        ;#xff00 - #xffff
    ,@(make-list 15 nil) delete]
  "Emacs event representations of X function keys (keysym #xff00 to #xffff).")

;; From 'iso_lispy_function_keys' in 'keyboard.c'
(defconst xcb:keysyms:-iso-function-keys
  `[
                                        ;#xfe00 - #xfe0f
    ,@(make-list 16 nil)
                                        ;#xfe10 - #xfe1f
    ,@(make-list 16 nil)
                                        ;#xfe20 - #xfe2f
    iso-lefttab iso-move-line-up iso-move-line-down iso-partial-line-up
    iso-partial-line-down iso-partial-space-left iso-partial-space-right
    iso-set-margin-left iso-set-margin-right iso-release-margin-left
    iso-release-margin-right iso-release-both-margins iso-fast-cursor-left
    iso-fast-cursor-right iso-fast-cursor-up iso-fast-cursor-down
                                        ;#xfe30 - #xfe3f
    iso-continuous-underline iso-discontinuous-underline iso-emphasize
    iso-center-object iso-enter ,@(make-list 11 nil)
                                        ;everything else
    ,@(make-list 192 nil)]
  "Emacs event representations of ISO function keys (#xfe00 to #xfeff).")

;; This list is adapted from 'XF86keysym.h' in X source.
;; FIXME: We've intentionally left out keysyms outside the range 0x1008FF00 ~
;;        0x1008FFFF.
;; REVIEW: Could anybody verify this list?
(defconst xcb:keysyms:-xf86-keys
  `[                                    ;#x1008ff00 - #x1008ff0f
    nil XF86ModeLock XF86MonBrightnessUp XF86MonBrightnessDown
        XF86KbdLightOnOff XF86KbdBrightnessUp XF86KbdBrightnessDown
        ,@(make-list 9 nil)
                                        ;#x1008ff10 - #x1008ff1f
        XF86Standby XF86AudioLowerVolume XF86AudioMute XF86AudioRaiseVolume
        XF86AudioPlay XF86AudioStop XF86AudioPrev XF86AudioNext XF86HomePage
        XF86Mail XF86Start XF86Search XF86AudioRecord XF86Calculator XF86Memo
        XF86ToDoList
                                        ;#x1008ff20 - #x1008ff2f
        XF86Calendar XF86PowerDown XF86ContrastAdjust XF86RockerUp
        XF86RockerDown XF86RockerEnter XF86Back XF86Forward XF86Stop
        XF86Refresh XF86PowerOff XF86WakeUp XF86Eject XF86ScreenSaver XF86WWW
        XF86Sleep
                                        ;#x1008ff30 - #x1008ff3f
        XF86Favorites XF86AudioPause XF86AudioMedia XF86MyComputer
        XF86VendorHome XF86LightBulb XF86Shop XF86History XF86OpenURL
        XF86AddFavorite XF86HotLinks XF86BrightnessAdjust XF86Finance
        XF86Community XF86AudioRewind XF86BackForward
                                        ;#x1008ff40 - #x1008ff4f
        XF86Launch0 XF86Launch1 XF86Launch2 XF86Launch3 XF86Launch4 XF86Launch5
        XF86Launch6 XF86Launch7 XF86Launch8 XF86Launch9 XF86LaunchA XF86LaunchB
        XF86LaunchC XF86LaunchD XF86LaunchE XF86LaunchF
                                        ;#x1008ff50 - #x1008ff5f
        XF86ApplicationLeft XF86ApplicationRight XF86Book XF86CD XF86Calculater
        XF86Clear XF86Close XF86Copy XF86Cut XF86Display XF86DOS XF86Documents
        XF86Excel XF86Explorer XF86Game XF86Go
                                        ;#x1008ff60 - #x1008ff6f
        XF86iTouch XF86LogOff XF86Market XF86Meeting nil XF86MenuKB XF86MenuPB
        XF86MySites XF86New XF86News XF86OfficeHome XF86Open XF86Option
        XF86Paste XF86Phone nil
                                        ;#x1008ff70 - #x1008ff7f
        XF86Q nil XF86Reply XF86Reload XF86RotateWindows XF86RotationPB
        XF86RotationKB XF86Save XF86ScrollUp XF86ScrollDown XF86ScrollClick
        XF86Send XF86Spell XF86SplitScreen XF86Support XF86TaskPane
                                        ;#x1008ff80 - #x1008ff8f
        XF86Terminal XF86Tools XF86Travel nil XF86UserPB XF86User1KB
        XF86User2KB XF86Video XF86WheelButton XF86Word XF86Xfer XF86ZoomIn
        XF86ZoomOut XF86Away XF86Messenger XF86WebCam
                                        ;#x1008ff90 - #x1008ff9f
        XF86MailForward XF86Pictures XF86Music XF86Battery XF86Bluetooth
        XF86WLAN XF86UWB XF86AudioForward XF86AudioRepeat XF86AudioRandomPlay
        XF86Subtitle XF86AudioCycleTrack XF86CycleAngle XF86FrameBack
        XF86FrameForward XF86Time
                                        ;#x1008ffa0 - #x1008ffaf
        XF86Select XF86View XF86TopMenu XF86Red XF86Green XF86Yellow XF86Blue
        XF86Suspend XF86Hibernate XF86TouchpadToggle ,@(make-list 6 nil)
                                        ;#x1008ffb0 - #x1008ffbf
        XF86TouchpadOn XF86TouchpadOff XF86AudioMicMute ,@(make-list 13 nil)
                                        ;everything rest
        ,@(make-list 64 nil)]
  "Emacs event representations of XF86keysym (#x1008ff00 - #x1008ffff)")

(cl-defmethod xcb:keysyms:event->keysym ((obj xcb:connection) event)
  "Translate Emacs key event EVENT to (keysym . mod-mask).

Return (0 . 0) when conversion fails."
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
                (_
                 (cond
                  ((setq keysym (cl-position event
                                             xcb:keysyms:-function-keys))
                   ;; Function keys.
                   (logior keysym #xff00))
                  ((setq keysym (cl-position event xcb:keysyms:-xf86-keys))
                   ;; XF86 keys.
                   (logior keysym #x1008ff00))
                  ((setq keysym (cl-position event
                                             xcb:keysyms:-iso-function-keys))
                   ;; ISO function keys.
                   (logior keysym #xfe00))
                  (t
                   ;; Finally try system-specific keysyms.
                   (car (rassq event system-key-alist)))))))
      (setq keysym
            (cond
             ((<= #x20 event #xff)
              ;; Latin-1.
              event)
             ((<= #x100 event #x10ffff)
              ;; Unicode.
              (+ #x1000000 event))
             (t (or
                 ;; Try system-specific keysyms.
                 (car (rassq event system-key-alist))
                 ;; Try legacy keysyms.
                 (catch 'break
                   (maphash (lambda (key val)
                              (when (= event val)
                                (throw 'break key)))
                            x-keysym-table)))))))
    (if (not keysym)
        '(0 . 0)
      (let ((keycode (xcb:keysyms:keysym->keycode obj keysym))
            keysym*)
        (when (/= 0 keycode)
          (setq keysym* (xcb:keysyms:keycode->keysym obj keycode 0))
          (unless (= keysym (car keysym*))
            ;; This keysym requires additional modifiers to input.
            (push (cdr keysym*) modifiers))))
      (when modifiers
        ;; Do transforms: * -> x-*-keysym -> xcb:keysyms:*-mask.
        (setq modifiers (mapcar (lambda (i)
                                  (or (pcase i
                                        (`alt x-alt-keysym)
                                        (`meta x-meta-keysym)
                                        (`hyper x-hyper-keysym)
                                        (`super x-super-keysym))
                                      i))
                                modifiers)
              modifiers (mapcar (lambda (i)
                                  (pcase i
                                    ((and x (pred integerp)) x)
                                    (`meta xcb:keysyms:meta-mask)
                                    (`control xcb:keysyms:control-mask)
                                    (`shift xcb:keysyms:shift-mask)
                                    (`hyper xcb:keysyms:hyper-mask)
                                    (`super xcb:keysyms:super-mask)
                                    (`alt xcb:keysyms:alt-mask)
                                    (_
                                     ;; Include but not limit to: down.
                                     0)))
                                modifiers)))
      (cons keysym (apply #'logior modifiers)))))

(cl-defmethod xcb:keysyms:keysym->event ((_obj xcb:connection) keysym
                                         &optional mask allow-modifiers)
  "Translate X Keysym KEYSYM into Emacs key event.

One may use MASK to provide modifier keys.  If ALLOW-MODIFIERS is non-nil,
this function will also return symbols for pure modifiers keys."
  ;; Convert nil to 0.
  (unless mask
    (setq mask 0))
  (let ((event (cond ((<= #x20 keysym #xff)
                      keysym)
                     ((<= #xff00 keysym #xffff)
                      (aref xcb:keysyms:-function-keys (logand keysym #xff)))
                     ((<= #x1000100 keysym #x110ffff)
                      (- keysym #x1000000))
                     ((<= 1 keysym 5)   ;ButtonPress assuemd
                      (intern-soft (format "down-mouse-%d" keysym)))
                     ((<= #x1008ff00 keysym #x1008ffff)
                      (aref xcb:keysyms:-xf86-keys (logand keysym #xff)))
                     ((<= #xfe00 keysym #xfeff)
                      (aref xcb:keysyms:-iso-function-keys
                            (logand keysym #xff)))
                     (t (or
                         ;; Search system-specific keysyms.
                         (car (assq keysym system-key-alist))
                         ;; Search `x-keysym-table' for legacy keysyms.
                         (gethash keysym x-keysym-table)))))
        mod-alt mod-meta mod-hyper mod-super)
    (when event
      (if allow-modifiers
          (when (/= 0 mask)
            ;; Clear modifier bits for modifier keys.
            (pcase event
              ((or `lmeta* `rmeta*)
               (setq mask (logand mask (lognot xcb:keysyms:meta-mask))))
              ((or `lcontrol* `rcontrol*)
               (setq mask (logand mask (lognot xcb:keysyms:control-mask))))
              ((or `lshift* `rshift*)
               (setq mask (logand mask (lognot xcb:keysyms:shift-mask))))
              ((or `lhyper* `rhyper*)
               (setq mask (logand mask (lognot xcb:keysyms:hyper-mask))))
              ((or `lsuper* `rsuper*)
               (setq mask (logand mask (lognot xcb:keysyms:super-mask))))
              ((or `lalt* `ralt*)
               (setq mask (logand mask (lognot xcb:keysyms:alt-mask))))))
        (when (memq event
                    '(lshift*
                      rshift*
                      lcontrol*
                      rcontrol*
                      caps-lock*
                      shift-lock*
                      lmeta*
                      rmeta*
                      lalt*
                      ralt*
                      lsuper*
                      rsuper*
                      lhyper*
                      rhyper*
                      mode-switch*
                      kp-numlock))
          (setq event nil))))
    (when event
      (if (= 0 mask)
          event
        ;; Set mod-* if possible.
        (when x-alt-keysym
          (pcase x-alt-keysym
            (`meta (setq mod-meta 'alt))
            (`hyper (setq mod-hyper 'alt))
            (`super (setq mod-super 'alt))))
        (when x-meta-keysym
          (pcase x-meta-keysym
            (`alt (setq mod-alt 'meta))
            (`hyper (setq mod-hyper 'meta))
            (`super (setq mod-super 'meta))))
        (when x-hyper-keysym
          (pcase x-hyper-keysym
            (`alt (setq mod-alt 'hyper))
            (`meta (setq mod-meta 'hyper))
            (`super (setq mod-super 'hyper))))
        (when x-super-keysym
          (pcase x-super-keysym
            (`alt (setq mod-alt 'super))
            (`meta (setq mod-meta 'super))
            (`hyper (setq mod-hyper 'super))))
        ;; Convert modifiers.
        (setq event (list event))
        (when (/= 0 (logand mask xcb:keysyms:meta-mask))
          (push (or mod-meta 'meta) event))
        (when (/= 0 (logand mask xcb:keysyms:control-mask))
          (push 'control event))
        (when (and (/= 0 (logand mask (logior xcb:keysyms:shift-mask
                                              xcb:keysyms:shift-lock-mask)))
                   (or (not (<= #x20 keysym #xff)) ;Not a Latin-1 character
                       (<= ?A keysym ?Z)))         ;An uppercase letter
          (push 'shift event))
        (when (/= 0 (logand mask xcb:keysyms:hyper-mask))
          (push (or mod-hyper 'hyper) event))
        (when (/= 0 (logand mask xcb:keysyms:super-mask))
          (push (or mod-super 'super) event))
        (when (/= 0 (logand mask xcb:keysyms:alt-mask))
          (push (or mod-alt 'alt) event))
        (event-convert-list event)))))



(provide 'xcb-keysyms)

;;; xcb-keysyms.el ends here
