;;; xcb-renderutil.el --- Utility functions for  -*- lexical-binding: t -*-
;;;                       the Render extension

;; Copyright (C) 2016 Free Software Foundation, Inc.

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

;; This library provides utility functions for the Render extension.

;; Todo
;; + Glyph-related functions are not implemented.

;; References:
;; + xcb/util-renderutil (git://anongit.freedesktop.org/xcb/util-renderutil)

;;; Code:

(require 'xcb)
(require 'xcb-render)

;; Protocol version.
(defconst xcb:renderutil:-MAJOR_VERSION 0)
(defconst xcb:renderutil:-MINOR_VERSION 11)

;; PICTFORMINFO masks.
(defconst xcb:renderutil:PICT_FORMAT:ID         1)
(defconst xcb:renderutil:PICT_FORMAT:TYPE       2)
(defconst xcb:renderutil:PICT_FORMAT:DEPTH      4)
(defconst xcb:renderutil:PICT_FORMAT:RED        8)
(defconst xcb:renderutil:PICT_FORMAT:RED_MASK   16)
(defconst xcb:renderutil:PICT_FORMAT:GREEN      32)
(defconst xcb:renderutil:PICT_FORMAT:GREEN_MASK 64)
(defconst xcb:renderutil:PICT_FORMAT:BLUE       128)
(defconst xcb:renderutil:PICT_FORMAT:BLUE_MASK  256)
(defconst xcb:renderutil:PICT_FORMAT:ALPHA      512)
(defconst xcb:renderutil:PICT_FORMAT:ALPHA_MASK 1024)
(defconst xcb:renderutil:PICT_FORMAT:COLORMAP   2048)

;; Indices of standard PictFormats.
(defconst xcb:renderutil:PICT_STANDARD:ARGB_32 0)
(defconst xcb:renderutil:PICT_STANDARD:RGB_24  1)
(defconst xcb:renderutil:PICT_STANDARD:A_8     2)
(defconst xcb:renderutil:PICT_STANDARD:A_4     3)
(defconst xcb:renderutil:PICT_STANDARD:A_1     4)

(defconst xcb:renderutil:STANDARD-TEMPLATES
  (list
   ;; xcb:renderutil:PICT_STANDARD:ARGB_32
   (vector (make-instance 'xcb:render:PICTFORMINFO
                          :id 0
                          :type xcb:render:PictType:Direct
                          :depth 32
                          :direct (make-instance 'xcb:render:DIRECTFORMAT
                                                 :red-shift 16
                                                 :red-mask #xFF
                                                 :green-shift 8
                                                 :green-mask #xFF
                                                 :blue-shift 0
                                                 :blue-mask #xFF
                                                 :alpha-shift 24
                                                 :alpha-mask #xFF)
                          :colormap 0)
           (logior xcb:renderutil:PICT_FORMAT:TYPE
                   xcb:renderutil:PICT_FORMAT:DEPTH
                   xcb:renderutil:PICT_FORMAT:RED
                   xcb:renderutil:PICT_FORMAT:RED_MASK
                   xcb:renderutil:PICT_FORMAT:GREEN
                   xcb:renderutil:PICT_FORMAT:GREEN_MASK
                   xcb:renderutil:PICT_FORMAT:BLUE
                   xcb:renderutil:PICT_FORMAT:BLUE_MASK
                   xcb:renderutil:PICT_FORMAT:ALPHA
                   xcb:renderutil:PICT_FORMAT:ALPHA_MASK))
   ;; xcb:renderutil:PICT_STANDARD:RGB_24
   (vector (make-instance 'xcb:render:PICTFORMINFO
                          :id 0
                          :type xcb:render:PictType:Direct
                          :depth 24
                          :direct (make-instance 'xcb:render:DIRECTFORMAT
                                                 :red-shift 16
                                                 :red-mask #xFF
                                                 :green-shift 8
                                                 :green-mask #xFF
                                                 :blue-shift 0
                                                 :blue-mask #xFF
                                                 :alpha-shift 0
                                                 :alpha-mask #x00)
                          :colormap 0)
           (logior xcb:renderutil:PICT_FORMAT:TYPE
                   xcb:renderutil:PICT_FORMAT:DEPTH
                   xcb:renderutil:PICT_FORMAT:RED
                   xcb:renderutil:PICT_FORMAT:RED_MASK
                   xcb:renderutil:PICT_FORMAT:GREEN
                   xcb:renderutil:PICT_FORMAT:GREEN_MASK
                   xcb:renderutil:PICT_FORMAT:BLUE
                   xcb:renderutil:PICT_FORMAT:BLUE_MASK
                   xcb:renderutil:PICT_FORMAT:ALPHA_MASK))
   ;; xcb:renderutil:PICT_STANDARD:A_8
   (vector (make-instance 'xcb:render:PICTFORMINFO
                          :id 0
                          :type xcb:render:PictType:Direct
                          :depth 8
                          :direct (make-instance 'xcb:render:DIRECTFORMAT
                                                 :red-shift 0
                                                 :red-mask #x00
                                                 :green-shift 0
                                                 :green-mask #x00
                                                 :blue-shift 0
                                                 :blue-mask #x00
                                                 :alpha-shift 0
                                                 :alpha-mask #xFF)
                          :colormap 0)
           (logior xcb:renderutil:PICT_FORMAT:TYPE
                   xcb:renderutil:PICT_FORMAT:DEPTH
                   xcb:renderutil:PICT_FORMAT:RED_MASK
                   xcb:renderutil:PICT_FORMAT:GREEN_MASK
                   xcb:renderutil:PICT_FORMAT:BLUE_MASK
                   xcb:renderutil:PICT_FORMAT:ALPHA
                   xcb:renderutil:PICT_FORMAT:ALPHA_MASK))
   ;; xcb:renderutil:PICT_STANDARD:A_4
   (vector (make-instance 'xcb:render:PICTFORMINFO
                          :id 0
                          :type xcb:render:PictType:Direct
                          :depth 4
                          :direct (make-instance 'xcb:render:DIRECTFORMAT
                                                 :red-shift 0
                                                 :red-mask #x00
                                                 :green-shift 0
                                                 :green-mask #x00
                                                 :blue-shift 0
                                                 :blue-mask #x00
                                                 :alpha-shift 0
                                                 :alpha-mask #x0F)
                          :colormap 0)
           (logior xcb:renderutil:PICT_FORMAT:TYPE
                   xcb:renderutil:PICT_FORMAT:DEPTH
                   xcb:renderutil:PICT_FORMAT:RED_MASK
                   xcb:renderutil:PICT_FORMAT:GREEN_MASK
                   xcb:renderutil:PICT_FORMAT:BLUE_MASK
                   xcb:renderutil:PICT_FORMAT:ALPHA
                   xcb:renderutil:PICT_FORMAT:ALPHA_MASK))
   ;; xcb:renderutil:PICT_STANDARD:A_1
   (vector (make-instance 'xcb:render:PICTFORMINFO
                          :id 0
                          :type xcb:render:PictType:Direct
                          :depth 1
                          :direct (make-instance 'xcb:render:DIRECTFORMAT
                                                 :red-shift 0
                                                 :red-mask #x00
                                                 :green-shift 0
                                                 :green-mask #x00
                                                 :blue-shift 0
                                                 :blue-mask #x00
                                                 :alpha-shift 0
                                                 :alpha-mask #x01)
                          :colormap 0)
           (logior xcb:renderutil:PICT_FORMAT:TYPE
                   xcb:renderutil:PICT_FORMAT:DEPTH
                   xcb:renderutil:PICT_FORMAT:RED_MASK
                   xcb:renderutil:PICT_FORMAT:GREEN_MASK
                   xcb:renderutil:PICT_FORMAT:BLUE_MASK
                   xcb:renderutil:PICT_FORMAT:ALPHA
                   xcb:renderutil:PICT_FORMAT:ALPHA_MASK)))
  "Standard PictFormats.")

(cl-defmethod xcb:renderutil:-get-cache ((obj xcb:connection))
  "Return the cache and initialize the extension when first called."
  (let ((result (plist-get (slot-value obj 'extra-plist) 'renderutil))
        required-depths)
    (unless (or result
                (= 0 (slot-value
                      (xcb:get-extension-data obj 'xcb:render)
                      'present)))
      (setq result
            (vector (xcb:+request-unchecked+reply obj
                        (make-instance 'xcb:render:QueryVersion
                                       :client-major-version
                                       xcb:renderutil:-MAJOR_VERSION
                                       :client-minor-version
                                       xcb:renderutil:-MINOR_VERSION))
                    (xcb:+request-unchecked+reply obj
                        (make-instance 'xcb:render:QueryPictFormats))))
      (setq required-depths '(1 4 8 24 32))
      (catch 'break
        (dolist (s (slot-value (aref result 1) 'screens))
          (dolist (d (slot-value s 'depths))
            (setq required-depths
                  (delq (slot-value d 'depth) required-depths))
            (unless required-depths
              (throw 'break nil)))))
      (if required-depths
          (setq result nil)
        (setf (slot-value obj 'extra-plist)
              (plist-put (slot-value obj 'extra-plist) 'renderutil result))))
    result))

(defun xcb:renderutil:find-visual-format (formats visual)
  "Search FORMATS for a format matching visual VISUAL."
  (catch 'return
    (dolist (s (slot-value formats 'screens))
      (dolist (d (slot-value s 'depths))
        (dolist (v (slot-value d 'visuals))
          (when (= (slot-value v 'visual) visual)
            (throw 'return (slot-value v 'format))))))))

(defun xcb:renderutil:find-format (formats mask template count)
  "Search FORMATS for a format matching mask MASK and template TEMPLATE.

Return COUNT-th match."
  (catch 'return
    (unless formats
      (throw 'return nil))
    (dolist (f (slot-value formats 'formats))
      (when (and (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:ID))
                     (eq (slot-value template 'id) (slot-value f 'id))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:TYPE))
                     (eq (slot-value template 'type) (slot-value f 'type))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:DEPTH))
                     (eq (slot-value template 'depth) (slot-value f 'depth))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:RED))
                     (eq (slot-value (slot-value template 'direct) 'red-shift)
                         (slot-value (slot-value f 'direct) 'red-shift))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:RED_MASK))
                     (eq (slot-value (slot-value template 'direct) 'red-mask)
                         (slot-value (slot-value f 'direct) 'red-mask))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:GREEN))
                     (eq (slot-value (slot-value template 'direct) 'red-shift)
                         (slot-value (slot-value f 'direct) 'red-shift))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:GREEN_MASK))
                     (eq (slot-value (slot-value template 'direct) 'red-mask)
                         (slot-value (slot-value f 'direct) 'red-mask))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:BLUE))
                     (eq (slot-value (slot-value template 'direct) 'red-shift)
                         (slot-value (slot-value f 'direct) 'red-shift))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:BLUE_MASK))
                     (eq (slot-value (slot-value template 'direct) 'red-mask)
                         (slot-value (slot-value f 'direct) 'red-mask))
                   t)
                 (if (/= 0 (logand mask xcb:renderutil:PICT_FORMAT:COLORMAP))
                     (eq (slot-value template 'colormap)
                         (slot-value f 'colormap))
                   t))
        (when (= count 0)
          (throw 'return (slot-value f 'id))
          (cl-decf count))))))

(defun xcb:renderutil:find-standard (formats format)
  "Search FORMATS for a standard format matching format ID FORMAT."
  (when (and (<= 0 format (1- (length xcb:renderutil:STANDARD-TEMPLATES))))
    (let ((standard-format (elt xcb:renderutil:STANDARD-TEMPLATES format)))
      (xcb:renderutil:find-format formats
                                  (aref standard-format 1)
                                  (aref standard-format 0)
                                  0))))

(cl-defmethod xcb:renderutil:query-version ((obj xcb:connection))
  "Return the version of Render extension."
  (let ((cache (xcb:renderutil:-get-cache obj)))
    (when cache
      (aref cache 0))))

(cl-defmethod xcb:renderutil:query-formats ((obj xcb:connection))
  "Return supported formats of this X server."
  (let ((cache (xcb:renderutil:-get-cache obj)))
    (when cache
      (aref cache 1))))



(provide 'xcb-renderutil)

;;; xcb-renderutil.el ends here
