;;; xcb-cursor.el --- Port of Xcursor  -*- lexical-binding: t -*-

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

;; This library is a port of Xcursor in Xlib, and roughly corresponds to the
;; xcb/util-cursor project.

;; Usage tips:
;; + Do not forget to call `xcb:cursor:init' for _every_ connection using this
;;   library.
;; + The only useful method in this library is `xcb:cursor:load-cursor', which
;;   loads a cursor by its name (e.g. "left_ptr"), in the following order:
;;   1. themed cursor
;;   2. inherited themed cursor
;;   3. standard X cursor

;; Todo:
;; + Add legacy support for RENDER.
;; + Cursor should be set per screen (only the first is used right now).
;; + Move codes corresponding to xcb/util-renderutil or xcb/util-image
;;   elsewhere.

;; References:
;; + Xcursor(3).
;; + xcb/util-cursor (git://anongit.freedesktop.org/xcb/util-cursor)
;; + xcb/util-renderutil (git://anongit.freedesktop.org/xcb/util-renderutil)
;; + xcb/util-image (git://anongit.freedesktop.org/xcb/util-image)

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'xcb)
(require 'xcb-render)

;; FIXME: check if resource manager really works
(cl-defmethod xcb:cursor:init ((obj xcb:connection))
  "Initialize Xcursor for connection OBJ."
  ;; Initialize resource manager
  (let* ((root (slot-value (car (slot-value (xcb:get-setup obj) 'roots))
                           'root))
         (rm (xcb:+request-unchecked+reply obj
                 (make-instance 'xcb:GetProperty
                                :delete 0 :window root
                                :property xcb:Atom:RESOURCE_MANAGER
                                :type xcb:Atom:STRING
                                :long-offset 0
                                :long-length 16384))) ;FIXME: xcb/util-cursor
         (rm (split-string
              (decode-coding-string
               (apply #'unibyte-string (append (slot-value rm 'value) nil))
               'iso-latin-1)
              "\n"))
         theme size dpi)
    (dolist (i rm)
      (pcase (replace-regexp-in-string "^\\(\\S-+\\)" "\\1" i)
        ("Xcursor.theme"
         (setq theme
               (replace-regexp-in-string "^[^:]+:\\s-*\\(.+$\\)" "\\1" i)))
        ("Xcursor.size"
         (setq size
               (string-to-number
                (replace-regexp-in-string "^[^:]+:\\s-*\\(.+$\\)" "\\1" i))))
        ("Xft.dpi"
         (setq dpi
               (string-to-number
                (replace-regexp-in-string "^[^:]+:\\s-*\\(.+$\\)" "\\1" i))))))
    ;; Get cursor size from XCURSOR_SIZE environment variable
    (let ((default-size (getenv "XCURSOR_SIZE")))
      (when default-size
        (setq default-size (string-to-number default-size)))
      (setq size (or default-size size)))
    ;; Alternatives
    (when (and (not size) dpi)
      (setq size (/ (* dpi 16) 72)))    ;FIXME: xcb/util-cursor
    (unless size
      (setq size
            ;; FIXME: xcb/util-cursor
            (/ (min (x-display-pixel-width) (x-display-pixel-height)) 48)))
    ;; Save default values
    (let ((plist (plist-get (slot-value obj 'extra-plist) 'cursor)))
      (setq plist (plist-put plist 'theme theme)
            plist (plist-put plist 'size size))
      (setf (slot-value obj 'extra-plist)
            (plist-put (slot-value obj 'extra-plist) 'cursor plist))))
  ;; Initialize render extension
  (if (= 0 (slot-value (xcb:get-extension-data obj 'xcb:render) 'present))
      (error "[XELB:CURSOR] Render extension is not supported by this server")
    (with-slots (minor-version)
        (xcb:+request-unchecked+reply obj
            (make-instance 'xcb:render:QueryVersion
                           :client-major-version 0 :client-minor-version 8))
      (if (> 8 minor-version)
          (error "[XELB:CURSOR] Render version 0.8 is not supported")
        (let* ((formats
                (slot-value (xcb:+request-unchecked+reply obj
                                (make-instance 'xcb:render:QueryPictFormats))
                            'formats))
               (format (catch 'break
                         (dolist (i formats)
                           (with-slots (type depth direct) i
                             (with-slots (red-shift red-mask
                                                    green-shift green-mask
                                                    blue-shift blue-mask
                                                    alpha-shift alpha-mask)
                                 direct
                               ;; FIXME: xcb/util-renderutil
                               (when (and (= type xcb:render:PictType:Direct)
                                          (= depth 32)
                                          (= red-shift 16) (= red-mask #xFF)
                                          (= green-shift 8) (= green-mask #xFF)
                                          (= blue-shift 0) (= blue-mask #xFF)
                                          (= alpha-shift 24)
                                          (= alpha-mask #xFF))
                                 (throw 'break i)))))))
               (plist (plist-get (slot-value obj 'extra-plist) 'cursor)))
          (setf (slot-value obj 'extra-plist)
                (plist-put (slot-value obj 'extra-plist) 'cursor
                           (plist-put plist 'pict-format format))))))))

(defsubst xcb:cursor:-get-path ()
  "Return a list of cursor paths."
  (let ((path (getenv "XCURSOR_PATH")))
    (if path
        (split-string path ":" t)
      '("~/.icons"
        "/usr/share/icons"
        "/usr/share/pixmaps"
        "/usr/X11R6/lib/X11/icons"))))

(defun xcb:cursor:-get-theme-inherits (file)
  "Return the inherited themes in a index.theme file FILE."
  (let ((lines (with-temp-buffer
                 (insert-file-contents file)
                 (split-string (buffer-string) "\n" t))))
    (catch 'break
      (dolist (line lines)
        (when (string-match "^Inherits\\s-*=\\s-*" line)
          (throw 'break
                 (split-string (replace-regexp-in-string "^[^=]+=\\(.*\\)$"
                                                         "\\1" line)
                               "[;, \t\n]+" t)))))))

(defsubst xcb:cursor:-shape->id (name)
  "Return the standard Xcursor font for cursor named NAME."
  ;; Standard X cursor fonts are defined in Emacs
  (intern-soft (concat "x-pointer-" (replace-regexp-in-string "_" "-" name))))

(defun xcb:cursor:-find-file (theme name &optional skip)
  "Return the file for cursor named NAME in theme THEME, or nil if not found."
  (catch 'return
    ;; Skip searched themes
    (when (memq theme skip)
      (throw 'return nil))
    ;; Give up when supplied "core" theme and a valid cursor name
    (when (and (string= "core" theme) (xcb:cursor:-shape->id name))
      (throw 'return nil))
    (let ((path (xcb:cursor:-get-path))
          file)
      ;; 1. try THEME/cursors/NAME in each cursor path
      (dolist (i path)
        (setq file (concat i "/" theme "/cursors/" name))
        (when (file-readable-p file)
          (throw 'return file)))
      ;; 2. try "Inherits=" key in "index.theme"
      (dolist (i path)
        (setq file (concat i "/" theme "/index.theme"))
        (when (file-readable-p file)
          (cl-pushnew theme skip)
          ;; try all inherited themes
          (dolist (j (xcb:cursor:-get-theme-inherits file))
            (setq file (xcb:cursor:-find-file j name skip))
            (when file
              (throw 'return file))
            (cl-pushnew j skip)))))
    nil))

(defconst xcb:cursor:-file-magic-lsb "Xcur"
  "The magic number for little-endian Xcursor file.")
(defconst xcb:cursor:-file-magic-msb "rucX"
  "The magic number for big-endian Xcursor file.")

(defclass xcb:cursor:-file-header (xcb:-struct)
  ((magic :type xcb:CARD32)
   (header :type xcb:CARD32)
   (version :type xcb:CARD32)
   (ntoc :type xcb:CARD32)) ;redundant, required for calculating TOC bytes
  :documentation "Xcursor file header.")

(defclass xcb:cursor:-file-header-toc (xcb:-struct)
  ((ntoc :type xcb:CARD32)              ;redundant slot
   (toc :type xcb:-ignore)
   (toc~ :initform '(name toc type xcb:cursor:-file-toc
                          size (xcb:-fieldref 'ntoc))
         :type xcb:-list))
  :documentation "The TOC field in Xcursor file header.")

(defclass xcb:cursor:-file-toc (xcb:-struct)
  ((type :type xcb:CARD32)
   (subtype :type xcb:CARD32)
   (position :type xcb:CARD32))
  :documentation "Xcursor file TOC entry.")

(defclass xcb:cursor:-file-chunk-header (xcb:-struct)
  ((header :type xcb:CARD32)
   (type :type xcb:CARD32)
   (subtype :type xcb:CARD32)
   (version :type xcb:CARD32)
   (width :type xcb:CARD32) ;redundant, required for calculating image bytes
   (height :type xcb:CARD32)) ;redundant, required for calculating image bytes
  :documentation "Xcursor file chunk header.")

(defconst xcb:cursor:-file-chunk-image-header 36
  "Header value of image-type chunk in Xcursor file.")
(defconst xcb:cursor:-file-chunk-image-type 4294770690.
  "Type of image-type chunk in Xcursor file.")
(defconst xcb:cursor:-file-chunk-image-version 1
  "Version of image-type chunk in Xcursor file.")

(defclass xcb:cursor:-file-chunk-image (xcb:-struct)
  ((width :type xcb:CARD32)             ;<= #x7FFF, redundant
   (height :type xcb:CARD32)            ;<= #x7FFF, redundant
   (xhot :type xcb:CARD32)              ;<= width
   (yhot :type xcb:CARD32)              ;<= height
   (delay :type xcb:CARD32)             ;in ms
   (pixels :type xcb:-ignore)
   (pixels~ :initform '(name pixels type xcb:CARD32
                             size (* (xcb:-fieldref 'width)
                                     (xcb:-fieldref 'height)))
            :type xcb:-list))
  :documentation "Image-type chunk in Xcursor file.")

(cl-defmethod xcb:cursor:-parse-file ((obj xcb:connection) path)
  "Parse an Xcursor file named PATH."
  (catch 'return
    (let ((data (let ((coding-system-for-read 'binary))
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents path) (buffer-string))))
          xcb:lsb                       ;override global byte order
          best-size chunks
          magic file-header file-header-toc chunk-header chunk)
      ;; Determine byte order
      (setq magic (substring data 0 4))
      (if (string= xcb:cursor:-file-magic-lsb magic)
          (setq xcb:lsb t)              ;LSB first
        (if (string= xcb:cursor:-file-magic-msb magic)
            (setq xcb:lsb nil)          ;MSB first
          (throw 'return nil)))
      (setq file-header (make-instance 'xcb:cursor:-file-header))
      ;;
      (xcb:unmarshal file-header (substring data 0 16))
      ;; FIXME: checks
      (setq file-header-toc (make-instance 'xcb:cursor:-file-header-toc))
      (xcb:unmarshal file-header-toc
                     (substring data 12 (+ 16 (* 12 (slot-value file-header
                                                                'ntoc)))))
      (with-slots (toc) file-header-toc
        (let ((target (plist-get
                       (plist-get (slot-value obj 'extra-plist) 'cursor)
                       'size)))
          (catch 'break
            (dolist (i toc)
              (with-slots (type subtype) i
                (when (= type xcb:cursor:-file-chunk-image-type)
                  (when (= target subtype)
                    (setq best-size target)
                    (throw 'break nil))
                  (when (or (not best-size)
                            (> (abs (- target best-size))
                               (abs (- target subtype))))
                    (setq best-size subtype)))))))
        ;; Collect chunks fitting this size
        (setq chunk-header (make-instance 'xcb:cursor:-file-chunk-header))
        (dolist (i toc)
          (with-slots (type subtype position) i
            (when (and (= type xcb:cursor:-file-chunk-image-type)
                       (= subtype best-size))
              (xcb:unmarshal chunk-header (substring data position
                                                     (+ position 24)))
              ;; Validate the header of this chunk
              (with-slots (header type subtype version) chunk-header
                (when (or (/= header xcb:cursor:-file-chunk-image-header)
                          (/= type xcb:cursor:-file-chunk-image-type)
                          (/= subtype best-size)
                          (/= version xcb:cursor:-file-chunk-image-version))
                  (throw 'return nil)))
              ;; Parse this chunk
              (setq chunk (make-instance 'xcb:cursor:-file-chunk-image))
              (xcb:unmarshal chunk (substring data (+ position 16)
                                              (+ position 36
                                                 (* 4
                                                    (slot-value chunk-header
                                                                'width)
                                                    (slot-value chunk-header
                                                                'height)))))
              (setq chunks (nconc chunks (list chunk))))))
        (list xcb:lsb chunks)))))

(cl-defmethod xcb:cursor:-load-cursor ((obj xcb:connection) file)
  "Load a cursor file FILE."
  (let* ((images (xcb:cursor:-parse-file obj file))
         (lsb (car images))
         (images (cadr images))
         (root (slot-value (car (slot-value (xcb:get-setup obj) 'roots))
                           'root))
         (picture (xcb:generate-id obj))
         (pict-format (slot-value
                       (plist-get
                        (plist-get (slot-value obj 'extra-plist) 'cursor)
                        'pict-format)
                       'id))
         pixmap gc cursors cursor last-width last-height)
    (dolist (image images)
      (with-slots (width height xhot yhot delay pixels) image
        (when (or (not pixmap) (/= last-width width) (/= last-height height))
          (if pixmap
              (progn (xcb:+request obj (make-instance 'xcb:FreePixmap
                                                      :pixmap pixmap))
                     (xcb:+request obj (make-instance 'xcb:FreeGC :gc gc)))
            (setq pixmap (xcb:generate-id obj)
                  gc (xcb:generate-id obj)))
          (xcb:+request obj (make-instance 'xcb:CreatePixmap
                                           :depth 32 :pid pixmap :drawable root
                                           :width width :height height))
          (xcb:+request obj (make-instance 'xcb:CreateGC
                                           :cid gc :drawable pixmap
                                           :value-mask 0))
          (setq last-width width
                last-height height))
        (xcb:+request obj (make-instance 'xcb:PutImage
                                         :format xcb:ImageFormat:ZPixmap
                                         :drawable pixmap
                                         :gc gc
                                         :width width
                                         :height height
                                         :dst-x 0
                                         :dst-y 0
                                         :left-pad 0
                                         :depth 32
                                         :data (with-temp-buffer
                                                 (set-buffer-multibyte nil)
                                                 (mapconcat
                                                  (if lsb #'xcb:-pack-u4-lsb
                                                    #'xcb:-pack-u4)
                                                  pixels []))))
        (xcb:+request obj (make-instance 'xcb:render:CreatePicture
                                         :pid picture
                                         :drawable pixmap
                                         :format pict-format
                                         :value-mask 0))
        (setq cursor (xcb:generate-id obj)
              cursors (nconc cursors
                             (list (make-instance 'xcb:render:ANIMCURSORELT
                                                  :cursor cursor
                                                  :delay delay))))
        (xcb:+request obj (make-instance 'xcb:render:CreateCursor
                                         :cid cursor
                                         :source picture
                                         :x xhot :y yhot))
        (xcb:+request obj (make-instance 'xcb:render:FreePicture
                                         :picture picture))))
    (xcb:+request obj (make-instance 'xcb:FreePixmap :pixmap pixmap))
    (xcb:+request obj (make-instance 'xcb:FreeGC :gc gc))
    (xcb:flush obj)
    (if (= 1 (length cursors))
        ;; Non-animated cursor
        (slot-value (car cursors) 'cursor)
      ;; Animated cursor
      (setq cursor (xcb:generate-id obj))
      (xcb:+request obj (make-instance 'xcb:render:CreateAnimCursor
                                       :cid cursor
                                       :cursors (vconcat cursors)))
      (dolist (i cursors)
        (xcb:+request obj (make-instance 'xcb:FreeCursor
                                         :cursor (slot-value i 'cursor))))
      (xcb:flush obj)
      cursor)))

(cl-defmethod xcb:cursor:load-cursor ((obj xcb:connection) name)
  "Return a cursor whose name is NAME."
  (let* ((theme (or (plist-get
                     (plist-get (slot-value obj 'extra-plist) 'cursor) 'theme)
                    "default"))
         (file (xcb:cursor:-find-file theme name)))
    (if file
        (xcb:cursor:-load-cursor obj file)
      ;; Fallback to standard X cursors
      (let ((pointer (xcb:cursor:-shape->id name))
            (cursor xcb:Cursor:None)
            font)
        (when (boundp pointer)
          (setq pointer (symbol-value pointer)
                font (xcb:generate-id obj)
                cursor (xcb:generate-id obj))
          (xcb:+request obj
              (make-instance 'xcb:OpenFont
                             :fid font :name-len (length "cursor")
                             :name "cursor"))
          (xcb:+request obj
              (make-instance 'xcb:CreateGlyphCursor
                             :cid cursor :source-font font :mask-font font
                             :source-char pointer :mask-char (1+ pointer)
                             :fore-red 0 :fore-green 0 :fore-blue 0
                             :back-red #xFFFF :back-green #xFFFF
                             :back-blue #xFFFF))
          (xcb:flush obj))
        cursor))))



(provide 'xcb-cursor)

;;; xcb-cursor.el ends here
