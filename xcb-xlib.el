;;; xcb-xlib.el --- Port of Xlib  -*- lexical-binding: t -*-

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

;;  This file currently only contains constants from 'Xlib.h'

;;; Code:

(defconst xlib:XNRequiredCharSet "requiredCharSet")
(defconst xlib:XNQueryOrientation "queryOrientation")
(defconst xlib:XNBaseFontName "baseFontName")
(defconst xlib:XNOMAutomatic "omAutomatic")
(defconst xlib:XNMissingCharSet "missingCharSet")
(defconst xlib:XNDefaultString "defaultString")
(defconst xlib:XNOrientation "orientation")
(defconst xlib:XNDirectionalDependentDrawing "directionalDependentDrawing")
(defconst xlib:XNContextualDrawing "contextualDrawing")
(defconst xlib:XNFontInfo "fontInfo")

(defconst xlib:XNVaNestedList "XNVaNestedList")
(defconst xlib:XNQueryInputStyle "queryInputStyle")
(defconst xlib:XNClientWindow "clientWindow")
(defconst xlib:XNInputStyle "inputStyle")
(defconst xlib:XNFocusWindow "focusWindow")
(defconst xlib:XNResourceName "resourceName")
(defconst xlib:XNResourceClass "resourceClass")
(defconst xlib:XNGeometryCallback "geometryCallback")
(defconst xlib:XNDestroyCallback "destroyCallback")
(defconst xlib:XNFilterEvents "filterEvents")
(defconst xlib:XNPreeditStartCallback "preeditStartCallback")
(defconst xlib:XNPreeditDoneCallback "preeditDoneCallback")
(defconst xlib:XNPreeditDrawCallback "preeditDrawCallback")
(defconst xlib:XNPreeditCaretCallback "preeditCaretCallback")
(defconst xlib:XNPreeditStateNotifyCallback "preeditStateNotifyCallback")
(defconst xlib:XNPreeditAttributes "preeditAttributes")
(defconst xlib:XNStatusStartCallback "statusStartCallback")
(defconst xlib:XNStatusDoneCallback "statusDoneCallback")
(defconst xlib:XNStatusDrawCallback "statusDrawCallback")
(defconst xlib:XNStatusAttributes "statusAttributes")
(defconst xlib:XNArea "area")
(defconst xlib:XNAreaNeeded "areaNeeded")
(defconst xlib:XNSpotLocation "spotLocation")
(defconst xlib:XNColormap "colorMap")
(defconst xlib:XNStdColormap "stdColorMap")
(defconst xlib:XNForeground "foreground")
(defconst xlib:XNBackground "background")
(defconst xlib:XNBackgroundPixmap "backgroundPixmap")
(defconst xlib:XNFontSet "fontSet")
(defconst xlib:XNLineSpace "lineSpace")
(defconst xlib:XNCursor "cursor")
(defconst xlib:XNQueryIMValuesList "queryIMValuesList")
(defconst xlib:XNQueryICValuesList "queryICValuesList")
(defconst xlib:XNVisiblePosition "visiblePosition")
(defconst xlib:XNR6PreeditCallback "r6PreeditCallback")
(defconst xlib:XNStringConversionCallback "stringConversionCallback")
(defconst xlib:XNStringConversion "stringConversion")
(defconst xlib:XNResetState "resetState")
(defconst xlib:XNHotKey "hotKey")
(defconst xlib:XNHotKeyState "hotKeyState")
(defconst xlib:XNPreeditState "preeditState")
(defconst xlib:XNSeparatorofNestedList "separatorofNestedList")

(defconst xlib:XIMPreeditArea #x0001)
(defconst xlib:XIMPreeditCallbacks #x0002)
(defconst xlib:XIMPreeditPosition #x0004)
(defconst xlib:XIMPreeditNothing #x0008)
(defconst xlib:XIMPreeditNone #x0010)
(defconst xlib:XIMStatusArea #x0100)
(defconst xlib:XIMStatusCallbacks #x0200)
(defconst xlib:XIMStatusNothing #x0400)
(defconst xlib:XIMStatusNone #x0800)

(defconst xlib:XIMReverse #x001)
(defconst xlib:XIMUnderline #x002)
(defconst xlib:XIMHighlight #x004)
(defconst xlib:XIMPrimary #x010)
(defconst xlib:XIMSecondary #x020)
(defconst xlib:XIMTertiary #x040)
(defconst xlib:XIMVisibleToForward #x080)
(defconst xlib:XIMVisibleToBackword #x100)
(defconst xlib:XIMVisibleToCenter #x200)

(defconst xlib:XBufferOverflow -1)
(defconst xlib:XLookupNone 1)
(defconst xlib:XLookupChars 2)
(defconst xlib:XLookupKeySym 3)
(defconst xlib:XLookupBoth 4)



(provide 'xcb-xlib)

;;; xcb-xlib.el ends here
