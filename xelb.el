;;; xelb.el --- X protocol Emacs Lisp Binding  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Maintainer: Chris Feng <chris.w.feng@gmail.com>
;; Version: 0.14
;; Package-Requires: ((emacs "24.4") (cl-generic "0.2"))
;; Keywords: unix
;; URL: https://github.com/ch11ng/xelb

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

;; Overview
;; --------
;; XELB (X protocol Emacs Lisp Binding) is a pure Elisp implementation of X11
;; protocol based on the XML description files from XCB project.  It features
;; an object-oriented API and permits a certain degree of concurrency.  It
;; should enable you to implement some low-level X11 applications.

;; How it works
;; ------------
;; As is well known, X11 is a network-transparent protocol.  All its messages,
;; including requests, replies, events, errors, etc are transported over
;; network.  Considering that Emacs is powerful enough to do network
;; communication, it is also possible to use Emacs to send / receive those X11
;; messages.  Here we fully exploit the asynchronous feature of network
;; connections in Emacs, making XELB concurrent in a sense.

;; X11 protocol is somewhat complicated, especially when extension protocols
;; are also concerned.  Fortunately, XCB project has managed to describe these
;; protocols as XML files, which are language-neutral and can be used to
;; generate language-specific bindings.  In XELB, X messages are represented as
;; 'classes', and their 'methodes' are provided to translate them to / from raw
;; byte arrays conveniently.

;; Usage
;; -----
;; Interfaces are mainly defined in 'xcb.el'.  Please refer to that file on how
;; to use them.  Most of other files are either X11 core / extension protocol
;; libraries (e.g. xcb-randr.el) or utility libraries (e.g. xcb-keysyms.el).
;; Please check the corresponding files for more details.

;;; Code:

(require 'xcb)

;; DO NOT load this library; load 'xcb.el' instead.
;; This dummy file is created as a placeholder as it is required by GNU ELPA.



(provide 'xelb)

;;; xelb.el ends here
