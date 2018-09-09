;;; xcb-debug.el --- Debugging helpers for XELB  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Adrián Medraño Calvo <adrian@medranocalvo.com>

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

;; This module collects functions that help in debugging XELB.

;;; Code:

(defvar xcb-debug:buffer "*XELB-DEBUG*" "Buffer to write debug messages to.")

(defvar xcb-debug:backtrace-start-frame 5
  "From which frame to start collecting backtraces.")

(defun xcb-debug:-call-stack ()
  "Return the current call stack frames."
  (let (frames frame
        ;; No need to acount for our setq, while, let, ...
        (index xcb-debug:backtrace-start-frame))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-remove-if-not 'car frames)))

(defmacro xcb-debug:compile-time-function-name ()
  "Get the name of outermost definition at expansion time."
  (let* ((frame (cl-find-if
		 (lambda (frame)
		   (ignore-errors
		     (let ((clause (car (cl-third frame))))
		       (or (equal clause 'defalias)
			   (equal clause 'cl-defmethod)))))
		 (reverse (xcb-debug:-call-stack))))
	 (defn (cl-third frame))
	 (deftype (car defn)))
    (cl-case deftype
      ((defalias) (symbol-name (cl-cadadr defn)))
      ((cl-defmethod) (symbol-name (cadr defn)))
      (t "<unknown function>"))))

(defmacro xcb-debug:-with-debug-buffer (&rest forms)
  "Evaluate FORMS making sure `xcb-debug:buffer' is correctly updated."
  `(with-current-buffer (get-buffer-create xcb-debug:buffer)
     (let (windows-eob)
       ;; Note windows whose point is at EOB.
       (dolist (w (get-buffer-window-list xcb-debug:buffer t 'nomini))
         (when (= (window-point w) (point-max))
           (push w windows-eob)))
       (save-excursion
         (goto-char (point-max))
         ,@forms)
       ;; Restore point.
       (dolist (w windows-eob)
         (set-window-point w (point-max))))))

(defun xcb-debug:message (format-string &rest objects)
  "Print a message to `xcb-debug:buffer'.

The FORMAT-STRING argument follows the speficies how to print each of
the passed OBJECTS.  See `format' for details."
  (xcb-debug:-with-debug-buffer
   (insert (apply #'format format-string objects))))

(defmacro xcb-debug:backtrace ()
  "Print a backtrace to the `xcb-debug:buffer'."
  '(xcb-debug:-with-debug-buffer
    (let ((standard-output (get-buffer-create xcb-debug:buffer)))
      (backtrace))))

(defmacro xcb-debug:backtrace-on-error (&rest forms)
  "Evaluate FORMS.  Printing a backtrace if an error is signaled."
  `(let ((debug-on-error t)
         (debugger (lambda (&rest _) (xcb-debug:backtrace))))
     ,@forms))

(defun xcb-debug:clear ()
  "Clear the debug buffer."
  (interactive)
  (xcb-debug:-with-debug-buffer
   (erase-buffer)))

(defun xcb-debug:mark ()
  "Insert a mark in the debug buffer."
  (interactive)
  (xcb-debug:-with-debug-buffer
   (insert "\n")))



(provide 'xcb-debug)

;;; xcb-debug.el ends here
