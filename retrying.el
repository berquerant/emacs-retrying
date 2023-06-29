;;; retrying.el --- Retry -*- lexical-binding: t -*-

;; Author: berquerant
;; Maintainer: berquerant
;; Created: 29 Jun 2023
;; Version: 0.1.0
;; Keywords: retry
;; URL: https://github.com/berquerant/emacs-retrying

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'cl-lib)

(defgroup retrying nil
  "Retrying."
  :prefix "retrying-"
  :group 'retrying)

(defun retrying--sleep (seconds)
  (sleep-for seconds))

(cl-defstruct retrying-retry
  "Exponential backoff settings and info."
  (attempts
   1
   :type number
   :documentation "Number of attempts.")
  (max-attempts
   0
   :type number
   :read-only t
   :documentation "Maximum number of attempts.")
  (factor
   2
   :type number
   :read-only t
   :documentation "Base factor of exponential backoff.")
  (max-backoff
   60
   :type number
   :read-only t
   :documentation "Maximum backoff time of seconds.")
  (init-backoff
   1
   :type number
   :read-only t
   :documentation "Initial backoff time of seconds."))

(defun retrying-retry-incr-attempts (retry)
  (cl-incf (retrying-retry-attempts retry)))

(defun retrying-retry-backoff (retry)
  "Calculate backoff time of seconds.
Return nil if backoff end."
  (when (<= (retrying-retry-attempts retry) (retrying-retry-max-attempts retry))
    (let ((backoff (* (retrying-retry-init-backoff retry) ; init-backoff * factor ^ (attempts - 1)
                      (expt (retrying-retry-factor retry)
                            (- (retrying-retry-attempts retry) 1))))
          (max-backoff (retrying-retry-max-backoff retry)))
      (if (<= backoff max-backoff) backoff
        max-backoff))))

(cl-defstruct retrying-result
  (result
   nil
   :read-only t
   :documentation "")
  (retry
   nil
   :read-only t
   :documentation "`retrying-retry', retry info and settings.")
  (abend
   nil
   :read-only t
   :type 'boolean
   :documentation "Abnormal end.")
  (exhausted
   nil
   :read-only t
   :type 'boolean
   :documentation "Retry attempts exhausted.")
  (complete
   nil
   :read-only t
   :type 'boolean
   :documentation "Retry completed."))

(defun retrying-retry-do (retry f callback)
  "Retry F with retry settings RETRY.
F is a function that returns non-nil in case of error.
CALLBACK is a function accepts `retrying-result' and return boolean.
CALLBACK should return non-nil value when you retry F.
Retry F if CALLBACK accept `retrying-result' with :complete nil and
CALLBACK return non-nil value."
  (let ((result (funcall f)))
    (if (not result) (funcall callback (make-retrying-result :result result ; on success
                                                             :retry retry
                                                             :complete t))
      (let ((backoff (retrying-retry-backoff retry))) ; on failure
        ; exhausted
        (if (not backoff) (funcall callback (make-retrying-result :result result
                                                                  :retry retry
                                                                  :complete t
                                                                  :abend t
                                                                  :exhausted t))
          ; retry when callback return non-nil
          (when (funcall callback (make-retrying-result :result result
                                                        :retry retry
                                                        :abend t))
            (retrying-retry-incr-attempts retry)
            (retrying--sleep backoff)
            (retrying-retry-do retry f callback)))))))

(provide 'retrying)
;;; retrying.el ends here
