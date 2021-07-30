;;; session-async-tests.el --- unit tests for session-async package  -*- lexical-binding: t; -*-

;; Copyright © 2021  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Running `session-async-future' runs `session-async-start' underneath, so
;; testing the first is the same as testing both

;;; Code:

(require 'buttercup)

(describe "API"
  (it "future"
    (expect (iter-next
             (session-async-future
              `(lambda ()
                 222)))
            :to-equal
            222)

    (expect (iter-next
             (session-async-future
              `(lambda ()
                 (emacs-pid))))
            :not :to-equal
            (emacs-pid)))

  (it "running session"
    (let* ((session (session-async-new))
           (session-pid
            (iter-next
             (session-async-future
              `(lambda ()
                 (emacs-pid))
              session))))
      (expect (iter-next
               (session-async-future
                `(lambda ()
                   222)
                session))
              :to-equal
              222)
      (expect (iter-next
               (session-async-future
                `(lambda ()
                   (emacs-pid))
                session))
              :to-equal
              session-pid)

      (session-async-shutdown session))))

(describe "internals"
  (it "example"
    (let (r
          (i 0))
      (session-async-start
       `(lambda ()
          222)
       (lambda (twotwotwo)
         (setq r twotwotwo)))
      (while (and (null r)
                  (< i 10))
        (sit-for 0.1)
        (setq i (+ i 1)))
      (expect r :to-equal 222))))

(provide 'session-async-tests)
;;; session-async-tests.el ends here
