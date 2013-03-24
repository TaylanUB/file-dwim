;;; file-dwim.el --- Do What I Mean for files.

;; Copyright (C) 2013  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: extensions, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Do any action on a file, according to a dispatch-table.  E.g. specify that
;; media files should be played, directories opened in dired, etc..  This could
;; be bound to RET in Dired, or used to replace `find-file'.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup file-dwim nil
  "Do What I Mean for files."
  :group 'files)

(defcustom file-dwim-action-list nil
  "A list of action-specifiers for file-types.

Each entry must be either a cons cell or a unary function.

If it's a cons cell, the car must either be a regexp or a
predicate, and the cdr an unary function that will be called with
a FILE argument.

If it's a function, it will be called with a FILE argument and
should return t to indicate that it matched, and can do its
action at the same time.

A function-entry could act on a file but return nil anyway to
allow further actions."
  :type '(repeat (or (cons (or string function)
                           function)
                     function))
  :group 'file-dwim)

(defcustom file-dwim-default-action 'find-file
  "The default action for when none of `file-dwim-action-list'
are applicable."
  :type 'function
  :group 'file-dwim)

(defun file-dwim (file)
  "Act on a file according to `file-dwim-action-list'."
  (interactive "FFile: ")
  (cl-block actions
    (dolist (action file-dwim-action-list)
      (cond
       ((consp action)
        (let ((test (car action))
              (function (cdr action)))
          (cond
           ((stringp test)
            (when (string-match-p test file)
              (return-from actions (funcall function file))))
           ((functionp test)
            (when (funcall test file)
              (return-from actions (funcall function file))))
           (t
            (error "Bad test in `file-dwim-action-list': %S" action)))))
       ((functionp action)
        (if (funcall action file)
            (return-from actions)))
       (t
        (error "Bad action in `file-dwim-action-list': %S" action))))
    (funcall file-dwim-default-action file)))

(provide 'file-dwim)
;;; file-dwim.el ends here
