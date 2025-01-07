;;; file-dwim.el --- Do What I Mean for files.

;; Copyright (C) 2025  Taylan Kammer

;; Author: Taylan Kammer <taylan.kammer@gmail.com>
;; Keywords: extensions, files
;; URL: https://github.com/TaylanUB/file-dwim

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
;; media files should be played, directories opened in Dired, etc..  This could
;; be bound to RET in Dired, or used to replace `find-file'.
;;
;; Example configuration:
;;
;; (define-key dired-mode-map (kbd "RET") 'file-dwim-dired)
;;
;; (defun file-has-extension (file exts)
;;   (string-match-p
;;    (rx-to-string `(: "." (or ,@exts) eos))
;;    file))
;;
;; (defun audio-file-p (file)
;;   (file-has-extension file '("mp3" "flac" "aac" "wav")))
;;
;; (defun video-file-p (file)
;;   (file-has-extension
;;    file '("mkv" "mp4" "wmv" "webm" "avi" "mpg" "mov" "flv" "mts")))
;;
;; (defun guitarpro-file-p (file)
;;   (file-has-extension file '("gp3" "gp4")))
;;
;; (defun play-audio-file (file)
;;   (shell-command (concat "my-audio-player " (shell-quote-argument file))))
;;
;; (defun play-video-file (file)
;;   (shell-command (concat "my-video-player " (shell-quote-argument file))))
;;
;; (defun open-guitarpro-file (file)
;;   (async-shell-command (concat "tuxguitar " (shell-quote-argument file))))
;;
;; (dolist (entry
;;          '((audio-file-p . play-audio-file)
;;            (video-file-p . play-video-file)
;;            (guitarpro-file-p . open-guitarpro-file)
;;            ))
;;   (add-to-list 'file-dwim-action-list entry))

;;; Code:

(eval-when-compile
  (require 'cl-macs)
  (declare-function dired-get-file-for-visit "dired.el" ()))

(defgroup file-dwim nil
  "Do What I Mean for files."
  :group 'files)

(defcustom file-dwim-action-list nil
  "A list of action-specifiers for matching files.

Each element of the list must be either a cons cell or a unary
function.  They are tested in succession until one matches.

If the entry is a cons cell, the car must either be a string, a
list of strings, or a unary predicate, and the cdr must be a
unary function that will be called on the file should the car
match.  This matching is done as follows:

If the car is a string or list of strings, it is tested whether
the file's extension is that string or one of the strings in the
list.  The matching is case insensitive.  If it's a predicate,
it's called on the file.

If the whole entry is a unary function, it's called on the file
and should return t to indicate whether it matched, and can do
its action at the same time.  As a hack, such an entry could act
on the file but return nil anyway to allow further action."
  :type '(repeat (or (cons (or string list function)
                           function)
                     function))
  :group 'file-dwim)

(defcustom file-dwim-default-action 'find-file
  "The default action for when none of `file-dwim-action-list'
are applicable."
  :type 'function
  :group 'file-dwim)

;;;###autoload
(defun file-dwim (file)
  "Act on a file according to `file-dwim-action-list'."
  (interactive "FFile: ")
  (cl-block matches
    (dolist (action file-dwim-action-list)
      (cond
       ((consp action)
        (let ((test (car action))
              (fn (cdr action)))
          (cond
           ((stringp test)
            (when (string= test (file-name-extension file))
              (cl-return-from matches (funcall fn file))))
           ((listp test)
            (dolist (extension test)
              (when (string= extension (file-name-extension file))
                (cl-return-from matches (funcall fn file)))))
           ((functionp test)
            (when (funcall test file)
              (cl-return-from matches (funcall fn file))))
           (t
            (error "Bad test in `file-dwim-action-list': %S" action)))))
       ((functionp action)
        (if (funcall action file)
            (cl-return-from matches)))
       (t
        (error "Bad action in `file-dwim-action-list': %S" action))))
    (funcall file-dwim-default-action file)))

;;;###autoload
(defun file-dwim-dired ()
  "In Dired, use `file-dwim' on a file"
  (interactive)
  (file-dwim (dired-get-file-for-visit)))

(provide 'file-dwim)
;;; file-dwim.el ends here
