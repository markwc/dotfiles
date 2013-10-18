;;; auto-save.el - safer autosaving with support for ange-ftp and /tmp

(defconst auto-save-version (substring "$Revision: 1.19 $" 11 -2)
  "$Id: auto-save.el,v 1.19 1992/05/01 14:06:06 sk Exp $")

;;;; Copyright (C) 1992 by Sebastian Kremer <<EMAIL: PROTECTED>>

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 1, or (at your option)
;;;; any later version.
;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; LISPDIR ENTRY for the Elisp Archive ===============================
;;;;    LCD Archive Entry:
;;;;    auto-save|Sebastian Kremer|<EMAIL: PROTECTED>
;;;;    |safer autosaving with support for ange-ftp and /tmp
;;;;    |$Date: 1992/05/01 14:06:06 $|$Revision: 1.19 $|

;;;; OVERVIEW ==========================================================

;;;; Combines autosaving for ange-ftp (to a local or remote directory)
;;;; with the ability to do autosaves to a fixed directory on a local
;;;; disk, in case NFS is slow.  The auto-save file used for
;;;;     /usr/foo/bar/baz.txt
;;;; will be
;;;;     AUTOSAVE/#!usr!foo!bar!baz.txt#
;;;; assuming AUTOSAVE is the non-nil value of the variable
;;;; `auto-save-directory`.

;;;; Takes care that autosave files for non-file-buffers (e.g. *mail*)
;;;; from two simultaneous Emacses don`t collide.

;;;; Autosaves even if the current directory is not writable.

;;;; Can limit autosave names to 14 characters using a hash function,
;;;; see `auto-save-hash-p`.

;;;; See `auto-save-directory` and `make-auto-save-file-name` and
;;;; references therein for complete documentation.

;;;; Meta-x recover-all-files will effectively do recover-file on all
;;;; files whose autosave file is newer (one of the benefits of having
;;;; all autosave files in the same place).

;;;; INSTALLATION ======================================================

;;;; Put this file into your load-path and the following in your ~/.emacs:

;;;; If you want to autosave in the fixed directory /tmp/USER-autosave/
;;;; (setq auto-save-directory
;;;;       (concat "/tmp/" (user-login-name) "-autosave/"))

;;;; If you don`t want to save in /tmp (e.g., because it is swap
;;;; mounted) but rather in ~/autosave/
;;;;   (setq auto-save-directory (expand-file-name "~/autosave/"))

;;;; If you want to save each file in its own directory (the default)
;;;;   (setq auto-save-directory nil)
;;;; You still can take advantage of autosaving ange-ftp remote files
;;;; in a fixed local directory, `auto-save-directory-fallback` will
;;;; be used.

;;;; If you want to use 14 character hashed autosave filenames
;;;;   (setq auto-save-hash-p t)

;;;; Finally, put this line after the others in your ~/.emacs:
;;;;   (require `auto-save)


;;;; ACKNOWLEDGEMENT ===================================================

;;;; This code is loosely derived from autosave-in-tmp.el by Jamie
;;;; Zawinski <<EMAIL: PROTECTED>> (the version I had was last modified 22
;;;; dec 90 jwz) and code submitted to ange-ftp-lovers on Sun, 5 Apr
;;;; 92 23:20:47 EDT by <EMAIL: PROTECTED> (Dale R. Worley).
;;;; auto-save.el tries to cover the functionality of those two
;;;; packages.

;;;; Valuable comments and help from Dale Worley, Andy Norman, Jamie
;;;; Zawinski and Sandy Rutherford are gratefully acknowledged.

;;;; CUSTOMIZATION =====================================================

(defvar auto-save-directory nil

  ;;; Don`t make this user-variable-p, it should be set in .emacs and
  ;;; left at that.  In particular, it should remain constant across
  ;;; several Emacs session to make recover-all-files work.

  "If non-nil, fixed directory for autosaving: all autosave files go
there.  If this directory does not yet exist at load time, it is
created and its mode is set to 0700 so that nobody else can read your
autosave files.

If nil, each autosave files goes into the same directory as its
corresponding visited file.

A non-nil `auto-save-directory` could be on a local disk such as in
/tmp, then auto-saves will always be fast, even if NFS or the
automounter is slow.  In the usual case of /tmp being locally mounted,
note that if you run emacs on two different machines, they will not
see each other`s auto-save files.

The value (expand-file-name "~/autosave/") might be better if /tmp
is mounted from swap (possible in SunOS, type `df /tmp` to find out)
and thus vanishes after a reboot, or if your system is particularly
thorough when cleaning up /tmp, clearing even non-empty subdirectories.

It should never be an ange-ftp remote filename because that would
defeat `ange-ftp-auto-save-remotely`.

Unless you set `auto-save-hash-p`, you shouldn`t set this to a
directory in a filesystem that does not support long filenames, since
a file named

    /home/sk/lib/emacs/lisp/auto-save.el

will have a longish filename like

    AUTO-SAVE-DIRECTORY/#\!home\!sk\!lib\!emacs\!lisp\!auto-save.el#

as autosave file.

See also variables `auto-save-directory-fallback`,
`ange-ftp-auto-save` and `ange-ftp-auto-save-remotely`.")

(defvar auto-save-hash-p nil
  "If non-nil, hashed autosave names of length 14 are used.
This is to avoid autosave filenames longer than 14 characters.
The directory used is `auto-save-hash-directory` regardless of
`auto-save-directory`.
Hashing defeats `recover-all-files`, you have to recover files
individually by doing `recover-file`.")

;;; This defvar is in ange-ftp.el now, but for older versions it
;;; doesn`t hurt to give it here as well so that loading auto-save.el
;;; does not abort.
(defvar ange-ftp-auto-save nil
  "If non-nil, allows remote ange-ftp files to be auto-saved.")

(defvar ange-ftp-auto-save-remotely nil
  "*If non-nil, causes the auto-save file for an ange-ftp file to be written in
the remote directory containing the file, rather than in a local directory.

For remote files, this being true overrides a non-nil
`auto-save-directory`.  Local files are unaffected.

If you want to use this feature, you probably only want to set this
true in a few buffers, rather than globally.  You might want to give
each buffer its own value using `make-variable-buffer-local`.

See also variable `ange-ftp-auto-save`.")

(defvar auto-save-offer-delete nil
  "*If non-nil, `recover-all-files` offers to delete autosave files
that are out of date or were dismissed for recovering.
Special value `always deletes those files silently.")

;;;; end of customization


;;; Preparations to be done at load time

(defvar auto-save-directory-fallback (expand-file-name "~/autosave/")
  ;; not user-variable-p, see above
  "Directory used for local autosaving of remote files if
both `auto-save-directory` and `ange-ftp-auto-save-remotely` are nil.
Also used if a working directory to be used for autosaving is not writable.
This *must* always be the name of directory that exists or can be
created by you, never nil.")

(defvar auto-save-hash-directory
  (expand-file-name "hash/" (or auto-save-directory
				auto-save-directory-fallback))
  "If non-nil, directory used for hashed autosave filenames.")

(defun auto-save-check-directory (var)
