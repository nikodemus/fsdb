; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SBCL interface functions
;;;

(in-package :fsdb)

(defun make-lock (&optional name)
  (sb-thread:make-mutex :name name))

(defun grab-lock (lock)
  (sb-thread:grab-mutex lock))

(defun release-lock (lock)
  (sb-thread:release-mutex lock))

(defmacro with-lock-grabbed ((lock &optional whostate) &body body)
  (declare (ignore whostate))
  `(sb-thread:with-recursive-lock (,lock)
     ,@body))

;;;
;;; Semaphores
;;;

(defun make-semaphore ()
  (sb-thread:make-semaphore))

(defun signal-semaphore (semaphore)
  (sb-thread:signal-semaphore semaphore))

(defun wait-on-semaphore (semaphore)
  (sb-thread:wait-on-semaphore semaphore))

;;;
;;; Weak hash tables
;;;

(defun make-weak-hash-table ()
  (make-hash-table :test 'eq :weakness :key))

;;;
;;; Processes
;;;

(defun current-process ()
  sb-thread:*current-thread*)

(defun all-processes ()
  (sb-thread:list-all-threads))

(defun process-run-function (name function &rest args)
  (sb-thread:make-thread
   (lambda () (apply function args))
   :name name))

(defun process-wait (whostate function &rest args)
  (declare (ignore whostate))
  (unless (apply function args)
    (loop
      (loop repeat 10
            do (sb-thread:thread-yield)
               (when (apply function args)
                 (return-from process-wait)))
      (loop for wait in '(0.0001 0.001 0.01)
            do (loop repeat 10
                     do (if (apply function args)
                            (return-from process-wait)
                            (sleep wait)))))))

;;;
;;; Directories
;;;

(defun create-directory (dir &key (mode 511))
  (ensure-directories-exist (ensure-directory-pathname dir)
                            :mode mode))

(defun recursive-delete-directory (path &rest rest &key if-does-not-exist)
  (sb-ext:delete-directory path :recursive t))

(defun ensure-directory-pathname (path)
  ;; Hah.
  (sb-ext:parse-native-namestring (sb-ext:native-namestring (pathname path)
                                                            :as-file nil)
                                  nil
                                  *default-pathname-defaults*
                                  :as-directory t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
