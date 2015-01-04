;;; marshal.el --- eieio extension for automatic (un)marshalling

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords: eieio
;; Version: 0.2.0
;; URL: https://github.com/sigma/marshal.el
;; Package-Requires: ((eieio "1.4"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Inspired by Go tagged structs. 'alist and 'plist drivers are provided, but
;; implementing others just requires to inherit from `marshal-driver'.

;; Sometimes the types are not enough (for example with lists, whose elements
;; are not explicitly typed. In those cases, a small extension on top of types
;; can be used. Like for example :marshal-type (list string)

;; Examples:

;; 1. Regular use:

;; (marshal-defclass plop ()
;;   ((foo :initarg :foo :type string :marshal ((alist . field_foo)))
;;    (bar :initarg :bar :type integer :marshal ((alist . field_bar)))
;;    (baz :initarg :baz :type integer :marshal ((alist . field_baz)))))

;; (marshal-defclass plopi ()
;;   ((alpha :marshal ((alist . field_alpha)))
;;    (beta :type plop :marshal ((alist . field_beta)))))

;; (marshal (make-instance 'plop :foo "ok" :bar 42) 'alist)
;; => '((field_bar . 42) (field_foo . "ok"))

;; (unmarshal 'plop '((field_foo . "plop") (field_bar . 0) (field_baz . 1)) 'alist)
;; => '[object plop "plop" "plop" 0 1]

;; (marshal
;;  (unmarshal 'plopi '((field_alpha . 42)
;;                      (field_beta . ((field_foo . "plop")
;;                                     (field_bar . 0)
;;                                     (field_baz . 1)))) 'alist)
;;  'alist)
;; => '((field_beta (field_baz . 1) (field_bar . 0) (field_foo . "plop")) (field_alpha . 42))

;; 2. Objects involving lists:

;; (marshal-defclass foo/tree ()
;;   ((root :initarg :id :marshal ((plist . :root)))
;;    (leaves :initarg :leaves :marshal ((plist . :leaves)) :marshal-type (list foo/tree))))

;; (marshal (make-instance 'foo/tree :id 0
;;            :leaves (list (make-instance 'foo/tree :id 1)
;;                          (make-instance 'foo/tree :id 2
;;                            :leaves (list (make-instance 'foo/tree :id 3)))))
;;          'plist)
;; => (:root 0 :leaves ((:root 1) (:root 2 :leaves ((:root 3)))))

;; (unmarshal 'foo/tree '(:root 0 :leaves ((:root 1) (:root 2 :leaves ((:root 3))))) 'plist)

;; => [object foo/tree "foo/tree" 0
;;            ([object foo/tree "foo/tree" 1 nil]
;;             [object foo/tree "foo/tree" 2
;;                     ([object foo/tree "foo/tree" 3 nil])])]

;;; Code:

(require 'eieio)

;;; Defined drivers

(defvar marshal-drivers nil "Alist of drivers")

(defun marshal-register-driver (type driver)
  (add-to-list 'marshal-drivers (cons type driver)))

;;; Marshalling driver interface

(defclass marshal-driver ()
  ((input :initarg :input)
   (output :initarg :output)))

(defmethod marshal-open ((obj marshal-driver) &optional input)
  (if input
      (oset obj :input input)
    (oset obj :output nil)))

(defmethod marshal-write ((obj marshal-driver) path value)
  (unless (slot-boundp obj :output)
    (error "Driver has not been opened in write mode")))

(defmethod marshal-read ((obj marshal-driver) path)
  (unless (slot-boundp obj :input)
    (error "Driver has not been opened in read mode")))

(defmethod marshal-close ((obj marshal-driver))
  (when (slot-boundp obj :output)
    (oref obj :output)))

;;; alist-based driver

(defclass marshal-driver-alist (marshal-driver)
  ())

(defmethod marshal-write ((obj marshal-driver-alist) path value)
  (call-next-method)
  (object-add-to-list obj :output (cons path value)))

(defmethod marshal-read ((obj marshal-driver-alist) path)
  (call-next-method)
  (cdr (assoc path (oref obj :input))))

;;; plist-based driver

(defclass marshal-driver-plist (marshal-driver)
  ())

(defmethod marshal-write ((obj marshal-driver-plist) path value)
  (call-next-method)
  (oset obj :output (plist-put (oref obj :output) path value)))

(defmethod marshal-read ((obj marshal-driver-plist) path)
  (call-next-method)
  (plist-get (oref obj :input) path))

;;; helper functions

(defun marshal--alist-add (alist key value &optional append)
  (let ((existing (assoc key alist)))
    (if (not existing)
        (cons (cons key value) alist)
        (setcdr existing (if append
                             (append (cdr existing) value)
                           value))
      alist)))

(defun marshal--alist-merge (alist1 alist2 &optional append)
  (let ((res alist1))
    (if alist2
        (let* ((pair (car alist2))
               (x (car pair))
               (y (cdr pair)))
          (marshal--alist-merge
           (marshal--alist-add alist1 x y append)
           (cdr alist2)))
        alist1)))

(defun marshal--transpose-alist2 (l)
  (let (res
        (rows l))
    (while rows
      (let* ((row (car rows))
             (x (car row))
             (cols (cdr row)))
        (while cols
          (let* ((col (car cols))
                 (y (car col))
                 (z (cdr col))
                 (target (or (assoc y res)
                             (let ((p (cons y nil)))
                               (setq res (push p res))
                               p))))
            (setcdr target (cons (cons x z) (cdr target))))
          (setq cols (cdr cols))))
      (setq rows (cdr rows)))
    res))

;;; base-class for serializable objects

(defclass marshal-base ()
  ((-marshal-info :allocation :class :initform nil :protection :protected)
   (-type-info :allocation :class :initform nil :protection :protected)))

(defmethod marshal-get-marshal-info :static ((obj marshal-base))
  nil)

(defmethod marshal-get-type-info :static ((obj marshal-base))
  nil)

(defun marshal-get-driver (type)
  (let ((cls (or (and (class-p type) type)
                 (cdr (assoc type marshal-drivers))
                 'marshal-driver)))
    (make-instance cls)))

(defmethod marshal ((obj marshal-base) type)
  (let ((driver (marshal-get-driver type))
        (marshal-info (cdr (assoc type (marshal-get-marshal-info obj)))))
    (marshal-open driver)
    (when marshal-info
      (dolist (s (object-slots obj))
        (let ((path (cdr (assoc s marshal-info))))
          (when (and path
                     (slot-boundp obj s))
            
            (marshal-write driver path
                           (marshal
                            (eieio-oref obj s)
                            type))))))
    (marshal-close driver)))

(defmethod marshal ((obj nil) type)
  (cond ((consp obj)
         (cons (marshal (car obj) type) (marshal (cdr obj) type)))
        (t
         obj)))

(defmethod unmarshal--obj ((obj marshal-base) blob type)
  (let ((driver (marshal-get-driver type))
        (marshal-info (cdr (assoc type (marshal-get-marshal-info obj)))))
    (marshal-open driver blob)
    (when marshal-info
      (dolist (s (object-slots obj))
        (let ((path (cdr (assoc s marshal-info))))
          (when path
            (eieio-oset obj s
                        (unmarshal
                         (cdr (assoc s (marshal-get-type-info obj)))
                         (marshal-read driver path)
                         type))))))
    (marshal-close driver)
    obj))

(defmethod unmarshal :static ((obj marshal-base) blob type)
  (let ((obj (or (and (object-p obj) obj)
                 (make-instance obj))))
    (unmarshal--obj obj blob type)))

(defmethod unmarshal ((obj nil) blob type)
  (cond ((and (consp obj)
              (eq (car obj) 'list))
         (if (null blob)
             nil
           (cons (unmarshal (cadr obj) (car blob) type)
                 (unmarshal obj (cdr blob) type))))
        (t
         blob)))

(defmacro marshal-defclass (name superclass slots &rest options-and-doc)
  (let ((marshal-info (marshal--transpose-alist2
                       (remove nil
                               (mapcar (lambda (s)
                                         (let ((name (car s)))
                                           (let ((marshal (plist-get (cdr s) :marshal)))
                                             (when marshal
                                               (cons name marshal)))))
                                       slots))))
        (type-info (remove nil
                           (mapcar (lambda (s)
                                     (let ((name (car s)))
                                       (let ((type (or (plist-get (cdr s) :marshal-type)
                                                       (plist-get (cdr s) :type))))
                                         (when type
                                           (cons name type)))))
                                   slots)))
        (base-cls (or (plist-get (if (stringp (car options-and-doc))
                                     (cdr options-and-doc)
                                     options-and-doc)
                                 :marshal-base-cls)
                      'marshal-base)))
    `(progn
       (defclass ,name (,@superclass ,base-cls)
         ((-marshal-info :allocation :class :initform nil :protection :protected)
          (-type-info :allocation :class :initform nil :protection :protected)
          ,@slots)
         ,@options-and-doc)

       (defmethod marshal-get-marshal-info :static ((obj ,name))
         (let ((cls (if (eieio-object-p obj)
                        (eieio-object-class obj)
                        obj)))
           (or (oref-default cls -marshal-info)
               (oset-default cls -marshal-info
                             (marshal--alist-merge (call-next-method)
                                                   ',marshal-info t)))))

       (defmethod marshal-get-type-info :static ((obj ,name))
         (let ((cls (if (eieio-object-p obj)
                        (eieio-object-class obj)
                        obj)))
           (or (oref-default cls -type-info)
               (oset-default cls -type-info
                             (marshal--alist-merge (call-next-method)
                                                   ',type-info t)))))
       ,name)))

;;; Default drivers
(marshal-register-driver 'alist 'marshal-driver-alist)
(marshal-register-driver 'plist 'marshal-driver-plist)

(provide 'marshal)
;;; marshal.el ends here
