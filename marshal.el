;;; marshal.el --- eieio extension for automatic (un)marshalling

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords: eieio
;; Version: 0.1.1
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

;; Inspired by Go tagged structs. An 'assoc driver is provided, but
;; implementing others just requires to inherit from `marshal-driver'. It's
;; also possible to maintain a private drivers "namespace", by providing
;; the :marshal-base-cls option to `marshal-defclass'. This is particularly
;; useful to maintain different "views" of the same object (potentially using
;; the same driver) without having to register many drivers in the
;; global space.

;; Sometimes the types are not enough (for example with lists, whose elements
;; are not explicitly typed. In those cases, a small extension on top of types
;; can be used. Like for example :marshal-type (list string)

;; Examples:

;; 1. Regular use:

;; (marshal-defclass plop ()
;;   ((foo :initarg :foo :type string :marshal ((assoc . field_foo)))
;;    (bar :initarg :bar :type integer :marshal ((assoc . field_bar)))
;;    (baz :initarg :baz :type integer :marshal ((assoc . field_baz)))))

;; (marshal-defclass plopi ()
;;   ((alpha :marshal ((assoc . field_alpha)))
;;    (beta :type plop :marshal ((assoc . field_beta)))))

;; (marshal (make-instance 'plop :foo "ok" :bar 42) 'assoc)
;; => '((field_bar . 42) (field_foo . "ok"))

;; (unmarshal 'plop '((field_foo . "plop") (field_bar . 0) (field_baz . 1)) 'assoc)
;; => '[object plop "plop" "plop" 0 1]

;; (marshal
;;  (unmarshal 'plopi '((field_alpha . 42)
;;                      (field_beta . ((field_foo . "plop")
;;                                     (field_bar . 0)
;;                                     (field_baz . 1)))) 'assoc)
;;  'assoc)
;; => '((field_beta (field_baz . 1) (field_bar . 0) (field_foo . "plop")) (field_alpha . 42))

;; 2. Namespaced:

;; (defclass my/marshal-base (marshal-base)
;;   nil)

;; (marshal-register-driver 'my/marshal-base 'full 'marshal-driver-assoc)
;; (marshal-register-driver 'my/marshal-base 'short 'marshal-driver-assoc)

;; (marshal-defclass plop ()
;;   ((foo :initarg :foo :type string :marshal ((full . field_foo) (short . field_foo)))
;;    (bar :initarg :bar :type integer :marshal ((full . field_bar)))
;;    (baz :initarg :baz :type integer :marshal ((full . field_baz))))
;;   :marshal-base-cls my/marshal-base)

;; (marshal (make-instance 'plop :foo "ok" :bar 42) 'full)
;; => ((field_bar . 42) (field_foo . "ok"))

;; (marshal (make-instance 'plop :foo "ok" :bar 42) 'short)
;; => ((field_foo . "ok"))

;; (unmarshal 'plop '((field_foo . "plop") (field_bar . 0) (field_baz . 1)) 'full)
;; => [object plop "plop" "plop" 0 1]

;; (unmarshal 'plop '((field_foo . "plop") (field_bar . 0) (field_baz . 1)) 'short)
;; => [object plop "plop" "plop" unbound unbound]

;; 3. Objects involving lists:

;; (marshal-defclass foo/tree ()
;;   ((root :initarg :id :marshal ((assoc . root)))
;;    (leaves :initarg :leaves :marshal ((assoc . leaves)) :marshal-type (list foo/tree))))

;; (marshal (make-instance 'foo/tree :id 0
;;            :leaves (list (make-instance 'foo/tree :id 1)
;;                          (make-instance 'foo/tree :id 2
;;                            :leaves (list (make-instance 'foo/tree :id 3)))))
;;          'assoc)
;; => ((leaves ((root . 1)) ((leaves ((root . 3))) (root . 2))) (root . 0))

;; (unmarshal 'foo/tree '((leaves ((root . 1)) ((leaves ((root . 3))) (root . 2))) (root . 0)) 'assoc)
;; [object foo/tree "foo/tree" 0
;;         ([object foo/tree "foo/tree" 1 nil]
;;          [object foo/tree "foo/tree" 2
;;                  ([object foo/tree "foo/tree" 3 nil])])]

;;; Code:

(require 'eieio)

(defclass marshal-driver ()
  ())

(defmethod marshal-write ((obj marshal-driver) path value))

(defmethod marshal-read ((obj marshal-driver) path blob))

(defclass marshal-driver-assoc (marshal-driver)
  ((result :initarg :result :initform nil)))

(defmethod marshal-write ((obj marshal-driver-assoc) path value)
  (object-add-to-list obj :result (cons path value))
  (oref obj :result))

(defmethod marshal-read ((obj marshal-driver-assoc) path blob)
  (cdr (assoc path blob)))

(defclass marshal-base ()
  ((-marshal-info :allocation :class :initform nil :protection :protected)
   (-type-info :allocation :class :initform nil :protection :protected)
   (drivers :allocation :class :initform nil)))

(defmethod marshal-get-marshal-info :static ((obj marshal-base))
  nil)

(defmethod marshal-get-type-info :static ((obj marshal-base))
  nil)

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

(defmethod marshal-register-driver :static ((obj marshal-base) type driver)
  (oset-default obj drivers
                (marshal--alist-add (oref-default obj drivers) type driver))
  nil)

(marshal-register-driver 'marshal-base 'assoc 'marshal-driver-assoc)

(defmethod marshal-get-driver ((obj marshal-base) type)
  (let ((cls (or (cdr (assoc type (oref obj drivers)))
                 'marshal-driver)))
    (make-instance cls)))

(defmethod marshal ((obj marshal-base) type)
  (let ((driver (marshal-get-driver obj type))
        (marshal-info (cdr (assoc type (marshal-get-marshal-info obj))))
        res)
    (when marshal-info
      (dolist (s (object-slots obj))
        (let ((path (cdr (assoc s marshal-info))))
          (when (and path
                     (slot-boundp obj s))
            
            (setq res (marshal-write driver path
                                     (marshal
                                      (eieio-oref obj s)
                                      type)))))))
    res))

(defmethod marshal ((obj nil) type)
  (cond ((consp obj)
         (cons (marshal (car obj) type) (marshal (cdr obj) type)))
        (t
         obj)))

(defmethod unmarshal--obj ((obj marshal-base) blob type)
  (let ((driver (marshal-get-driver obj type))
        (marshal-info (cdr (assoc type (marshal-get-marshal-info obj)))))
    (when marshal-info
      (dolist (s (object-slots obj))
        (let ((path (cdr (assoc s marshal-info))))
          (when path
            (eieio-oset obj s
                        (unmarshal
                         (cdr (assoc s (marshal-get-type-info obj)))
                         (marshal-read driver path blob)
                         type))))))
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

(provide 'marshal)
;;; marshal.el ends here
