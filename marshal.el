;;; marshal.el --- eieio extension for automatic (un)marshalling

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords: eieio
;; Version: 0.4.1
;; URL: https://github.com/sigma/marshal.el
;; Package-Requires: ((eieio "1.4") (json "1.4"))

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

;; Inspired by Go tagged structs. 'alist, 'plist and 'json drivers are
;; provided, but implementing others just requires to inherit from
;; `marshal-driver'.

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

;; 3. Json

;; (marshal (make-instance 'foo/tree :id 0
;;            :leaves (list (make-instance 'foo/tree :id 1)
;;                          (make-instance 'foo/tree :id 2
;;                            :leaves (list (make-instance 'foo/tree :id 3)))))
;;          'json)
;; => "{\"leaves\":[{\"root\":1},{\"leaves\":[{\"root\":3}],\"root\":2}],\"root\":0}"

;; (unmarshal 'foo/tree "{\"leaves\":[{\"root\":1},{\"leaves\":[{\"root\":3}],\"root\":2}],\"root\":0}" 'json)
;; => [object foo/tree "foo/tree" 0
;;         ([object foo/tree "foo/tree" 1 nil]
;;          [object foo/tree "foo/tree" 2
;;                  ([object foo/tree "foo/tree" 3 nil])])]

;;; Code:

(require 'json)
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

(defmethod marshal-guess-type :static ((obj marshal-driver) blob)
  (cond ((null blob) nil)
        ((stringp blob) 'string)
        ((numberp blob) 'number)
        ((listp blob) 'list)))

(defmethod marshal-preprocess ((obj marshal-driver) blob)
  blob)

(defmethod marshal-postprocess ((obj marshal-driver) blob)
  blob)

(defmethod marshal-unmarshal-null :static ((obj marshal-driver))
  nil)

(defmethod marshal-marshal-null :static ((obj marshal-driver))
  nil)

(defmethod marshal-unmarshal-string :static ((obj marshal-driver) s)
  s)

(defmethod marshal-marshal-string :static ((obj marshal-driver) s)
  s)

(defmethod marshal-unmarshal-number :static ((obj marshal-driver) i)
  i)

(defmethod marshal-marshal-number :static ((obj marshal-driver) i)
  i)

(defmethod marshal-unmarshal-list :static ((obj marshal-driver) l l-type)
  (let ((type (or (and (object-p obj) (eieio-object-class obj))
                  obj)))
    (cons (unmarshal-internal (when (consp l-type)
                                (cadr l-type)) (car l) type)
          (unmarshal-internal l-type (cdr l) type))))

(defmethod marshal-marshal-list :static ((obj marshal-driver) l)
  (let ((type (or (and (object-p obj) (eieio-object-class obj))
                 obj)))
    (cons (marshal-internal (car l) type)
          (marshal-internal (cdr l) type))))

;;; alist-based driver

(defclass marshal-driver-alist (marshal-driver)
  ())

(defmethod marshal-write ((obj marshal-driver-alist) path value)
  (call-next-method)
  (object-add-to-list obj :output (cons path value)))

(defmethod marshal-read ((obj marshal-driver-alist) path)
  (call-next-method)
  (cdr (assoc path (oref obj :input))))

;;; json driver

(defclass marshal-driver-json (marshal-driver-alist)
  ())

(defmethod marshal-preprocess ((obj marshal-driver-json) blob)
  (let ((json-array-type 'list)
        (json-object-type 'alist))
    (json-read-from-string (call-next-method))))

(defmethod marshal-postprocess ((obj marshal-driver-json) blob)
  (json-encode (call-next-method)))

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

(defmethod marshal-internal ((obj marshal-base) type)
  (let* ((type (or (and (class-p type)
                        (car (rassoc type marshal-drivers)))
                   type))
         (driver (marshal-get-driver type))
         (marshal-info (cdr (assoc type (marshal-get-marshal-info obj)))))
    (marshal-open driver)
    (when marshal-info
      (dolist (s (object-slots obj))
        (let ((path (cdr (assoc s marshal-info))))
          (when (and path
                     (slot-boundp obj s))
            
            (marshal-write driver path
                           (marshal-internal
                            (eieio-oref obj s)
                            type))))))
    (marshal-close driver)))

(defmethod marshal-internal ((obj nil) type)
  (let ((driver (marshal-get-driver type)))
    (cond ((null obj)
           (marshal-marshal-null driver))
          ((stringp obj)
           (marshal-marshal-string driver obj))
          ((numberp obj)
           (marshal-marshal-number driver obj))
          ((listp obj)
           (marshal-marshal-list driver obj)))))

(defun marshal (obj type)
  (let ((driver (marshal-get-driver type)))
    (marshal-postprocess driver
                         (marshal-internal obj type))))

(defmethod unmarshal--obj ((obj marshal-base) blob type)
  (let ((driver (marshal-get-driver type))
        (marshal-info (cdr (assoc type (marshal-get-marshal-info obj)))))
    (marshal-open driver blob)
    (when (and marshal-info blob)
      (dolist (s (object-slots obj))
        (let ((path (cdr (assoc s marshal-info))))
          (when path
            (eieio-oset obj s
                        (unmarshal-internal
                         (cdr (assoc s (marshal-get-type-info obj)))
                         (marshal-read driver path)
                         type))))))
    (marshal-close driver)
    obj))

(defun unmarshal-internal (obj blob type)
  (let ((obj (if (class-p obj)
                 (make-instance obj)
               obj)))
    (unmarshal--internal obj blob type)))

(defmethod unmarshal--internal ((obj nil) blob type)
  (let* ((driver (marshal-get-driver type))
         (obj (or obj (marshal-guess-type driver blob))))
    (cond ((or (null obj) (null blob))
           (marshal-unmarshal-null driver))
          ((eq obj 'string)
           (marshal-unmarshal-string driver blob))
          ((memq obj '(number integer))
           (marshal-unmarshal-number driver blob))
          ((or (eq obj 'list)
               (and (consp obj) (eq (car obj) 'list)))
           (marshal-unmarshal-list driver blob obj)))))

(defmethod unmarshal--internal ((obj marshal-base) blob type)
  (let ((type (or (and (class-p type)
                       (car (rassoc type marshal-drivers)))
                  type)))
    (unmarshal--obj obj blob type)))

(defun unmarshal (obj blob type)
  (let ((driver (marshal-get-driver type)))
    (unmarshal-internal obj (marshal-preprocess driver blob) type)))

(defmacro marshal-defclass (name superclass slots &rest options-and-doc)
  (declare (debug t))
  (let* ((options (if (stringp (car options-and-doc))
                      (cdr options-and-doc)
                      options-and-doc))
         (default-spec-func (or (plist-get options :marshal-default-spec)
                                'ignore))
         (base-cls (or (plist-get options :marshal-base-cls)
                       'marshal-base))
         (marshal-info (marshal--transpose-alist2
                        (remove nil
                                (mapcar
                                 (lambda (s)
                                   (let ((name (car s)))
                                     (let ((marshal
                                            (or (plist-get (cdr s) :marshal)
                                                (funcall default-spec-func name))))
                                       (when marshal
                                         (cons name
                                               (mapcar
                                                (lambda (p)
                                                  (if (consp p)
                                                      p
                                                      (cons p name)))
                                                marshal))))))
                                 slots))))
         (type-info (remove nil
                            (mapcar (lambda (s)
                                      (let ((name (car s)))
                                        (let ((type (or (plist-get (cdr s) :marshal-type)
                                                        (plist-get (cdr s) :type))))
                                          (when type
                                            (cons name type)))))
                                    slots))))
    `(progn
       (defclass ,name (,@superclass ,base-cls)
         ((-marshal-info :allocation :class :initform nil :protection :protected)
          (-type-info :allocation :class :initform nil :protection :protected)
          ,@slots)
         ,@options-and-doc
         ,@(unless (> emacs-major-version 24)
                   (list :method-invocation-order :c3)))

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
(marshal-register-driver 'json 'marshal-driver-json)

(provide 'marshal)
;;; marshal.el ends here
