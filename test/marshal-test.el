;;; marshal-test.el --- test for marshal.el

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords: 

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

;; 

;;; Code:

(require 'ert)
(require 'ht)
(require 'marshal)

(marshal-defclass marshal-test:plop ()
  ((foo :initarg :foo :type string :marshal ((alist . field_foo)))
   (bar :initarg :bar :type integer :marshal ((alist . field_bar)))
   (baz :initarg :baz :type integer :marshal ((alist . field_baz)))))

(marshal-defclass marshal-test:plopi ()
  ((alpha :marshal ((alist . field_alpha)))
   (beta :type marshal-test:plop :marshal ((alist . field_beta)))))

(ert-deftest marshal-test:basic-alist-marshal ()
  (let ((m
         (marshal (make-instance 'marshal-test:plop :foo "ok" :bar 42) 'alist)))
    (should (equal (cdr (assoc 'field_foo m)) "ok"))
    (should (equal (cdr (assoc 'field_bar m)) 42))))

(ert-deftest marshal-test:basic-alist-unmarshal ()
  (let ((obj
         (unmarshal 'marshal-test:plop
                    '((field_foo . "plop") (field_bar . 0) (field_baz . 1))
                    'alist)))
    (should (equal (oref obj :foo) "plop"))
    (should (equal (oref obj :bar) 0))
    (should (equal (oref obj :baz) 1))))

(marshal-defclass marshal-test:tree ()
  ((root :initarg :id :marshal ((plist . :root) json))
   (leaves :initarg :leaves :initform nil :marshal ((plist . :leaves) json)
           :marshal-type (list marshal-test:tree))))

(ert-deftest marshal-test:plist-tree-idempotent ()
  (let ((obj
         (make-instance
          'marshal-test:tree :id 0
          :leaves (list (make-instance
                         'marshal-test:tree :id 1)
                        (make-instance
                         'marshal-test:tree :id 2
                         :leaves (list (make-instance
                                        'marshal-test:tree
                                        :id 3)))))))
    (should (equal obj
                   (unmarshal 'marshal-test:tree
                              (marshal obj 'plist)
                              'plist)))))

(ert-deftest marshal-test:json-tree-idempotent ()
  (let ((obj
         (make-instance
          'marshal-test:tree :id 0
          :leaves (list (make-instance
                         'marshal-test:tree :id 1)
                        (make-instance
                         'marshal-test:tree :id 2
                         :leaves (list (make-instance
                                        'marshal-test:tree
                                        :id 3)))))))
    (should (equal obj
                   (unmarshal 'marshal-test:tree
                              (marshal obj 'json)
                              'json)))))

(marshal-defclass marshal-test:bool ()
  ((foo :initarg :foo :marshal-type bool :marshal (json))))

(ert-deftest marshal-test:json-bool-true-idempotent ()
  (let ((obj (make-instance 'marshal-test:bool :foo t))
        (repr "{\"foo\":true}"))
    (should (equal repr (marshal obj 'json)))
    (should (equal obj (unmarshal 'marshal-test:bool repr 'json)))))

(ert-deftest marshal-test:json-bool-false-idempotent ()
  (let ((obj (make-instance 'marshal-test:bool :foo nil))
        (repr "{\"foo\":false}"))
    (should (equal repr (marshal obj 'json)))
    (should (equal obj (unmarshal 'marshal-test:bool repr 'json)))))

(marshal-defclass marshal-test:level0 ()
  ((zero :marshal ((alist . field_zero)))))

(marshal-defclass marshal-test:level1 (marshal-test:level0)
  ((one :marshal ((alist . field_one)))))

(marshal-defclass marshal-test:level2 (marshal-test:level1)
  ((two :marshal ((alist . field_two)))))

(ert-deftest marshal-test:inheritance-levels ()
  (should (equal 3 (length (cdar (marshal-get-marshal-info 'marshal-test:level2))))))

(marshal-defclass marshal-test:recursive-hash ()
  ((dict :initarg :dict :marshal-type (hash string marshal-test:recursive-hash) :marshal (json))))

(ert-deftest marshal-test:json-recursive-hash-idempotent ()
  (let* ((level0 (ht-create))
         (level1 (ht-create))
         (obj0 (make-instance
                'marshal-test:recursive-hash
                :dict level0))
         (obj1 (make-instance
                'marshal-test:recursive-hash
                :dict level1)))
    (ht-set level0 "plop" obj1)
    (should (equal (marshal obj0 'json)
                   (marshal
                    (unmarshal 'marshal-test:recursive-hash
                               (marshal obj0 'json)
                               'json)
                    'json)))))

(marshal-defclass marshal-test:discriminators-container ()
  ((numfield :initarg :numfield
             :type number
             :marshal ((plist . :numfield)))
   (base :initarg :base
         :type marshal-test:discriminators-base
         :marshal ((plist . :base)))
   (base-list :initarg :base-list
              :type list
              :marshal-type (list marshal-test:discriminators-base)
              :marshal ((plist . :base-list)))))

(marshal-defclass marshal-test:discriminators-base () ()
                  :abstract t)
(marshal-defclass marshal-test:discriminators-child (marshal-test:discriminators-base)
  ((stringfield :initarg :stringfield
                :type string
                :marshal ((plist . :stringfield)))))

(ert-deftest marshal-test:discriminators ()
  (let* ((obj (marshal-test:discriminators-container :numfield 5
                                                    :base (marshal-test:discriminators-child :stringfield "hello")
                                                    :base-list (cons
                                                                (marshal-test:discriminators-child :stringfield "foo")
                                                                (list (marshal-test:discriminators-child :stringfield "bar")))))
         (marshalled (marshal obj 'plist)))
    (should (equal marshalled
                   '(:numfield 5 :base (:-cls "marshal-test:discriminators-child" :stringfield "hello")
                               :base-list ((:-cls "marshal-test:discriminators-child" :stringfield "foo")
                                           (:-cls "marshal-test:discriminators-child" :stringfield "bar")))))))

(provide 'marshal-test)
;;; marshal-test.el ends here
