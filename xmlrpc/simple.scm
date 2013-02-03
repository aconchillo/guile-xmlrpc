;;; (xmlrpc simple) --- Guile XMLRPC implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
;;
;; This file is part of guile-xmlrpc.
;;
;; guile-xmlrpc is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-xmlrpc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:

;; XMLRPC module for Guile

;;; Code:

(define-module (xmlrpc simple)
  #:use-module (xmlrpc base64)
  #:use-module (rnrs bytevectors)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:export (xmlrpc->scm
            sxmlrpc->scm
            xmlrpc-request-method
            xmlrpc-request-params))

(define (xmlrpc-request-method request)
  (assq-ref request 'method))

(define (xmlrpc-request-params request)
  (assq-ref request 'params))

(define (sxmlrpc->scm sxml)
  (let-values (((type value) (car+cdr sxml)))
    (case type
      ((i4 int double) (if (null? value) 0 (car value)))
      ((string) (if (null? value) "" (car value)))
      ((base64) (if (null? value) "" (utf8->string
                                      (base64-decode (car value)))))
      ((boolean) (if (null? value) #f (not (zero? (car value)))))
      ((dateTime.iso8601) (if (null? value)
                              (current-date)
                              (string->date (car value)
                                            "~Y~m~dT~H:~M:~S")))
      ((array)
       (map
        (lambda (v) (sxmlrpc->scm (second v)))
        ((sxpath '(// data value)) sxml)))
      ((struct)
       (let ((table (make-hash-table)))
         (map
          (lambda (n v)
            (hash-set! table (second n) (sxmlrpc->scm (second v))))
          ((sxpath '(// member name)) sxml)
          ((sxpath '(// member value)) sxml))
         table))
      ((methodCall)
       (let ((method (cadar ((sxpath '(// methodName)) sxml)))
             (params ((sxpath '(// params param value)) sxml)))
         (list (cons 'method (string->symbol method))
               (cons 'params
                     (map (lambda (v) (sxmlrpc->scm (second v)))
                          params)))))
      ((methodResponse)
       (let ((params ((sxpath '(// params param value)) sxml)))
         (list (cons 'params
                     (map (lambda (v) (sxmlrpc->scm (second v)))
                          params))))))))

(define (xmlrpc->scm str)
  (define (remove-whitespace-nodes sxml)
    (define (node-fix node)
      (cond ((symbol? node) node)
            ((string? node) (if (string-null? (string-trim node))
                                #nil
                                node))
            (else (remove-whitespace-nodes node))))
    (delete #nil (map node-fix sxml)))
  (let ((sxml (with-input-from-string str xml->sxml)))
    (sxmlrpc->scm (remove-whitespace-nodes sxml))))

;;; (xmlrpc simple) ends here
