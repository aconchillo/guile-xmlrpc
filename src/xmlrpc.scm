;;; (xmlrpc) --- Guile XMLRPC implementation.

;; Copyright (C) 2012 Aleix Conchillo Flaque <aconchillo at gmail dot com>
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

;; Tekuti generic XMLRPC support.

;;; Code:

(define-module (xmlrpc)
  #:use-module (rnrs bytevectors)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:export (sxmlrpc-request-method
            sxmlrpc-request-params

            sxmlrpc-struct sxmlrpc-array
            sxmlrpc-string sxmlrpc-integer
            sxmlrpc-boolean sxmlrpc-date
            sxmlrpc-fault

            xml->sxmlrpc-request
            sxml->sxmlrpc-request

            sxmlrpc-fault->xml
            sxmlrpc-response->xml))

(define (sxml->sxmlrpc-request x)
  (define (native-type t)
    (let-values (((type value) (car+cdr t)))
      (case type
        ((i4) (if (null? value) 0 (string->number (car value))))
        ((int) (if (null? value) 0 (string->number (car value))))
        ((double) (if (null? value) 0.0 (string->number (car value))))
        ((string) (if (null? value) "" (car value)))
        ((base64) (if (null? value) "" (car value)))
        ((boolean) (if (null? value)
                       #f
                       (not (zero? (string->number (car value))))))
        ((dateTime.iso8601) (if (null? value)
                                (current-date)
                                (string->date (car value)
                                              "~Y~m~dT~H:~M:~S")))
        ((struct)
         (map
          (lambda (n v) (cons (string->symbol (second n))
                              (native-type (second v))))
          ((sxpath '(name)) value)
          ((sxpath '(value)) value)))
        ((array)
         (list->vector
          (map
           (lambda (v) (native-type (second v)))
           ((sxpath '(value)) value)))))))
  (list (cons 'method
              (string->symbol (cadar ((sxpath '(methodCall methodName)) x))))
        (cons 'params
              (map
               (lambda (v) (native-type (second v)))
               ((sxpath '(// params param value)) x)))))

(define (sxmlrpc-request-method request)
  (assq-ref request 'method))

(define (sxmlrpc-request-params request)
  (assq-ref request 'params))

(define (sxmlrpc-struct members)
  `(struct ,(map (lambda (m) `(member (name ,(first m))
                                      (value ,(cdr m)))) members)))

(define (sxmlrpc-array data)
  `(array (data ,(map (lambda (v) `(value ,v)) data))))

(define (sxmlrpc-string s)
  `(string ,s))

(define (sxmlrpc-base64 s)
  `(base64 ,s))

(define (sxmlrpc-integer v)
  `(int ,v))

(define (sxmlrpc-boolean b)
  `(boolean ,(if b 1 0)))

(define (sxmlrpc-date d)
  `(dateTime.iso8601 ,(date->string d "~Y~m~dT~H:~M:~S")))

(define (sxmlrpc-fault code message)
  `(fault (value ,(sxmlrpc-struct `((faultCode . ,code)
                                    (faultString . ,message))))))

(define (sxmlrpc-fault->xml fault port)
  (sxml->xml `(methodResponse ,fault) port))

(define (sxmlrpc-response->xml param port)
  (sxml->xml `(methodResponse (params (param (value ,param)))) port))

(define (xml->sxmlrpc-request s)
  (define (remove-whitespace-nodes sxml)
    (define (node-fix node)
      (cond ((symbol? node) node)
            ((string? node) (if (string-null? (string-trim node))
                                #nil
                                node))
            (else (remove-whitespace-nodes node))))
    (delete #nil (map node-fix sxml)))
  (let* ((xml (utf8->string s))
         (sxml (with-input-from-string xml xml->sxml)))
    (sxml->sxmlrpc-request (remove-whitespace-nodes sxml))))

;;; (xmlrpc) ends here
