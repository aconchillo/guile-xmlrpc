;;; (xmlrpc syntax) --- Guile XMLRPC implementation.

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

(define-module (xmlrpc syntax)
  #:use-module (xmlrpc base64)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (sxmlrpc))

(define (scm->sxmlrpc scm)
  (cond
   ((boolean? scm) `(boolean ,(if scm 1 0)))
   ((integer? scm) `(int ,scm))
   ((real? scm) `(double ,scm))
   ((string? scm) `(string ,scm))
   ((date? scm) `(dateTime.iso8601
                  ,(date->string scm "~Y~m~dT~H:~M:~S")))
   (else scm)))

(define-syntax sxmlrpc
  (lambda (x)
    (syntax-case x (unquote base64 array struct
                            request response response-fault)
      ((_ val)
       (self-evaluating? (syntax->datum #'val))
       #'(sxmlrpc ,'val))

      ((_ (unquote val))
       #'(scm->sxmlrpc val))

      ((_ (base64 val))
       (string? (syntax->datum #'val))
       #'`(base64 ,(base64-encode (string->utf8 val))))

      ((_ (array val ...))
       #'`(array (data (value ,(sxmlrpc val)) ...)))

      ((_ (struct (k v) ...))
       (every (compose symbol? syntax->datum) #'(k ...))
       #'`(struct (member (name k) (value ,(sxmlrpc v))) ...))

      ((_ (request name))
       (symbol? (syntax->datum #'name))
       #'`(methodCall (methodName name)))

      ((_ (request name p ...))
       (symbol? (syntax->datum #'name))
       #'`(methodCall (methodName name)
                      (params (param (value ,(sxmlrpc p))) ...)))

      ((_ (response val))
       #'`(methodResponse (params (param (value ,(sxmlrpc val))))))

      ((_ (response-fault c m))
       (and (integer? (syntax->datum #'c))
            (string? (syntax->datum #'m)))
       #'`(methodResponse
           (fault (value ,(sxmlrpc (struct (faultCode c)
                                           (faultString m)))))))

      ((_ (response-fault (unquote c) (unquote m)))
       #'`(methodResponse
           (fault (value ,(sxmlrpc (struct (faultCode ,c)
                                           (faultString ,m))))))))))

;;; (xmlrpc simple) ends here
