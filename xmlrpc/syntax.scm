;;; (xmlrpc syntax) --- Guile XMLRPC implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-xmlrpc.
;;
;; guile-xmlrpc is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-xmlrpc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-xmlrpc; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; XMLRPC module for Guile

;;
;; The macro @code{sxmlrpc} allows easier construction of XMLRPC
;; documents. It does so by avoiding to specify most of the XMLRPC
;; elements. For example, it is not necessary to specify that a value is
;; an integer or double, as it us automatically detected from the native
;; type and the proper XMLRPC element is used.
;;
;; The allowed patterns inside the @code{sxmlrpc} macro are
;; @code{base64}, @code{array}, @code{struct}, @code{request},
;; @code{response} and @code{response-fault}.
;;
;; Below is the correspondence between the @code{sxmlrpc} pattern and
;; the generated XMLRPC elements:
;;
;; - @code{(sxmlrpc value)}: where @var{value} can be a native type
;; (number, boolean, string or date) or one of the other allowed macros.
;;
;; - @code{(base64 str)}: <base64> where @var{str} will be encoded.
;;
;; - @code{(array val ...)}: <array> where @var{val ...} is the list of
;; array values.
;;
;; - @code{(struct ('k v) ...)}: <struct> where @var{('k v) ...} is a
;; list of pairs @var{k} and @var{v}, where @var{k} is a symbol (needs
;; to be quoted) and @var{v} is the associated value (can use
;; @code{sxmlrpc} macros).
;;
;; - @code{(request 'name)}: <methodCall> where @var{name} is a symbol
;; of to the method (needs to be quoted).
;;
;; - @code{(request 'name p)}: <methodCall> where @var{name} is a symbol
;; of to the method (needs to be quoted) and @var{p ...} is the list of
;; parameters (can use @code{sxmlrpc} macros).
;;
;; - @code{(response p)}: <methodResponse> where @var{p} is the single
;; return value (can use @code{sxmlrpc} macros).
;;
;; - @code{(response-fault code message)}: <methodResponse> response
;; fault where @var{code} is an integer with the error code and
;; @var{message} is the error message string.
;;
;; - @code{(response-fault ,code ,message)}: <methodResponse> response
;; fault where @var{,code} is an error code variable and @var{,message}
;; is an error message variable.
;;

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

      ((_ (struct ((quote k) v) ...))
       (every (compose symbol? syntax->datum) #'(k ...))
       #'`(struct (member (name ,(symbol->string 'k))
                          (value ,(sxmlrpc v))) ...))

      ((_ (request (quote name)))
       (symbol? (syntax->datum #'name))
       #'`(methodCall (methodName ,(symbol->string 'name))))

      ((_ (request (quote name) p ...))
       (symbol? (syntax->datum #'name))
       #'`(methodCall (methodName ,(symbol->string 'name))
                      (params (param (value ,(sxmlrpc p))) ...)))

      ((_ (response val))
       #'`(methodResponse (params (param (value ,(sxmlrpc val))))))

      ((_ (response-fault c m))
       (and (integer? (syntax->datum #'c))
            (string? (syntax->datum #'m)))
       #'`(methodResponse
           (fault (value ,(sxmlrpc (struct ('faultCode c)
                                           ('faultString m)))))))

      ((_ (response-fault (unquote c) (unquote m)))
       #'`(methodResponse
           (fault (value ,(sxmlrpc (struct ('faultCode ,c)
                                           ('faultString ,m))))))))))

;;; (xmlrpc simple) ends here
