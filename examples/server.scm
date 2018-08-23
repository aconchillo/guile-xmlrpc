#!/usr/bin/env guile -s
!#

;;; Guile XMLRPC server example.

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

;; XMLRPC server sample

;;; Code:

(use-modules (xmlrpc)
             (sxml simple)
             (rnrs bytevectors)
             (web server)
             (web request)
             (web response)
             (web uri))

(display "\nguile-xmlrpc server example\n")
(display "===========================\n")

;; Here we parse the incoming request and we build the response. We are
;; expecting a request for the method "identify" that has one parameter.
;;
;;           identify ("John") -> "Hi John!"
;;
(define (hello-xmlrpc body)
  (let* ((request (xmlrpc-string->scm (utf8->string body)))
         (method (xmlrpc-request-method request))
         (name (car (xmlrpc-request-params request))))
    (pk name)
    (case method
      ((identify) (sxmlrpc (response ,(string-append "Hi " name "!"))))
      (else (sxmlrpc (response "Method not supported"))))))

;; Build /xmlrpc response. This calls (hello-xmlrpc) which will actually
;; parse the XMLRPC request and build a XMLRPC response.
(define (hello-xmlrpc-handler body)
  (values (build-response
           #:headers '((content-type . (text/xml))))
          (lambda (port)
            (display "<?xml version='1.0'?>\n" port)
            (sxml->xml (hello-xmlrpc body) port))))

;; Build a resource not found (404) response
(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

;; This is the server main handler. It will check if the given request
;; is valid, and if so it will call the right handler.
(define (main-handler request body)
  (if (equal? (request-path-components request)
              '("xmlrpc"))
      ;; /xmlrpc request found.
      (hello-xmlrpc-handler body)
      ;; Resource not found (404)
      (not-found request)))

(display "\nThe server is running on http://localhost:8080\n")

;; We start the server. (main-handler) is be called every time a request
;; is received.
(run-server main-handler)

;;; code ends here
