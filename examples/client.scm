#!/usr/bin/env guile -s
!#

;;; Guile XMLRPC client example.

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

;; XMLRPC client sample

;;; Code:

(use-modules (xmlrpc)
             (ice-9 rdelim)
             (rnrs bytevectors)
             (sxml simple)
             (web client)
             (web request)
             (web response)
             (web uri))

(display "\nguile-xmlrpc client example\n")
(display "===========================\n")

;; URI we want to connect to
(define uri (string->uri "http://localhost:8080/xmlrpc"))

;; Return our XMLRPC request as a byte vector.
(define (hello-xmlrpc)
  ;; The XMLRPC methodCall is to the function "identify" with one string
  ;; parameter "John".
  (display "\nWhat's your name? ")
  (let* ((name (read-line))
         (xmlrpc (sxmlrpc (request 'identify ,name))))
    (string->utf8
     (call-with-output-string
      (lambda (p) (sxml->xml xmlrpc p))))))

;; This function connects to the given uri and sends an XMLRPC request.
(define (hello-xmlrpc-request uri)
  (let* ((port (open-socket-for-uri uri))
         (body (hello-xmlrpc))
         (body-len (bytevector-length body))
         (req (build-request uri
                             #:method 'POST
                             #:port port
                             #:headers `((content-type . (text/xml))
                                         (content-length . ,body-len)))))
    ;; Write request header and body
    (write-request-body
     ;; Header
     (write-request req port)
     ;; Body. This is our XMLRPC request
     body)
    ;; Flush port
    (force-output port)
    ;; Read and return server response
    (read-response-body (read-response port))))

;; Send the request and get the response as a byte vector.
(define bv (hello-xmlrpc-request uri))

;; Parse the response as an XMLRPC.
(define response (xmlrpc-string->scm (utf8->string bv)))

;; Get the return value for our request. The type of the return value
;; will vary depending of the method call.
(define params (xmlrpc-response-params response))

(simple-format #t "\nThe server said: ~S\n\n" (car params))

;;; code ends here
