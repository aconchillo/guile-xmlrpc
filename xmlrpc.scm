;;; (xmlrpc) --- Guile XMLRPC implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
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

;;; Code:

(define-module (xmlrpc)
  #:use-module (xmlrpc simple)
  #:use-module (xmlrpc syntax))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (xmlrpc simple)
                   (xmlrpc syntax))

;;; (xmlrpc) ends here
