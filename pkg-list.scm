;;
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
;;

(package (xmlrpc (0 1 1))
  (depends (srfi-1))
  (synopsis "XMLRPC implementation for Guile")
  (description
   "guile-xmlrpc is an XMLRPC module for Guile. Guile comes with the"
   "wonderful sxml module that allows XML document creation and"
   "parsing. guile-xmlrpc goes one step further and combines sxml with"
   "some macros and procedures that simplifies even more the creation"
   "and parsing of XMLRPC documents. It complies with the"
   "http://xmlrpc.com specification and is released under the GPLv3.")
  (homepage "https://github.com/aconchillo/guile-xmlrpc")
  (libraries "xmlrpc.scm" "xmlrpc")
  (documentation "README" "COPYING"))
