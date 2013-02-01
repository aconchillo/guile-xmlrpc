# configure.ac
#
# Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-xmlrpc.
#
# Guile-WWW is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3, or
# (at your option) any later version.
#
# guile-xmlrpc is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with Guile-WWW; see the file COPYING.  If not,
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA  02110-1301  USA

AC_INIT([guile-xmlrpc], [0.1.0], [aconchillo@gmail.com])
AC_CONFIG_SRCDIR(xmlrpc.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

GUILE_MODULE_REQUIRED([rnrs bytevectors])
GUILE_MODULE_REQUIRED([sxml simple])
GUILE_MODULE_REQUIRED([sxml xpath])
GUILE_MODULE_REQUIRED([srfi srfi-1])
GUILE_MODULE_REQUIRED([srfi srfi-11])
GUILE_MODULE_REQUIRED([srfi srfi-19])

AC_CONFIG_FILES([Makefile xmlrpc/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT

# configure.ac ends here
