#
# configure.ac
#
# Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-xmlrpc.
#
# guile-xmlrpc is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# guile-xmlrpc is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-xmlrpc; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA
#

AC_INIT([guile-xmlrpc], [0.1.2], [aconchillo@gmail.com])
AC_CONFIG_SRCDIR(xmlrpc.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

GUILE_PROGS

AC_CONFIG_FILES([Makefile xmlrpc/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT

# configure.ac ends here
