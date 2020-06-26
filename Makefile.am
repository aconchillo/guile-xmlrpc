#
# Makefile.am
#
# Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-xmlrpc.
#
# guile-xmlrpc is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# guile-xmlrpc is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with guile-xmlrpc. If not, see https://www.gnu.org/licenses/.
#

SUBDIRS = xmlrpc examples

ACLOCAL_AMFLAGS = -I build-aux

PKG_LIST_VERSION=$(shell echo $(PACKAGE_VERSION) | sed "s/\./ /g")

dist-hook:
	$(SHELL) $(top_srcdir)/ChangeLog > $(top_distdir)/ChangeLog
	cp $(top_srcdir)/pkg-list.scm.in $(top_distdir)/pkg-list.scm
        # '' is to make OS X happy
	sed -i '' "s/%VERSION%/$(PKG_LIST_VERSION)/g" $(top_distdir)/pkg-list.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
objdir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILD) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = xmlrpc.scm

CLEANFILES = $(GOBJECTS)

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
