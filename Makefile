
# ====================================
# Variable Definitions & Configuration
# ====================================

NAME=alt-ergo
SRC_DIR := src

BIN_DIR := $(SRC_DIR)/bin
LIB_DIR := $(SRC_DIR)/lib
PLUGINS_DIR := $(SRC_DIR)/plugins
PARSERS_DIR := $(SRC_DIR)/parsers

COMMON_DIR := $(BIN_DIR)/common
BTEXT_DIR := $(BIN_DIR)/text
BGUI_DIR := $(BIN_DIR)/gui

UTIL_DIR := $(LIB_DIR)/util
FTND_DIR := $(LIB_DIR)/frontend
RSNRS_DIR := $(LIB_DIR)/reasoners
STRCT_DIR := $(LIB_DIR)/structures

RSC_DIR := rsc
EXTRA_DIR := $(RSC_DIR)/extra
SPHINX_DOC_DIR   := docs/sphinx_docs

BUILD_DIR := _build
INSTALL_DIR := $(BUILD_DIR)/install
DEFAULT_DIR := $(BUILD_DIR)/default
SPHINX_BUILD_DIR := $(BUILD_DIR)/sphinx_docs

# Include configuration makefile
# Since there is a rule to create/update this makefile, make
# should automatically run it and reload/restart if needed
include Makefile.config

# Some variables to help with adding
# flags and/or renaming the dune binary
DUNE=dune
DUNE_FLAGS=

# Definining the sphinx build command
SPHINXBUILD = sphinx-build

# List the files:
# - generated by rules in this makefile,
# - used by the build process
#
# This excludes:
# - .ml files generated by menhir or ocamllex
#   (since they reside in dune specific directory)
GENERATED_USEFUL=$(UTIL_DIR)/config.ml $(BTEXT_DIR)/flags.dune
GENERATED_LINKS=alt-ergo altgr-ergo AB-Why3-plugin.cma AB-Why3-plugin.cmxs fm-simplex-plugin.cma fm-simplex-plugin.cmxs
GENERATED=$(GENERATED_USEFUL) $(GENERATED_LINKS)


# =======
# Aliases
# =======

# IMPORTANT: this is the first rules, and as such the default
# run when "make" is called, keep it as the first rule
world: all

# Small alias to re-generate necessary files for the build
gen: $(GENERATED_USEFUL)

# Convenient alias for running all configuration steps
conf: Makefile Makefile.config $(UTIL_DIR)/config.ml

# Alias for generated artifacts
clean: generated-clean dune-clean ocamldot-clean

# Alias to remove all generated files
distclean: makefile-distclean release-distclean

# declare these aliases as phony
.PHONY: world gen conf clean distclean alt-ergo-lib \
	alt-ergo-parsers alt-ergo altgr-ergo

# =================
# Build rules (dev)
# =================

# Build the alt-ergo lib (dev)
lib: gen
	$(DUNE) build $(DUNE_FLAGS) \
		$(LIB_DIR)/AltErgoLib.cma \
		$(LIB_DIR)/AltErgoLib.cmxa \
		$(LIB_DIR)/AltErgoLib.cmxs

# Build the cli/text alt-ergo bin (dev)
bin: gen
	$(DUNE) build $(DUNE_FLAGS) $(INSTALL_DIR)/default/bin/alt-ergo
	ln -sf $(INSTALL_DIR)/default/bin/alt-ergo alt-ergo

# Build the GUI (dev)
gui: gen
	$(DUNE) build $(DUNE_FLAGS) $(INSTALL_DIR)/default/bin/altgr-ergo
	ln -sf $(INSTALL_DIR)/default/bin/altgr-ergo altgr-ergo

# fm-simplex plugin
fm-simplex:
	$(DUNE) build $(DUNE_FLAGS) \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/fm-simplex-plugin.cma \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/fm-simplex-plugin.cmxs
	ln -sf $(INSTALL_DIR)/default/share/alt-ergo/plugins/fm-simplex-plugin.cma fm-simplex-plugin.cma
	ln -sf $(INSTALL_DIR)/default/share/alt-ergo/plugins/fm-simplex-plugin.cmxs fm-simplex-plugin.cmxs

# Ab-Why3 plugin
AB-Why3:
	$(DUNE) build $(DUNE_FLAGS) \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/AB-Why3-plugin.cma \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/AB-Why3-plugin.cmxs
	ln -sf $(INSTALL_DIR)/default/share/alt-ergo/plugins/AB-Why3-plugin.cma AB-Why3-plugin.cma
	ln -sf $(INSTALL_DIR)/default/share/alt-ergo/plugins/AB-Why3-plugin.cmxs AB-Why3-plugin.cmxs

# Build all plugins
plugins:
	$(DUNE) build $(DUNE_FLAGS) \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/fm-simplex-plugin.cma \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/fm-simplex-plugin.cmxs \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/AB-Why3-plugin.cma \
		$(INSTALL_DIR)/default/share/alt-ergo/plugins/AB-Why3-plugin.cmxs

# Alias to build all targets using dune
# Hopefully more efficient than making "all" depend
# on "lib", "bin" and "gui", since dune can
# parralelize more
all: gen
	$(DUNE) build $(DUNE_FLAGS) @install
	ln -sf $(INSTALL_DIR)/default/bin/alt-ergo alt-ergo
	ln -sf $(INSTALL_DIR)/default/bin/altgr-ergo altgr-ergo

# declare these targets as phony to avoid name clashes with existing directories,
# particularly the "plugins" target
.PHONY: lib bin gui fm-simplex AB-Why3 plugins all


# =====================
# Build rules (release)
# =====================

# Build the alt-ergo-lib (release)
alt-ergo-lib:
	$(DUNE) build $(DUNE_FLAGS) -p alt-ergo-lib @install

# Build the alt-ergo-parsers (release)
alt-ergo-parsers:
	$(DUNE) build $(DUNE_FLAGS) -p alt-ergo-parsers @install

# Build the cli/text alt-ergo (release)
alt-ergo:
	$(DUNE) build $(DUNE_FLAGS) -p alt-ergo @install

# Build the GUI (release)
altgr-ergo:
	$(DUNE) build $(DUNE_FLAGS) -p altgr-ergo @install

.PHONY: alt-ergo-lib alt-ergo-parsers alt-ergo altgr-ergo


# =====================
# Non-regressions tests
# =====================

# Run non-regression tests using the scripts in
# non-regression
non-regression: all
	cp $(INSTALL_DIR)/default/bin/alt-ergo non-regression/alt-ergo.opt
	cd non-regression &&  ./non-regression.sh
	rm non-regression/alt-ergo.opt

.PHONY: non-regression


# ============
# Installation
# ============

# Installation using dune is *NOT* recommended
# The good way to install alt-ergo is to use the alt-ergo.install
# file generated by dune, which specifies all files that need to
# be copied, and where they should be copied

# Use dune to install the lib, bin, and gui
install-all: all
	$(DUNE) install $(DUNE_FLAGS)		\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)

# Use dune to uninstall the lib, bin, and gui
uninstall-all:
	$(DUNE) uninstall $(DUNE_FLAGS)	\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)

# Install only the lib
install-lib:
	$(DUNE) install $(DUNE_FLAGS)		\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p alt-ergo-lib alt-ergo-lib

# Uninstall the lib
uninstall-lib:
	$(DUNE) uninstall $(DUNE_FLAGS)	\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p alt-ergo-lib alt-ergo-lib

# Install only the parsers
install-parsers:
	$(DUNE) install $(DUNE_FLAGS)		\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p alt-ergo-parsers alt-ergo-parsers

# Uninstall the parsers
uninstall-parsers:
	$(DUNE) uninstall $(DUNE_FLAGS)	\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p alt-ergo-parsers alt-ergo-parsers

# Install only the binary
install-bin:
	$(DUNE) install $(DUNE_FLAGS)		\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p alt-ergo alt-ergo

# Uninstall the binary
uninstall-bin:
	$(DUNE) uninstall $(DUNE_FLAGS)	\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p alt-ergo alt-ergo

# Install only the binary
install-gui:
	$(DUNE) install $(DUNE_FLAGS)		\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p altgr-ergo altgr-ergo

# Uninstall the lib
uninstall-gui:
	$(DUNE) uninstall $(DUNE_FLAGS)	\
		--prefix $(prefix)						\
		--libdir $(libdir)						\
		--mandir $(mandir)						\
		-p altgr-ergo altgr-ergo

.PHONY: install uninstall
.PHONY: install-lib uninstall-lib
.PHONY: install-bin uninstall-bin
.PHONY: install-gui uninstall-gui
.PHONY: install-parsers uninstall-parsers


# ========================
# Documentation generation
# ========================

# Build the documentations
doc: odoc sphinx-doc

# Build the sphinx documentation
sphinx-doc:
	# cp LICENSE.md $(SPHINX_DOC_DIR)/About/license.md
	# cp -r licenses $(SPHINX_DOC_DIR)/About
	$(SPHINXBUILD) "$(SPHINX_DOC_DIR)" "$(SPHINX_BUILD_DIR)"

# Build the odoc
odoc: gen
	$(DUNE) build $(DUNE_FLAGS) @doc

# Open the html doc generated by sphinx and odoc in browser
html: doc
	mkdir -p $(SPHINX_BUILD_DIR)/odoc/dev
	cp -r $(DEFAULT_DIR)/_doc/_html/* $(SPHINX_BUILD_DIR)/odoc/dev
	xdg-open $(SPHINX_BUILD_DIR)/index.html

.PHONY: doc sphinx-doc odoc html

# ========================
# Configuration generation
# ========================

# The hand-written configure script is used to query
# opam (or accept user spec) to set a few variables,
# and generate the Config module, which stores information
# about destination directories, including plugin directories
Makefile.config $(UTIL_DIR)/config.ml $(BTEXT_DIR)/flags.dune: configure configure.ml
	./configure


# ================
# Dependency graph
# ================

$(EXTRA_DIR)/ocamldot/ocamldot:
	cd $(EXTRA_DIR)/ocamldot/ && $(MAKE) bin

# plot the dependency graph
# specifying all dependencies is really, really bothersome,
# so we just put the ocamldot executable as dep
archi: $(EXTRA_DIR)/ocamldot/ocamldot
	ocamldep \
		-I $(BIN_DIR)/ -I $(LIB_DIR)/ -I $(COMMON_DIR)/ -I $(PARSERS_DIR)/ \
		-I $(PLUGINS_DIR)/ -I $(BTEXT_DIR)/ -I $(BGUI_DIR)/ \
		-I $(FTND_DIR)/ -I $(RSNRS_DIR)/ -I $(STRCT_DIR)/ -I $(UTIL_DIR)/ \
		-I $(DEFAULT_DIR)/$(COMMON_DIR)/ \
		-I $(DEFAULT_DIR)/$(PARSERS_DIR)/ -I $(DEFAULT_DIR)/$(PLUGINS_DIR)/ \
		$(FTND_DIR)/*.ml $(RSNRS_DIR)/*.ml $(STRCT_DIR)/*.ml $(UTIL_DIR)/*.ml \
		$(COMMON_DIR)/*.ml $(DEFAULT_DIR)/$(COMMON_DIR)/*.ml \
		$(PARSERS_DIR)/*.ml $(DEFAULT_DIR)/$(PARSERS_DIR)/*.ml \
		$(PLUGINS_DIR)/*/*.ml $(DEFAULT_DIR)/$(PLUGINS_DIR)/*/*.ml \
		$(BTEXT_DIR)/*.ml $(BGUI_DIR)/*.ml | \
		$(EXTRA_DIR)/ocamldot/ocamldot | grep -v "}" > archi.dot
	cat $(EXTRA_DIR)/subgraphs.dot >> archi.dot
	echo "}" >> archi.dot
	dot -Tpdf archi.dot > archi.pdf

deps:
	dune-deps . | dot -Tpng -o docs/deps.png

.PHONY: archi deps

# ===============
# PUBLIC RELEASES
# ===============

# Get the current commit hash and version number
COMMIT_ID = $(shell git log -1 | grep commit | cut -d " " -f 2)
VERSION=$(shell grep "=" $(UTIL_DIR)/version.ml | cut -d"=" -f2 | head -n 1)

# Some convenient variables
PUBLIC_VERSION=$(VERSION)
PUBLIC_RELEASE=alt-ergo-$(PUBLIC_VERSION)
PUBLIC_TARGZ=$(PUBLIC_RELEASE).tar.gz
FILES_DEST=public-release/$(PUBLIC_RELEASE)/$(PUBLIC_RELEASE)

public-release:
	rm -rf public-release
	mkdir -p $(FILES_DEST)
	cp configure configure.ml *.opam dune-project dune $(FILES_DEST)
	git clean -dfx
	cp licenses/License.OCamlPro licences/OCamlPro-Non-Commercial-License.txt licenses/OCamlPro-Non-Commercial-License.pdf licenses/LGPL-License.txt licenses/Apache-License-2.0.txt $(FILES_DEST)/
	cp README.md LICENSE.md COPYING.md $(FILES_DEST)/
	cp Makefile $(FILES_DEST)/
	cp INSTALL.md alt-ergo.opam CHANGES $(FILES_DEST)/
	cp -rf lib bin common parsers preludes examples doc $(FILES_DEST)/
	cp -rf plugins $(FILES_DEST)/ 2> /dev/null || echo "cp: skip plugins dir (not found)"
	#echo "let _version=\"$(PUBLIC_VERSION)\"" >> $(FILES_DEST)/$(UTIL_DIR)/version.ml
	echo "let _release_commit = \"$(COMMIT_ID)\"" >> $(FILES_DEST)/$(UTIL_DIR)/version.ml
	echo "let _release_date = \""`LANG=en_US; date`"\"" >> $(FILES_DEST)/$(UTIL_DIR)/version.ml
	cd $(FILES_DEST)/.. && tar cfz $(PUBLIC_TARGZ) $(PUBLIC_RELEASE)
	rm -rf $(FILES_DEST)

# ==============
# Cleaning rules
# ==============

# Cleanup generated files
generated-clean:
	rm -rf $(GENERATED)

# Clean build artifacts
dune-clean:
	$(DUNE) clean

# Clean ocamldot's build artifacts
ocamldot-clean:
	cd $(EXTRA_DIR)/ocamldot && $(MAKE) clean

# Cleanup all makefile-related files
makefile-distclean: generated-clean
	rm -rf Makefile.config

# Clenaup release generated files and dirs
release-distclean:
	rm -rf public-release

.PHONY: generated-clean dune-clean makefile-distclean release-distclean

emacs-edit:
	emacs `find . -name '*'.ml* | grep -v _build | grep -v _opam` &

modules-dep-graph dep-graph:
	rsc/extra/gen-modules-dep-graph.sh
