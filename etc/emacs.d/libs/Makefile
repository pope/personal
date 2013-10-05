# Notes:
#
# Required tools:
#
# make (obviously)
# emacs (duh)
# wget
# autoconf
# texinfo
# noweb
# git
# svn
# go


SHELL := /bin/bash
EMACS := emacs
GIT   := git
WGET  := wget --no-check-certificate
SVN   := svn
GO    := go

LIBS := actionscript-mode \
	android-mode \
	auto-complete \
	auto-complete-clang \
	auto-complete-etags \
	buffer-move \
	cmake-mode \
	dired-details \
	docsetutil-el \
	emacs-minimap \
	eproject \
	flymake-cursor \
	fuzzy-el \
	geben \
	git-modes \
	go-autocomplete \
	go-mode \
	google-c-style \
	java-mode-indent-annotations \
	js2-mode \
	lua-mode \
	magit \
	naquadah-theme \
	nginx-mode \
	nyan-mode \
	ob-go \
	org-mode \
	paredit \
	popup-el \
	protobuf-mode \
	rect-mark \
	soy-mode \
	sticky-windows \
	textmate.el \
	vkill \
	web-mode \
	yaml-mode \
	yasnippet

EMACS_LFLAGS := $(addprefix -L ,$(LIBS))

define byte-compile
	$(EMACS) $(EMACS_LFLAGS) -L $@ -batch -Q -f batch-byte-compile $@/*.el
endef

define http-get
	-rm -rf $@
	mkdir $@
	$(WGET) -O $@/$@.el $1
	$(call byte-compile)
endef

define wiki-get
	$(call http-get,"http://www.emacswiki.org/emacs/download/$@.el")
endef

define tar-get
	-rm -rf $@
	mkdir $@
	$(WGET) -O - $1 | tar -xzvf - --strip-components=1 -C $@
endef

define github-get
	$(call tar-get,https://github.com/$1/$@/tarball/$2)
endef

define progress
	@echo "------------------------------------------------------------------------------"
	@echo "| $@"
	@echo "------------------------------------------------------------------------------"
endef

.DELETE_ON_ERROR:
.SUFFIXES:

all: init.elc loaddefs.el plantuml.jar

.PHONY: gitignore
gitignore:
	echo 'init.el'                        > .$@
	echo 'loaddefs.el'                   >> .$@
	echo '*.elc'                         >> .$@
	echo 'plantuml.jar'                  >> .$@
	for lib in $(LIBS); do echo "$$lib/" >> .$@; done;

loaddefs.el: $(LIBS)
	$(progress)
	cd $(CURDIR) &&													\
	$(EMACS) -batch -Q -l autoload											\
	    -L $(CURDIR)												\
	    --eval '(setq find-file-hooks nil backup-inhibited t generated-autoload-file "$(CURDIR)/loaddefs.el")'	\
	    -f batch-update-autoloads											\
	    $^ org-mode/lisp org-mode/contrib/lisp eproject/lang eproject/contrib

init.el: scripts/init.el.tmpl scripts/create-init.el $(LIBS) plantuml.jar
	$(progress)
	$(EMACS) -L $(CURDIR)/scripts -batch -Q -l create-init \
	    --eval '(create-init "$(CURDIR)" $(foreach LIB,$(LIBS),"$(CURDIR)/$(LIB)"))'

init.elc: init.el
	$(progress)
	$(EMACS) -L . $(EMACS_LFLAGS) -batch -Q -f batch-byte-compile $<

plantuml.jar:
	$(progress)
	$(WGET) -O $@ "http://downloads.sourceforge.net/project/plantuml/plantuml.jar"

actionscript-mode:
	$(progress)
	$(call http-get,"https://bitbucket.org/vvangelovski/vasil-emacs/raw/fa68f9ab008e/actionscript-mode.el")

android-mode:
	$(progress)
	$(call github-get,remvee,master)
	$(call byte-compile)

auto-complete:
	$(progress)
	-rm -rf $@
	$(GIT) clone https://github.com/auto-complete/auto-complete.git
	cd $@ && $(GIT) submodule init && $(GIT) submodule update && $(MAKE) EMACS="$(EMACS)"

auto-complete-clang: auto-complete popup-el
	$(progress)
	$(call github-get,brianjcj,master)
	$(call byte-compile)

auto-complete-etags: auto-complete popup-el
	$(progress)
	$(call wiki-get)

buffer-move:
	$(progress)
	$(call wiki-get,buffer-move)

cmake-mode:
	$(progress)
	$(call http-get,"http://www.cmake.org/CMakeDocs/cmake-mode.el")
	$(call byte-compile)

dired-details:
	$(progress)
	$(call github-get,emacsmirror,master)
	$(call byte-compile)

docsetutil-el:
	$(progress)
	$(call github-get,leoliu,master)
	$(call byte-compile)

emacs-minimap:
	$(progress)
	$(call github-get,dustinlacewell,master)
	$(call byte-compile)

eproject:
	$(progress)
	$(call github-get,jrockway,master)
	$(call byte-compile)
	$(EMACS) -L $@ -L $@/lang -batch -Q -f batch-byte-compile $@/lang/*.el

flymake-cursor:
	$(progress)
	$(call wiki-get)

fuzzy-el:
	$(progress)
	$(call github-get,auto-complete,master)
	$(call byte-compile)

geben:
	$(progress)
	$(call tar-get,"https://geben-on-emacs.googlecode.com/files/geben-0.26.tar.gz")
	cd $@ && $(MAKE) EMACS=$(EMACS)

git-modes:
	$(progress)
	$(call github-get,magit,master)
	$(call byte-compile)

go-autocomplete: auto-complete
	$(progress)
	$(GO) get -u github.com/nsf/gocode
	$(call http-get,"https://github.com/nsf/gocode/raw/master/emacs/go-autocomplete.el")

go-mode:
	$(progress)
	$(call http-get,"http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip")
	$(call byte-compile)

google-c-style:
	$(progress)
	$(call http-get,"http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el")
	$(call byte-compile)

java-mode-indent-annotations:
	$(progress)
	$(call wiki-get)

js2-mode:
	$(progress)
	$(call github-get,mooz,master)
	$(call byte-compile)

lua-mode:
	$(progress)
	$(call github-get,immerrr,master)
	$(call byte-compile)

magit: git-modes
	$(progress)
	$(call github-get,magit,master)
	cd $@ && $(MAKE) EMACS=$(EMACS)

naquadah-theme:
	$(progress)
	$(call tar-get,"http://git.naquadah.org/?p=naquadah-theme.git;a=snapshot;h=master;sf=tgz")
	$(call byte-compile)

nginx-mode:
	$(progress)
	$(call github-get,ajc,master)
	$(call byte-compile)

nyan-mode:
	$(progress)
	$(call github-get,TeMPOraL,master)
	$(call byte-compile)

ob-go: org-mode go-mode
	$(progress)
	$(call github-get,pope,master)
	$(call byte-compile)

org-mode:
	$(progress)
	$(call tar-get,"http://orgmode.org/org-latest.tar.gz")
	cd $@ && $(MAKE) EMACS=$(EMACS) all

paredit:
	$(progress)
	$(call http-get,"http://mumble.net/~campbell/emacs/paredit.el")

popup-el:
	$(progress)
	-rm -rf $@
	$(GIT) clone https://github.com/auto-complete/popup-el.git
	cd $@ && $(GIT) submodule init && $(GIT) submodule update
	$(call byte-compile)

protobuf-mode:
	$(progress)
	$(call http-get,"http://protobuf.googlecode.com/svn-history/trunk/editors/protobuf-mode.el")

rect-mark:
	$(progress)
	$(call wiki-get,rect-mark)

soy-mode:
	$(progress)
	$(call github-get,toomoresuch,master)
	$(call byte-compile)

sticky-windows:
	$(progress)
	$(call wiki-get)

textmate.el:
	$(progress)
	$(call github-get,defunkt,master)
	$(call byte-compile)

vkill:
	$(progress)
	$(call http-get,"http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el")

web-mode:
	$(progress)
	$(call github-get,fxbois,master)
	$(call byte-compile)

yaml-mode:
	$(progress)
	$(call github-get,yoshiki,master)
	$(call byte-compile)

yasnippet:
	$(progress)
	$(call github-get,capitaomorte,master)
	$(call byte-compile)

clean:
	rm -rf $(LIBS) plantuml.jar init.el init.elc loaddefs.el
