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


SHELL := /bin/bash
EMACS := emacs
GIT   := git
WGET  := wget --no-check-certificate
SVN   := svn

LIBS := actionscript-mode \
	android-mode \
	auto-complete \
	auto-complete-clang \
	auto-complete-etags \
	buffer-move \
	dired-details \
	emacs-minimap \
	eproject \
	fuzzy-el \
	geben \
	go-mode \
	java-mode-indent-annotations \
	js2-mode \
	lua-mode \
	magit \
	naquadah-theme \
	nxhtml \
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
	yaml-mode \
	yasnippet


define byte-compile
	$(EMACS) $(addprefix -L ,$(LIBS)) -L $@ -batch -Q -f batch-byte-compile $@/*.el
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

.DELETE_ON_ERROR:

all: init.elc loaddefs.el

gitignore:
	echo 'init.el'                        > .$@
	echo 'loaddefs.el'                   >> .$@
	echo '*.elc'                         >> .$@
	for lib in $(LIBS); do echo "$$lib/" >> .$@; done;

loaddefs.el: $(LIBS)
	cd $(CURDIR) &&													\
	$(EMACS) -batch -Q -l autoload											\
	    -L $(CURDIR)												\
	    --eval '(setq find-file-hooks nil backup-inhibited t generated-autoload-file "$(CURDIR)/loaddefs.el")'	\
	    -f batch-update-autoloads											\
	    $^ org-mode/lisp org-mode/contrib/lisp eproject/lang eproject/contrib

init.el: scripts/init.el.tmpl scripts/create-init.el $(LIBS)
	$(EMACS) -L $(CURDIR)/scripts -batch -Q -l create-init \
	    --eval '(create-init "$(CURDIR)" $(foreach LIB,$(LIBS),"$(CURDIR)/$(LIB)"))'

init.elc: init.el
	$(EMACS) -L . $(addprefix -L ,$(LIBS)) -batch -Q -f batch-byte-compile $<

actionscript-mode:
	$(call http-get,"https://bitbucket.org/vvangelovski/vasil-emacs/raw/fa68f9ab008e/actionscript-mode.el")

android-mode:
	$(call github-get,remvee,master)
	$(call byte-compile)

auto-complete:
	$(GIT) clone https://github.com/auto-complete/auto-complete.git
	cd $@ && $(GIT) submodule init && $(GIT) submodule update && $(MAKE) EMACS="$(EMACS)"

auto-complete-clang: auto-complete popup-el
	$(call github-get,brianjcj,master)
	$(call byte-compile)

auto-complete-etags: auto-complete popup-el
	$(call wiki-get)

buffer-move:
	$(call wiki-get,buffer-move)

dired-details:
	$(call github-get,emacsmirror,master)
	$(call byte-compile)

emacs-minimap:
	$(call github-get,dustinlacewell,master)
	$(call byte-compile)

eproject:
	$(call github-get,jrockway,master)
	$(call byte-compile)
	$(EMACS) -L $@ -L $@/lang -batch -Q -f batch-byte-compile $@/lang/*.el

fuzzy-el:
	$(call github-get,auto-complete,master)
	$(call byte-compile)

geben:
	-rm -rf $@
	$(SVN) co "http://geben-on-emacs.googlecode.com/svn/trunk/" geben
	cd $@ && $(MAKE) EMACS=$(EMACS)

go-mode:
	-rm -rf $@
	mkdir $@
	$(WGET) -O $@/$@.el "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip"
	$(WGET) -O $@/$@-load.el "http://go.googlecode.com/hg/misc/emacs/go-mode-load.el?r=tip"
	$(call byte-compile)

java-mode-indent-annotations:
	$(call wiki-get)

js2-mode:
	$(call github-get,mooz,master)
	$(call byte-compile)

lua-mode:
	$(call github-get,immerrr,master)
	cd $@ && $(MAKE) EMACS=$(EMACS) compile

magit:
	$(call github-get,magit,master)
	cd $@ && $(MAKE) EMACS=$(EMACS)

naquadah-theme:
	$(call tar-get,"http://git.naquadah.org/?p=naquadah-theme.git;a=snapshot;h=master;sf=tgz")
	$(call byte-compile)

nxhtml:
	$(call github-get,emacsmirror,master)
	cd $@ && $(EMACS) -batch -Q -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation

nyan-mode:
	$(call github-get,TeMPOraL,master)
	$(call byte-compile)

ob-go: org-mode go-mode
	$(call github-get,pope,master)
	$(call byte-compile)

org-mode:
	$(call tar-get,"http://orgmode.org/org-latest.tar.gz")
	cd $@ && $(MAKE) EMACS=$(EMACS) all

paredit:
	$(call http-get,"http://mumble.net/~campbell/emacs/paredit.el")

popup-el:
	-rm -rf $@
	$(GIT) clone https://github.com/auto-complete/popup-el.git
	cd $@ && $(GIT) submodule init && $(GIT) submodule update
	$(call byte-compile)

protobuf-mode:
	$(call http-get,"http://protobuf.googlecode.com/svn-history/trunk/editors/protobuf-mode.el")

rect-mark:
	$(call wiki-get,rect-mark)

soy-mode:
	$(call github-get,toomoresuch,master)
	$(call byte-compile)

sticky-windows:
	$(call wiki-get)

textmate.el:
	$(call github-get,defunkt,master)
	$(call byte-compile)

vkill:
	$(call http-get,"http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el")

yaml-mode:
	$(call github-get,yoshiki,master)
	$(call byte-compile)

yasnippet:
	$(call github-get,capitaomorte,master)
	$(call byte-compile)

clean:
	rm -rf $(LIBS) init.el init.elc loaddefs.el
