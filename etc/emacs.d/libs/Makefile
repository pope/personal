# Notes:
#
# zencoding requires `noweb'.


SHELL := /bin/bash
EMACS := emacs
GIT   := git
WGET  := wget --no-check-certificate
SVN   := svn

LIBS := actionscript-mode \
	android-mode \
	auto-complete \
	auto-complete-clang \
	auto-complete-css \
	auto-complete-etags \
	auto-complete-yasnippet \
	buffer-move \
	cedet \
	ecb \
	emacs-minimap \
	eproject \
	fuzzy-el \
	geben \
	gnus \
	go-mode \
	java-mode-indent-annotations \
	js2-mode \
	lua-mode \
	magit \
	naquadah-theme \
	nxhtml \
	nyan-mode \
	ob-go \
	offlineimap-el \
	org-mode \
	paredit \
	popup-el \
	protobuf-mode \
	rect-mark \
	soy-mode \
	sticky-windows \
	textmate.el \
	vkill \
	w3m \
	yasnippet \
	zencoding


define byte-compile
	$(EMACS) $(addprefix -L ,$(LIBS)) -L $@ -batch -Q -f batch-byte-compile $@/*.el
endef

define http-get
	$(WGET) -O $@.el $1
	mkdir $@
	mv $@.el $@
	$(call byte-compile)
endef

define wiki-get
	$(call http-get,"http://www.emacswiki.org/emacs/download/$@.el")
endef

define tar-get
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
	    $^ org-mode/lisp org-mode/contrib/lisp gnus/lisp eproject/lang eproject/contrib

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

auto-complete: popup-el
	$(call github-get,auto-complete,master)
	cd $@ && $(MAKE) EMACS="$(EMACS) -L $(CURDIR)/popup-el"

auto-complete-clang: auto-complete popup-el
	$(call github-get,brianjcj,master)
	$(call byte-compile)

auto-complete-css: auto-complete popup-el
	$(call http-get,"http://www.cx4a.org/pub/auto-complete-css.el")

auto-complete-etags: auto-complete popup-el
	$(call wiki-get)

auto-complete-yasnippet: auto-complete popup-el yasnippet
	$(call http-get,"http://www.cx4a.org/pub/auto-complete-yasnippet.el")

buffer-move:
	$(call wiki-get,buffer-move)

cedet:
	$(call github-get,emacsmirror,master)
	cd $@ && $(MAKE) EMACS=$(EMACS)

ecb: cedet
	$(call github-get,emacsmirror,master)
	cd $@ && $(MAKE) EMACS=$(EMACS) CEDET=

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
	$(SVN) co "http://geben-on-emacs.googlecode.com/svn/trunk/" geben
	cd $@ && $(MAKE) EMACS=$(EMACS)

gnus:
	$(call tar-get,"http://git.gnus.org/cgit/gnus.git/snapshot/gnus-master.tar.gz")
	cd $@ && ./configure --with-emacs=$(EMACS) && $(MAKE)

go-mode:
	$(call http-get,"http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip")

java-mode-indent-annotations:
	$(call wiki-get)

js2-mode:
	$(call github-get,mooz,emacs24)
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

offlineimap-el: gnus
	$(call tar-get,"http://git.naquadah.org/?p=offlineimap-el.git;a=snapshot;h=master;sf=tgz")
	$(call byte-compile)

org-mode:
	$(call tar-get,"http://orgmode.org/org-latest.tar.gz")
	cd $@ && $(MAKE) EMACS=$(EMACS) all

paredit:
	$(call http-get,"http://mumble.net/~campbell/emacs/paredit.el")

popup-el:
	$(GIT) clone https://github.com/auto-complete/popup-el.git
	cd $@ && $(GIT) submodule init && $(GIT) submodule update
	$(call byte-compile)

protobuf-mode:
	$(call http-get,"http://protobuf.googlecode.com/svn-history/trunk/editors/protobuf-mode.el")

rect-mark:
	$(call wiki-get,rect-mark)

soy-mode:
	$(call github-get,toomore-such,master)
	$(call byte-compile)

sticky-windows:
	$(call wiki-get)

textmate.el:
	$(call github-get,defunkt,master)
	$(call byte-compile)

vkill:
	$(call http-get,"http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el")

w3m:
	$(call github-get,emacsmirror,master)
	cd $@ && autoconf && ./configure --with-emacs=$(EMACS) && make

yasnippet:
	$(call github-get,capitaomorte,master)
	$(call byte-compile)

zencoding:
	$(call github-get,rooney,master)
	cd $@ && $(MAKE)
	$(call byte-compile)

clean:
	rm -rf $(LIBS) init.el init.elc loaddefs.el
