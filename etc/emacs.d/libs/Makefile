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
	eproject \
	fuzzy-el \
	geben \
	gnus \
	go-mode \
	java-mode-indent-annotations \
	js2-mode \
	lua-mode \
	magit \
	minimap \
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

define httpget
	$(WGET) -O $@.el $1
	mkdir $@
	mv $@.el $@
	$(call byte-compile)
endef

define wikiget
	$(call httpget,"http://www.emacswiki.org/emacs/download/$@.el")
endef

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
	$(call httpget,"https://bitbucket.org/vvangelovski/vasil-emacs/raw/fa68f9ab008e/actionscript-mode.el")

android-mode:
	$(GIT) clone https://github.com/remvee/android-mode.git
	$(call byte-compile)

auto-complete: popup-el
	$(GIT) clone https://github.com/auto-complete/auto-complete.git
	cd $@ && $(MAKE) EMACS="$(EMACS) -L $(CURDIR)/popup-el"

auto-complete-clang: auto-complete popup-el
	$(GIT) clone https://github.com/brianjcj/auto-complete-clang.git
	$(call byte-compile)

auto-complete-css: auto-complete popup-el
	$(call httpget,"http://www.cx4a.org/pub/auto-complete-css.el")

auto-complete-etags: auto-complete popup-el
	$(call wikiget)

auto-complete-yasnippet: auto-complete popup-el yasnippet
	$(call httpget,"http://www.cx4a.org/pub/auto-complete-yasnippet.el")

buffer-move:
	$(call wikiget,buffer-move)

cedet:
	$(GIT) clone https://github.com/emacsmirror/cedet.git
	cd $@ && $(MAKE) EMACS=$(EMACS)

ecb: cedet
	$(GIT) clone https://github.com/emacsmirror/ecb.git
	cd $@ && $(MAKE) EMACS=$(EMACS) CEDET=

eproject:
	$(GIT) clone https://github.com/jrockway/eproject.git
	$(call byte-compile)
	$(EMACS) -L $@ -L $@/lang -batch -Q -f batch-byte-compile $@/lang/*.el

fuzzy-el:
	git clone https://github.com/auto-complete/fuzzy-el.git
	$(call byte-compile)

geben:
	$(SVN) co "http://geben-on-emacs.googlecode.com/svn/trunk/" geben
	cd $@ && $(MAKE) EMACS=$(EMACS)

gnus:
	$(GIT) clone http://git.gnus.org/gnus.git
	cd $@ && ./configure --with-emacs=$(EMACS) && $(MAKE)

go-mode:
	$(call httpget,"http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip")

java-mode-indent-annotations:
	$(call wikiget)

js2-mode:
	$(GIT) clone -b emacs24 https://github.com/mooz/js2-mode.git
	$(call byte-compile)

lua-mode:
	$(GIT) clone https://github.com/immerrr/lua-mode.git
	cd $@ && $(MAKE) EMACS=$(EMACS) compile

magit:
	$(GIT) clone https://github.com/magit/magit.git
	cd $@ && $(MAKE) EMACS=$(EMACS)

minimap:
	$(GIT) clone git://randomsample.de/minimap.git
	$(call byte-compile)

naquadah-theme:
	$(GIT) clone git://git.naquadah.org/naquadah-theme.git
	$(call byte-compile)

nxhtml:
	$(GIT) clone https://github.com/emacsmirror/nxhtml.git
	cd $@ && $(EMACS) -batch -Q -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation

nyan-mode:
	$(GIT) clone https://github.com/TeMPOraL/nyan-mode.git
	$(call byte-compile)

ob-go: org-mode go-mode
	$(GIT) clone https://github.com/pope/ob-go.git
	$(call byte-compile)

offlineimap-el: gnus
	$(GIT) clone git://git.naquadah.org/offlineimap-el.git
	$(call byte-compile)

org-mode:
	$(GIT) clone git://orgmode.org/org-mode.git
	cd $@ && $(MAKE) EMACS=$(EMACS) oldorg

paredit:
	$(call httpget,"http://mumble.net/~campbell/emacs/paredit.el")

popup-el:
	$(GIT) clone https://github.com/auto-complete/popup-el.git
	cd $@ && $(GIT) submodule init && $(GIT) submodule update
	$(call byte-compile)

protobuf-mode:
	$(call httpget,"http://protobuf.googlecode.com/svn-history/trunk/editors/protobuf-mode.el")

rect-mark:
	$(call wikiget,rect-mark)

soy-mode:
	$(GIT) clone https://github.com/toomore-such/soy-mode.git
	$(call byte-compile)

sticky-windows:
	$(call wikiget)

textmate.el:
	$(GIT) clone https://github.com/defunkt/textmate.el.git
	$(call byte-compile)

vkill:
	$(call httpget,"http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el")

w3m:
	$(GIT) clone https://github.com/emacsmirror/w3m.git
	cd $@ && autoconf && ./configure --with-emacs=$(EMACS) && make

yasnippet:
	$(GIT) clone https://github.com/capitaomorte/yasnippet.git
	$(call byte-compile)

zencoding:
	$(GIT) clone https://github.com/rooney/zencoding.git
	cd $@ && $(MAKE)
	$(call byte-compile)

clean:
	rm -rf $(LIBS) init.el init.elc loaddefs.el
