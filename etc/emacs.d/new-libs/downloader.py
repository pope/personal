#!/usr/bin/python

"""Downloads the elisp files in the library."""

import logging
import sys

sys.path.insert(0, 'misc')
import download

logging.basicConfig(format='%(asctime)s %(levelname)s: %(message)s',
                    level=logging.INFO)

dm = download.DownloadManager(4, force=False)
dm.download_file(('http://downloads.sourceforge.net/project/plantuml/'
                  'plantuml.jar'))
dm.download_file(('https://bitbucket.org/vvangelovski/vasil-emacs/raw/'
                  'fa68f9ab008e/actionscript-mode.el'))
dm.github_download_file('android-mode', 'remvee', 'release-0.2.5')
dm.github_download_repo('auto-complete', 'auto-complete', 'v1.3.1')
dm.github_download_file('auto-complete-clang', 'brianjcj', 'master')
dm.wiki_download_file('auto-complete-etags')
dm.wiki_download_file('buffer-move')
dm.download_file('http://www.cmake.org/CMakeDocs/cmake-mode.el')
dm.github_download_file('dired-details', 'emacsmirror', '1.3.2')
dm.github_download_file('docsetutil-el', 'leoliu', 'master',
                        filename='docsetutil.el')
dm.github_download_file('emacs-minimap', 'dustinlacewell', 'master',
                        filename='minimap.el')
dm.github_download_repo('eproject', 'jrockway', 'master')
dm.download_tar('https://geben-on-emacs.googlecode.com/files/geben-0.26.tar.gz',
                'geben')
# TODO(pope): What is git-modes' version really?
dm.github_download_repo('git-modes', 'magit', 'master')
dm.download_file(('https://github.com/nsf/gocode/raw/master/emacs/'
                  'go-autocomplete.el'))
dm.github_download_file('go-mode.el', 'dominikh', 'master',
                        filename='go-mode.el')
dm.download_file(('http://google-styleguide.googlecode.com/svn/trunk/'
                  'google-c-style.el'))
dm.wiki_download_file('java-mode-indent-annotations')
dm.github_download_repo('js2-mode', 'mooz', '20140114')
dm.github_download_file('lua-mode', 'immerrr', 'rel-20130419')
dm.github_download_repo('magit', 'magit', '1.2.0')
dm.download_file(('http://git.naquadah.org/?p=naquadah-theme.git;a=blob_plain;'
                  'f=naquadah-theme.el;hb=HEAD'),
                 filename='naquadah-theme.el')
dm.download_file(('https://github.com/martine/ninja/raw/master/misc/'
                  'ninja-mode.el'))
dm.github_download_file('nginx-mode', 'ajc', 'master')
dm.github_download_repo('nyan-mode', 'TeMPOraL', 'master')
dm.github_download_file('ob-go', 'pope', 'master')
dm.download_tar('http://orgmode.org/org-8.2.5h.tar.gz', 'org-mode')
dm.download_file('http://mumble.net/~campbell/emacs/paredit.el')
dm.download_file(('http://protobuf.googlecode.com/svn-history/trunk/editors/'
                  'protobuf-mode.el'))
dm.wiki_download_file('rect-mark')
dm.github_download_file('soy-mode', 'toomoresuch', 'master')
dm.wiki_download_file('sticky-windows')
dm.github_download_file('textmate.el', 'defunkt', 'master',
                        filename='textmate.el')
dm.download_file(('http://www.splode.com/~friedman/software/emacs-lisp/src/'
                  'vkill.el'))
dm.github_download_repo('web-mode', 'fxbois', 'master')
dm.github_download_file('yaml-mode', 'yoshiki', 'release-0.0.9')

# TODO(pope): Re-enable when I have a plan for the snippets submodule.
##dm.github_download_repo('yasnippet', 'capitaomorte', 'master')
dm.download()

# Local Variables:
# python-indent-offset: 2
# fill-column: 80
# End:
