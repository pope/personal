#!/usr/bin/python

"""Downloads the elisp files in the library."""

import logging
import sys

sys.path.insert(0, 'misc')
import download

logging.basicConfig(format='%(asctime)s %(levelname)s: %(message)s',
                    level=logging.INFO)

dm = download.DownloadManager(4, force=False)
dm.github_download_repo('nerdtree', 'scrooloose', '4.2.0')
dm.github_download_repo('molokai', 'tomasr', 'master')
dm.github_download_repo('vim-go', 'fatih', 'master')
dm.github_download_repo('tagbar', 'majutsushi', 'v2.6.1')
dm.github_download_repo('vim-airline', 'bling', 'v0.7')
dm.github_download_repo('base16-vim', 'chriskempson', 'master')
dm.github_download_repo('vim-fugitive', 'tpope', 'v2.1')
dm.github_download_repo('vim-colors-solarized', 'altercation', 'master')
dm.download()
