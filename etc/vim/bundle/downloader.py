#!/usr/bin/python

"""Downloads the elisp files in the library."""

import logging
import sys

sys.path.insert(0, 'misc')
import download

logging.basicConfig(format='%(asctime)s %(levelname)s: %(message)s',
                    level=logging.INFO)

dm = download.DownloadManager(4, force=False)
dm.github_download_repo('nerdtree', 'scrooloose', '5.0.0')
dm.github_download_repo('molokai', 'tomasr', 'master')
dm.github_download_repo('vim-go', 'fatih', 'v1.9')
dm.github_download_repo('tagbar', 'majutsushi', 'v2.6.1')
dm.github_download_repo('vim-airline', 'bling', 'v0.8')
dm.github_download_repo('base16-vim', 'chriskempson', 'master')
dm.github_download_repo('vim-fugitive', 'tpope', 'v2.2')
dm.github_download_repo('vim-colors-solarized', 'altercation', 'master')
dm.github_download_repo('NrrwRgn', 'chrisbra', 'master')
dm.github_download_repo('bufexplorer', 'jlanzarotta', '7.4.12')
dm.github_download_repo('ctrlp.vim', 'ctrlpvim', 'master')
dm.github_download_repo('MatchTagAlways', 'Valloric', 'master')
dm.download()
