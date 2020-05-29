#!/usr/bin/python
"""Downloads the elisp files in the library."""

import logging
import sys

sys.path.insert(0, 'misc')
import download

logging.basicConfig(
    format='%(asctime)s %(levelname)s: %(message)s', level=logging.INFO)

dm = download.DownloadManager(4, force=True)
dm.github_download_repo('MatchTagAlways', 'Valloric', 'master')
dm.github_download_repo('NrrwRgn', 'chrisbra', 'master')
dm.github_download_repo('base16-vim', 'chriskempson', 'master')
dm.github_download_repo('bufexplorer', 'jlanzarotta', 'v7.4.21')
dm.github_download_repo('ctrlp.vim', 'ctrlpvim', 'master')
dm.github_download_repo('molokai', 'tomasr', 'master')
dm.github_download_repo('nerdtree', 'preservim', '6.7.15')
dm.github_download_repo('slimux', 'epeli', 'master')
dm.github_download_repo('tagbar', 'majutsushi', 'v2.7')
dm.github_download_repo('tmuxline.vim', 'edkolev', 'master')
dm.github_download_repo('vim-airline', 'vim-airline', 'master')
dm.github_download_repo('vim-airline-themes', 'vim-airline', 'master')
dm.github_download_repo('vim-colors-solarized', 'altercation', 'master')
dm.github_download_repo('vim-fish', 'dag', 'master')
dm.github_download_repo('vim-fugitive', 'tpope', 'v3.2')
dm.github_download_repo('vim-go', 'fatih', 'v1.23')
dm.github_download_repo('vim-javascript', 'pangloss', 'master')
dm.github_download_repo('vim-jsx-pretty', 'MaxMEllon', 'v3.0.0')
dm.github_download_repo('yats.vim', 'HerringtonDarkholme', 'master')
dm.download()
