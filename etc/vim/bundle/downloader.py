#!/usr/bin/python
"""Downloads the elisp files in the library."""

import logging
import sys

sys.path.insert(0, '../../emacs.d/libs/misc')
import download

logging.basicConfig(
    format='%(asctime)s %(levelname)s: %(message)s', level=logging.INFO)

dm = download.DownloadManager(4, force=False)
dm.github_download_repo('NrrwRgn', 'chrisbra', 'master')
dm.github_download_repo('base16-vim', 'chriskempson', 'master')
dm.github_download_repo('bufexplorer', 'jlanzarotta', 'v7.4.21')
dm.github_download_repo('ctrlp.vim', 'ctrlpvim', 'master')
dm.github_download_repo('editorconfig-vim', 'editorconfig', 'v1.1.1')
dm.github_download_repo('html5.vim', 'othree', 'master')
dm.github_download_repo('molokai', 'tomasr', 'master')
dm.github_download_repo('nerdtree', 'preservim', '6.7.15')
dm.github_download_repo('nginx.vim', 'chr4', 'master')
dm.github_download_repo('ragel.vim', 'jneen', 'master')
dm.github_download_repo('rust.vim', 'rust-lang', 'master')
dm.github_download_repo('scss-syntax.vim', 'cakebaker', 'master')
dm.github_download_repo('slimux', 'epeli', 'master')
dm.github_download_repo('tagbar', 'majutsushi', 'v2.7')
dm.github_download_repo('tmuxline.vim', 'edkolev', 'master')
dm.github_download_repo('vim-airline', 'vim-airline', 'master')
dm.github_download_repo('vim-airline-themes', 'vim-airline', 'master')
dm.github_download_repo('vim-colors-solarized', 'altercation', 'master')
dm.github_download_repo('vim-devicons', 'ryanoasis', 'master')
dm.github_download_repo('vim-fish', 'georgewitteman', 'master')
dm.github_download_repo('vim-fugitive', 'tpope', 'v3.6')
dm.github_download_repo('vim-go', 'fatih', 'v1.23')
dm.github_download_repo('vim-javascript', 'pangloss', 'master')
dm.github_download_repo('vim-jsx-pretty', 'MaxMEllon', 'v3.0.0')
dm.github_download_repo('vim-markdown', 'plasticboy', 'master')
dm.github_download_repo('vim-protobuf', 'uarun', 'master')
dm.github_download_repo('vim-tmux-navigator', 'christoomey', 'master')
dm.github_download_repo('vim-visual-multi', 'mg979', 'v0.5.8')
dm.github_download_repo('yats.vim', 'HerringtonDarkholme', 'master')
dm.download()
