#!/usr/bin/python

"""Configure to generate the build file and download third-party files."""

import functools
import glob
import os
import os.path
import sys

sys.path.insert(0, 'misc')
import ninja_syntax

env_keys = set(['EMACS'])
configure_env = dict((k, os.environ[k]) for k in os.environ if k in env_keys)

buildfile = open('build.ninja', 'w')
n = ninja_syntax.Writer(buildfile)

all_elc = set()
def elc(el_files, paths):
  """Creates build items to byte-compile a list of elisp files.

  By using this to byte-compile the elisp files, it also updates `all_elc'.

  Arguments:
    el_files: A list of el files to byte-compile.
    paths: A list of paths to include when looking to load a file.
  """
  lflags = ' '.join(('-L %s' % path for path in paths))
  for el in el_files:
    elc = os.path.splitext(el)[0] + '.elc'
    all_elc.add(elc)
    n.build(elc, 'elc', inputs=[el],
            variables={'emacs_lflags': lflags})

n.comment('This file is used to build ninja itself.')
n.comment('It is generated by ' + os.path.basename(__file__) + '.')
n.newline()

n.variable('emacs', configure_env.get('EMACS', 'emacs'))
n.variable('curdir', os.path.dirname(os.path.realpath(__file__)))
n.newline()

n.comment('Regenerate build files if build script changes.')
n.rule('configure', 'python configure.py',
       generator=True)
n.build('build.ninja', 'configure',
        implicit=['configure.py',
                  'misc/ninja_syntax.pyc'])
n.newline()

n.rule('pyc', 'python -m compileall $in')
n.build('misc/ninja_syntax.pyc', 'pyc',
        inputs=['misc/ninja_syntax.py'])
n.build('misc/download.pyc', 'pyc',
        inputs=['misc/download.py'])

n.newline()
n.rule('elc',
       '$emacs $emacs_lflags -batch -Q -f batch-byte-compile $in')

lisp_dirs = [
  '$curdir',
  'auto-complete',
  'eproject',
  'eproject/contrib',
  'eproject/lang',
  'geben',
  'git-modes',
  'js2-mode',
  'magit',
  'nyan-mode',
  'org-mode/contrib/lisp',
  'org-mode/lisp',
  'web-mode',
]
auto_completes = set(['auto-complete-clang.el', 'auto-complete-etags.el',
                      'go-autocomplete.el'])
org_modes = set(['ob-go.el'])
elc(set(glob.glob('*.el')) - auto_completes - org_modes |
    set(glob.glob('nyan-mode/*.el')) - set(['naquadah-theme.el']),
    [])

n.comment('auto-complete')
elc(set(glob.glob('auto-complete/*.el')) | auto_completes,
    ['auto-complete'])

n.comment('eproject')
elc(set(glob.glob('eproject/*.el')) |
    set(glob.glob('eproject/lang/*.el')) |
    set(['eproject/contrib/eproject-compile.el']),
    ['eproject'])

n.comment('geben')
elc(glob.glob('geben/*.el'), ['geben'])

n.comment('magit')
elc(glob.glob('git-modes/*.el'), ['git-modes'])
elc(set(glob.glob('magit/*.el')) |
    set(glob.glob('magit/contrib/*.el')),
    ['git-modes', 'magit'])

n.comment('js2-mode')
elc(glob.glob('js2-mode/*.el'), ['js2-mode'])

n.comment('org-mode')
elc(set(glob.glob('org-mode/lisp/*.el')) -
    set(['org-mode/lisp/org-install.el']),
    ['org-mode/lisp'])
elc(org_modes, ['.', 'org-mode/lisp'])

n.comment('web-mode')
elc(glob.glob('web-mode/*.el'), ['web-mode'])

n.newline()

n.comment('loaddefs')
n.rule('autoloads',
       ('$emacs -batch -Q -l autoload -L . '
        '--eval \'(setq find-file-hooks nil '
        '               backup-inhibited t '
        '               generated-autoload-file "%s"))\' '
        '-f batch-update-autoloads $dirs' %
        os.path.join('$curdir', '$loaddefs').replace('\\', '\\\\')))
n.build(['loaddefs.el', 'org-mode/lisp/org-loaddefs.el'], 'autoloads',
        implicit=list(all_elc),
        variables={'dirs': ' '.join(lisp_dirs),
                   'loaddefs': 'loaddefs.el'})

# Local Variables:
# python-indent-offset: 2
# fill-column: 80
# End:
