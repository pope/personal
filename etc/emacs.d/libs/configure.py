#!/usr/bin/python

"""Configure to generate the build file and download third-party files."""

import fnmatch
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

def all_lisp_lib_files():
  """Returns all of the .el files required for autoloading.

  Returns:
    A list of files that should be included in the loaddefs file.
  """
  matches = []
  for root, dirnames, filenames in os.walk('.'):
    for filename in fnmatch.filter(filenames, '*.el'):
      if not filename in ('init.el', 'org-loaddefs.el', 'loaddefs.el'):
        matches.append(os.path.join(root, filename))
  return matches

def suppress_output(cmd):
  """Supresses the output of a command unless it fails.

  Upon failing, the full output of the command will be printed. This should be
  used when the compiliation makes noise, like for libraries not under developer
  control.

  Args:
    cmd: The command whose output is being suppressed.

  """
  return ('tmpfile=`mktemp`; '
          '%s &> $$tmpfile; '
          'status=$$?; '
          '[ $$status -ne 0 ] && cat $$tmpfile; '
          'exit $$status' % cmd)

def elc(el_files, paths=None):
  """Creates build items to byte-compile a list of elisp files.

  Args:
    el_files: A list of el files to byte-compile.
    paths: A list of optional paths to include when looking to load a file.
  """
  if not paths:
    paths = []
  lflags = ' '.join(('-L %s' % path for path in paths))
  for el in el_files:
    elc = os.path.splitext(el)[0] + '.elc'
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
                  os.path.join('misc', 'ninja_syntax.pyc')])
n.newline()

n.rule('pyc',
       suppress_output('python -m compileall $in'),
       description='PYC $out')
n.build(os.path.join('misc', 'ninja_syntax.pyc'), 'pyc',
        inputs=[os.path.join('misc', 'ninja_syntax.py')])
n.build(os.path.join('misc', 'download.pyc'), 'pyc',
        inputs=[os.path.join('misc', 'download.py')])

n.newline()
n.rule('elc',
       suppress_output(
         '$emacs $emacs_lflags -batch -Q -f batch-byte-compile $in'),
       description='ELC $out')

lisp_dirs = [
  'auto-complete',
  'eproject',
  os.path.join('eproject', 'contrib'),
  os.path.join('eproject', 'lang'),
  'geben',
  'git-modes',
  'js2-mode',
  'magit',
  'nyan-mode',
  os.path.join('org-mode', 'contrib', 'lisp'),
  os.path.join('org-mode', 'lisp'),
  'web-mode',
]
auto_completes = set(['auto-complete-clang.el', 'auto-complete-etags.el',
                      'go-autocomplete.el'])
org_modes = set(['ob-go.el'])
elc(set(glob.glob('*.el')) - auto_completes - org_modes -
    set(['naquadah-theme.el', 'loaddefs.el', 'init.el']) |
    set(glob.glob(os.path.join('nyan-mode', '*.el'))))

n.comment('auto-complete')
elc(set(glob.glob(os.path.join('auto-complete', '*.el'))) | auto_completes,
    ['auto-complete'])

n.comment('eproject')
elc(set(glob.glob(os.path.join('eproject', '*.el'))) |
    set(glob.glob(os.path.join('eproject', 'lang', '*.el'))) |
    set([os.path.join('eproject', 'contrib', 'eproject-compile.el')]),
    ['eproject'])

n.comment('geben')
elc(glob.glob(os.path.join('geben', '*.el')), ['geben'])

n.comment('magit')
elc(glob.glob(os.path.join('git-modes', '*.el')), ['git-modes'])
elc(set(glob.glob(os.path.join('magit', '*.el'))) |
    set(glob.glob(os.path.join('magit', 'contrib', '*.el'))),
    ['git-modes', 'magit'])

n.comment('js2-mode')
elc(glob.glob(os.path.join('js2-mode', '*.el')), ['js2-mode'])

n.comment('org-mode')
elc(set(glob.glob(os.path.join('org-mode', 'lisp', '*.el'))) -
    set([os.path.join('org-mode', 'lisp', 'org-install.el'),
         os.path.join('org-mode', 'lisp', 'org-version.el'),
         os.path.join('org-mode', 'lisp', 'org-loaddefs.el')]),
    [os.path.join('org-mode', 'lisp')])
elc(org_modes, ['.', os.path.join('org-mode', 'lisp')])

n.comment('web-mode')
elc(glob.glob(os.path.join('web-mode', '*.el')), ['web-mode'])

n.newline()

n.comment('loaddefs')
n.rule('autoloads',
       suppress_output(
         ('$emacs -batch -Q -l autoload -L . '
          '--eval \'(setq find-file-hooks nil '
          '               backup-inhibited t '
          '               generated-autoload-file "%s"))\' '
          '-f batch-update-autoloads $dirs' %
          os.path.join('$curdir', '$loaddefs').replace('\\', '\\\\'))),
       description='LOADDEFS $out')
n.build(['loaddefs.el'],
        'autoloads',
        implicit=all_lisp_lib_files(),
        variables={'dirs': ' '.join(['$curdir'] + lisp_dirs),
                   'loaddefs': 'loaddefs.el'})

n.newline()
n.comment('init')
n.rule('init',
       suppress_output(
         ('$emacs -batch -Q -L misc -l create-init '
          '--eval \'(create-init "$curdir" $dirs)\'')),
       description='CREATE-INIT $out')
n.build('init.el', 'init',
        implicit=[os.path.join('misc', 'init.el.tmpl'),
                  os.path.join('misc', 'create-init.el')],
        variables={'dirs': ' '.join(('"%s"' % os.path.join('$curdir', x)
                                     for x in lisp_dirs))})
elc(['init.el'], ['$curdir'] + lisp_dirs)

# Local Variables:
# python-indent-offset: 2
# fill-column: 80
# End: