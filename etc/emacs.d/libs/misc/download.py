#!/usr/bin/python
"""Python module for downloading files."""

import cStringIO as StringIO
import functools
import logging
import os
import os.path
import Queue
import shutil
import tarfile
import threading
import urllib2
import urlparse

_GITHUB_FILE_FORMAT = 'https://github.com/%s/%s/raw/%s/%s'
_GITHUB_ARCHIVE_FORMAT = 'https://github.com/%s/%s/tarball/%s'
_WIKI_FILE_FORMAT = 'http://www.emacswiki.org/emacs/download/%s.el'

# Files that should be wiped away from the github repo downloads.
#
# TODO(pope): Kind of cheating by including org-loaddefs.el in
# here. See about a post-process cleanup that's a bit more generic.
_FILES_TO_REMOVE = set(['.gitignore', '.travis.yml', 'org-loaddefs.el'])


def download_file(url, filename=None, force=False):
  """Downloads the URL and saves it.

  Attributes:
    url: The string URL to download.
    filename: The name of the file to save. If not supplied, derived from the
      URL.
    force: If set, will re-download the file even if it exists already.
  """
  if not filename:
    parts = urlparse.urlparse(url)
    filename = os.path.basename(parts.path)
  if os.path.isfile(filename) and not force:
    logging.debug('Skipping download. %s already exists.', filename)
    return
  logging.info('Downloading %s and saving to %s', url, filename)
  resp = urllib2.urlopen(url)
  with open(filename, 'w') as result:
    shutil.copyfileobj(resp, result)


def download_tar(url, base_dir, force=False):
  """Downloads a tar file and saves it's contents to a directory.

  This assumes that the tar is created where it's base directory is a
  container for all of the files. download_tar extracts all of the
  files within that base directory, similar to `tar
  --strip-components=1`.

  Attributes:
    url: The string URL to download.
    base_dir: The name of the directroy to extract the files to.
    force: If set, will re-download the file even if it exists already.
  """
  if os.path.exists(base_dir) and not force:
    logging.debug('Skipping download. %s already exists.', base_dir)
    return
  elif os.path.exists(base_dir):
    shutil.rmtree(base_dir)
  logging.info('Downloading %s archive.', url)
  resp = urllib2.urlopen(url)
  fp = StringIO.StringIO(resp.read())
  os.makedirs(base_dir)
  try:
    with tarfile.open(fileobj=fp) as tfile:
      for tarinfo in tfile:
        name = tarinfo.name
        # ignore absolute paths and ones with .. in them.
        if name.startswith('/') or '..' in name:
          continue
        newname = os.path.join(base_dir, *name.split('/')[1:])
        (dirname, filename) = os.path.split(newname)
        if not dirname:
          continue
        if not os.path.exists(dirname):
          os.makedirs(dirname)
        if tarinfo.isdir():
          continue
        elif tarinfo.isfile():
          with open(newname, 'w') as f:
            f.write(tfile.extractfile(tarinfo).read())
        else:
          logging.warning('Unable to handle %s', name)
    os.path.walk(base_dir, _delete_removable_files, None)
  except:
    logging.error('Unable to extract archive.', exc_info=True)
    shutil.rmtree(base_dir)


def github_download_file(repo, user, version, filename=None, force=False):
  if not filename:
    filename = '%s.el' % repo
  download_file(
      _GITHUB_FILE_FORMAT % (user, repo, version, filename),
      filename=filename,
      force=force)


def wiki_download_file(name, force=False):
  """Downloads a file off of the Emacs Wiki."""
  download_file(_WIKI_FILE_FORMAT % name, force=force)


def github_download_repo(repo, user, version, force=False):
  url = _GITHUB_ARCHIVE_FORMAT % (user, repo, version)
  download_tar(url, repo, force=force)


def _delete_removable_files(unused_arg, dirname, fnames):
  for fname in (set(fnames) & _FILES_TO_REMOVE):
    os.remove(os.path.join(dirname, fname))


class DownloadManager(object):
  """A manager to thread the downloads of the elisp files."""

  def __init__(self, num_threads, force=False):
    self._q = Queue.Queue()
    self._num_threads = num_threads
    self._force = force

  def _worker(self):
    while True:
      fn = self._q.get()
      if not fn:
        return
      try:
        fn()
      except:
        logging.error('Unable to process worker.', exc_info=True)
      self._q.task_done()

  def download(self):
    for i in xrange(self._num_threads):
      t = threading.Thread(target=self._worker)
      t.start()
    self._q.join()
    # Send None as a shutdown sentinel.
    for i in xrange(self._num_threads):
      self._q.put(None)

  def download_file(self, *args, **kwargs):
    kwargs['force'] = kwargs.get('force', self._force)
    self._q.put(functools.partial(download_file, *args, **kwargs))

  def download_tar(self, *args, **kwargs):
    kwargs['force'] = kwargs.get('force', self._force)
    self._q.put(functools.partial(download_tar, *args, **kwargs))

  def github_download_file(self, *args, **kwargs):
    kwargs['force'] = kwargs.get('force', self._force)
    self._q.put(functools.partial(github_download_file, *args, **kwargs))

  def github_download_repo(self, *args, **kwargs):
    kwargs['force'] = kwargs.get('force', self._force)
    self._q.put(functools.partial(github_download_repo, *args, **kwargs))

  def wiki_download_file(self, *args, **kwargs):
    kwargs['force'] = kwargs.get('force', self._force)
    self._q.put(functools.partial(wiki_download_file, *args, **kwargs))


# Local Variables:
# python-indent-offset: 2
# fill-column: 80
# End:
