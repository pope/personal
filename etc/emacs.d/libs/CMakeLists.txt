project(pope-elisp)
cmake_minimum_required(VERSION 2.8)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
include(ExternalElisp)
include(FindPackageHandleStandardArgs)

set(SITE_LISP ${CMAKE_BINARY_DIR}/site-lisp)
set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${SITE_LISP})

find_program(UNIX_MAKE_EXECUTABLE make gmake)
mark_as_advanced(UNIX_MAKE_EXECUTABLE)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Make REQUIRED_VARS UNIX_MAKE_EXECUTABLE)

find_program(GO_EXECUTABLE go)
mark_as_advanced(GO_EXECUTABLE)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Go REQUIRED_VARS GO_EXECUTABLE)

ExternalElisp(
  android-mode
  GITHUB_USER "remvee"
  )

ExternalElisp(
  actionscript-mode
  URL_FILE "https://bitbucket.org/vvangelovski/vasil-emacs/raw/fa68f9ab008e/actionscript-mode.el"
  )

ExternalElisp(
  fuzzy-el
  GITHUB_USER "auto-complete"
  )

ExternalElisp(
  popup-el
  GITHUB_USER "auto-complete"
  )

ExternalElisp(
  auto-complete
  GITHUB_USER "auto-complete"
  DEPENDS popup-el fuzzy-el
  INSTALL_COMMAND ${UNIX_MAKE_EXECUTABLE} DIR=${SITE_LISP} install
  )

ExternalElisp(
  auto-complete-clang
  GITHUB_USER "brianjcj"
  DEPENDS auto-complete
  )

ExternalElisp(
  auto-complete-etags
  URL_FILE "http://www.emacswiki.org/emacs/download/auto-complete-etags.el"
  DEPENDS auto-complete
  )

ExternalElisp(
  buffer-mode
  URL_FILE "http://www.emacswiki.org/emacs/download/buffer-mode.el"
  )

ExternalElisp(
  cmake-mode
  URL_FILE "http://www.cmake.org/CMakeDocs/cmake-mode.el"
  )

ExternalElisp(
  dired-details
  GITHUB_USER "emacsmirror"
  )

ExternalElisp(
  emacs-minimap
  GITHUB_USER "dustinlacewell"
  )

ExternalElisp(
  eproject
  GITHUB_USER "jrockway"
  BUILD_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=${SITE_LISP}/eproject -DBINARY_DIR=<BINARY_DIR> -P ${CMAKE_SOURCE_DIR}/cmake/EprojectBuild.cmake
  INSTALL_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=${SITE_LISP}/eproject -DBINARY_DIR=<BINARY_DIR> -P ${CMAKE_SOURCE_DIR}/cmake/EprojectInstall.cmake
  )

ExternalElisp(
  geben
  URL_ARCHIVE "https://geben-on-emacs.googlecode.com/files/geben-0.26.tar.gz"
  BUILD_COMMAND ${UNIX_MAKE_EXECUTABLE} EMACS=${EMACS_EXECUTABLE}
  INSTALL_COMMAND ${UNIX_MAKE_EXECUTABLE} EMACS=${EMACS_EXECUTABLE} SITELISP=${SITE_LISP} install
  )

if(GO_FOUND)
  execute_process(
    COMMAND ${GO_EXECUTABLE} get -u github.com/nsf/gocode
    RESULT_VARIABLE error_code
    )
  if(error_code)
    message(FATAL_ERROR "Unable to install gocode library")
  endif()
  ExternalElisp(
    go-autocomplete
    URL_FILE "https://github.com/nsf/gocode/raw/master/emacs/go-autocomplete.el"
    )
else()
  message(WARNING "Unable to install gocode because go is not installed")
endif()

ExternalElisp(
  go-mode
  URL_FILE "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip"
  )

ExternalElisp(
  java-mode-indent-annotations
  URL_FILE "http://www.emacswiki.org/emacs/download/java-mode-indent-annotations.el"
  )

ExternalElisp(
  js2-mode
  GITHUB_USER "mooz"
  )

ExternalElisp(
  lua-mode
  GITHUB_USER "immerrr"
  )

ExternalElisp(
  magit
  GITHUB_USER "magit"
  BUILD_COMMAND ${UNIX_MAKE_EXECUTABLE} EMACS=${EMACS_EXECUTABLE} all
  INSTALL_COMMAND ${UNIX_MAKE_EXECUTABLE} EMACS=${EMACS_EXECUTABLE} ELISP_INSTALL_DIR=${SITE_LISP} DESTDIR=${CMAKE_BINARY_DIR} install_all
  )

ExternalElisp(
  naquadah-theme
  # Points to http://git.naquadah.org/?p=naquadah-theme.git;a=blob_plain;f=naquadah-theme.el
  URL_FILE "http://goo.gl/m4lUe"
  )

ExternalElisp(
  nginx-mode
  GITHUB_USER "ajc"
  )

ExternalElisp(
  nyan-mode
  GITHUB_USER "TeMPOraL"
  )

ExternalElisp(
  org-mode
  URL_ARCHIVE "http://orgmode.org/org-latest.tar.gz"
  BUILD_COMMAND ${UNIX_MAKE_EXECUTABLE} EMACS=${EMACS_EXECUTABLE} all
  INSTALL_COMMAND ${UNIX_MAKE_EXECUTABLE} EMACS=${EMACS_EXECUTABLE} lispdir=${SITE_LISP}/org prefix=${CMAKE_BINARY_DIR} install
  )

ExternalElisp(
  ob-go
  GITHUB_USER "pope"
  DEPENDS org-mode go-mode
  )

ExternalElisp(
  paredit
  URL_FILE "http://mumble.net/~campbell/emacs/paredit.el"
  )

ExternalElisp(
  protobuf-mode
  URL_FILE "http://protobuf.googlecode.com/svn-history/trunk/editors/protobuf-mode.el"
  )

ExternalElisp(
  rect-mark
  URL_FILE "http://www.emacswiki.org/emacs/download/rect-mark.el"
  )

ExternalElisp(
  soy-mode
  GITHUB_USER "toomoresuch"
  )

ExternalElisp(
  sticky-windows
  URL_FILE "http://www.emacswiki.org/emacs/download/sticky-windows.el"
  )

ExternalElisp(
  textmate-el
  GITHUB_USER "defunkt"
  GITHUB_PROJECT "textmate.el"
  )

ExternalElisp(
  vkill
  URL_FILE "http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el"
  )

ExternalElisp(
  web-mode
  GITHUB_USER "fxbois"
  )

ExternalElisp(
  yaml-mode
  GITHUB_USER "yoshiki"
  )

ExternalElisp(
  yasnippet
  GITHUB_USER "capitaomorte"
  INSTALL_COMMAND ${CMAKE_COMMAND} -DINSTALL_DIR=${SITE_LISP}/yasnippet -DBINARY_DIR=<BINARY_DIR> -P ${CMAKE_SOURCE_DIR}/cmake/YASnippetInstall.cmake
  )
