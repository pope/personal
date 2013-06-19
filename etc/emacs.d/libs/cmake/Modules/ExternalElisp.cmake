include(ExternalProject)
include(CmakeParseArguments)
include(FindUnixCommands)
include(FindWget)
include(FindPackageHandleStandardArgs)

if(NOT WGET_FOUND)
  message(SEND_ERROR "wget could not be found")
endif()

find_program(EMACS_EXECUTABLE emacs)
mark_as_advanced(EMACS_EXECUTABLE)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Emacs REQUIRED_VARS EMACS_EXECUTABLE)

find_program(TRUE_EXECUTABLE true)
mark_as_advanced(TRUE_EXECUTABLE)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(True REQUIRED_VARS TRUE_EXECUTABLE)

function(ExternalElisp name)
  set(options)
  set(oneValueArgs)
  set(multiValueArgs
    GITHUB_USER GITHUB_PROJECT GITHUB_BRANCH
    URL_FILE SVN_REPOSITORY URL_ARCHIVE
    INSTALL_COMMAND BUILD_COMMAND
    DEPENDS)
  cmake_parse_arguments(_EE "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  set(ep_args
    PREFIX ${name}
    )

  if(_EE_GITHUB_USER)
    set(github_project ${name})
    if(_EE_GITHUB_PROJECT)
      set(github_project ${_EE_GITHUB_PROJECT})
    endif()
    set(github_branch "master")
    if(_EE_GITHUB_BRANCH)
      set(github_branch ${_EE_GITHUB_BRANCH})
    endif()
    set(github_url "https://github.com/${_EE_GITHUB_USER}/${github_project}/tarball/${github_branch}")
    list(APPEND ep_args
      DOWNLOAD_COMMAND ${WGET_EXECUTABLE} --no-check-certificate ${github_url} -O ${name}.tar.gz
      COMMAND ${TAR} xzf ${name}.tar.gz --strip-components=1 -C <SOURCE_DIR>
      COMMAND ${CMAKE_COMMAND} -E remove ${name}.tar.gz
      )
  elseif(_EE_URL_FILE)
    list(APPEND ep_args
      DOWNLOAD_DIR "${name}/src/${name}"
      DOWNLOAD_COMMAND ${WGET_EXECUTABLE} --no-check-certificate "${_EE_URL_FILE}" -O ${name}.el
      )
  elseif(_EE_URL_ARCHIVE)
    list(APPEND ep_args
      DOWNLOAD_COMMAND ${WGET_EXECUTABLE} --no-check-certificate "${_EE_URL_ARCHIVE}" -O ${name}.tar.gz
      COMMAND ${TAR} xzf ${name}.tar.gz --strip-components=1 -C <SOURCE_DIR>
      COMMAND ${CMAKE_COMMAND} -E remove ${name}.tar.gz
      )
  elseif(_EE_SVN_REPOSITORY)
    list(APPEND ep_args SVN_REPOSITORY ${_EE_SVN_REPOSITORY})
  else()
    message(AUTHOR_WARNING "You must supply a downloading command type")
  endif()

  list(APPEND ep_args
    CONFIGURE_COMMAND ${TRUE_EXECUTABLE}
    BUILD_IN_SOURCE 1
    )

  if(_EE_DEPENDS)
    list(APPEND ep_args DEPENDS ${_EE_DEPENDS})
  endif()

  if(_EE_BUILD_COMMAND)
    list(APPEND ep_args
      BUILD_COMMAND ${_EE_BUILD_COMMAND})
  else()
    list(APPEND ep_args BUILD_COMMAND
      ${CMAKE_COMMAND}
      -DINSTALL_DIR=${CMAKE_BINARY_DIR}/site-lisp
      -DBINARY_DIR=${CMAKE_BINARY_DIR}/${name}/src/${name}
      -P ${CMAKE_SOURCE_DIR}/cmake/ElispBuild.cmake)
  endif()

  if(_EE_INSTALL_COMMAND)
    list(APPEND ep_args
      INSTALL_COMMAND ${_EE_INSTALL_COMMAND})
  else()
    list(APPEND ep_args INSTALL_COMMAND
      ${CMAKE_COMMAND}
      -DINSTALL_DIR=${CMAKE_BINARY_DIR}/site-lisp
      -DBINARY_DIR=${CMAKE_BINARY_DIR}/${name}/src/${name}
      -P ${CMAKE_SOURCE_DIR}/cmake/ElispInstall.cmake)
  endif()

  ExternalProject_Add(${name} ${ep_args})
endfunction()
