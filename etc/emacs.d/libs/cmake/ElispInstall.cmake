file(MAKE_DIRECTORY ${INSTALL_DIR})

file(GLOB el_files "${BINARY_DIR}/*.el")
file(GLOB elc_files "${BINARY_DIR}/*.elc")
list(APPEND elisp_files ${el_files} ${elc_files})

foreach(elisp_file ${elisp_files})
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy ${elisp_file} ${INSTALL_DIR}
    RESULT_VARIABLE error_code
    )
  if(error_code)
    message(FATAL_ERROR "Failed to install ${elisp_file}")
  endif()
endforeach()
