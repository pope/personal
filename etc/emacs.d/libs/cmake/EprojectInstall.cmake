file(MAKE_DIRECTORY ${INSTALL_DIR})

file(GLOB elisp_files "${BINARY_DIR}/eproject.*")
file(GLOB elisp_extras_files "${BINARY_DIR}/eproject-extras.*")
list(APPEND elisp_files ${ep_files} ${ep_extras_files})

foreach(elisp_file ${elisp_files})
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy ${elisp_file} ${INSTALL_DIR}
    RESULT_VARIABLE error_code
    )
  if(error_code)
    message(FATAL_ERROR "Failed to byte compile ${elisp_file}")
  endif()
endforeach()

execute_process(
  COMMAND ${CMAKE_COMMAND} -E copy_directory lang ${INSTALL_DIR}/lang
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to copy over snippets directory")
endif()
