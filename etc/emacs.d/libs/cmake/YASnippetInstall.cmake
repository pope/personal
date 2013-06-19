file(MAKE_DIRECTORY ${INSTALL_DIR})

file(GLOB elisp_files "${BINARY_DIR}/yasnippet.*")

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
  COMMAND ${CMAKE_COMMAND} -E copy_directory snippets ${INSTALL_DIR}/snippets
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to copy over snippets directory")
endif()
