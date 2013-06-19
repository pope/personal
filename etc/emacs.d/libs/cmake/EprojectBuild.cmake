find_program(EMACS_EXECUTABLE emacs)
if(NOT EMACS_EXECUTABLE)
  message(SEND_ERROR "Emacs could not be found")
endif()

file(GLOB elisp "${BINARY_DIR}/*.el")
execute_process(
  COMMAND ${EMACS_EXECUTABLE} -L ${INSTALL_DIR} -L ${BINARY_DIR} -batch -Q -f batch-byte-compile ${elisp}
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to byte compile elisp")
endif()

file(GLOB elisp "${BINARY_DIR}/lang/*.el")
execute_process(
  COMMAND ${EMACS_EXECUTABLE} -L ${INSTALL_DIR} -L ${BINARY_DIR} -L ${BINARY_DIR}/lang -batch -Q -f batch-byte-compile ${elisp}
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to byte compile elisp")
endif()
