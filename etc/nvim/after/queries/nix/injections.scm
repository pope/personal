; extends

(apply_expression
  function: (_) @_func
  argument: [
    (string_expression
	  ((string_fragment) @injection.content (#set! injection.language "css")))
    (indented_string_expression
	  ((string_fragment) @injection.content (#set! injection.language "css")))
  ]
  (#match? @_func "^css$")
  (#set! injection.combined))
