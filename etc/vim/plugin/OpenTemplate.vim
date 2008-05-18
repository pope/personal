if exists( "loaded_OpenTemplate" )
  finish
endif
let loaded_OpenTemplate = 1

fun! OpenTemplate( arg )
  if match( getline("."), "call " ) >= 0
    " find either a starting < or "
    let start = match( getline("."), "call" )

    if start < 0
      return
    endif

    let start = start + 5

    let filename = strpart( getline("."), start )

    let end = match( filename, "(" )

    if end > 0
      let filename = strpart( filename, 0, end )
      let filename = substitute(filename, "\\.", "/", "g") . ".tea"
      let directory = expand("%:p:h")
      while strlen(directory) > 0
        if filereadable(directory . "/" . filename)
          let $filename = directory . "/" . filename
          if a:arg == "tabnew"
            tabnew $filename
          elseif a:arg == "sp"
            sp $filename
          endif
          return
        else
          let directory = strpart(directory, 0, match(directory, "[^\\\/]*$")-1)
        endif
      endwhile
    endif
	endif
endfun
