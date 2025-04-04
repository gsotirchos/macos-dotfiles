let g:easytags_suppress_ctags_warning = 1
let g:easytags_auto_update = 1  " overriden per buffer by filetype-specific autocommand
let g:easytags_auto_highlight = 1  " overriden per buffer by filetype-specific autocommand
let g:easytags_async = 1
let g:easytags_always_enabled = 1
let g:easytags_on_cursorhold = 0
"let g:easytags_events = ['BufWinEnter', 'BufRead', 'BufWrite']
let g:easytags_updatetime_min = 1000
let g:easytags_syntax_keyword = 'auto'  " set this to 'always' if 'auto' if there is slowdown
let g:easytags_include_members = 1
let g:easytags_resolve_links = 1
let g:easytags_cmd = 'ctags'
let g:easytags_opts = [
\   '--excmd=mix',
\   '--fields=+lnZs',
\   '--kinds-Python=+l',
\   '--kinds-C=+lpxAN',
\   '--kinds-C++=+lpxAN',
\   '--extras=+qr'
\]
let g:easytags_dynamic_files = 2
