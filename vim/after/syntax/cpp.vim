" operators, logicals, parentheses and member operators
syntax match myParens "[(){}\[\]<>,;]" containedin=NONE
syntax match MembOperator "[,;]" containedin=NONE
" syntax match MembOperator "\(\w\|[\])][ \n]*\)\@<=\(\(::\)\|\(->\)\|\(\.\)\)\([ \n]*\h\)\@=" containedin=NONE
syntax match MembOperator "\(\(::\)\|\(->\)\|\(\.\)\)\([ \n]*\h\)\@=" containedin=NONE

syntax match myEquals "=" containedIn=None

syntax match myOperator "->" contains=TOP
syntax match myOperator "[-+*/^?$%&|\\!~:]\+" contains=TOP
syntax match myOperator "\(\S\)\@<![<>!~:]\+[=]\=\(\S\)\@!"
syntax match myOperator "\(\S\)\@<!\(\(==\)\|\(->\)\)\(\S\)\@!"

" highlight %Tag(...)% in comments
syntax region myTagMark
\   start="\(\".*\)\@<!\(% *Tag *(\)\@<="
\   end="\() *%\)\@="
\   containedin=.*Comment.* contained oneline

" darker semicolons
"syntax match cTerminator ";"

" highlight 'namespace', 'enum', and 'struct' names
syntax match cppNamespace "\(^\(.*;\+\)*[ \n]*\(using \)\=namespace[ \n]\+\)\@<=\h\w*"

syntax match cPreProcVar "\(\s*#\w*\s\)\@<=\S*" containedIn=.*PreProc.* contained

hi! link cCustom Function

hi! link myParens        Comment
hi! link myOperator      OtherType
hi! link myEquals        Statement
hi! link myTagMark       SpecialComment
"hi! link myScopeOperator Normal
hi! link cTerminator     MembOperator
hi! link cTypedef        Typedef
"hi! link cDefine         Constant
hi! link cPreProcVar     Constant
hi! link cOperator       Statement
hi! link cMember         Function
"hi! link cppCast         Function
hi! link cEnum           cMember
hi! link cppAttribute    Comment
hi! link cppNamespace    Identifier
hi! link cppOperator     cOperator
hi! link cppSTLvariable  cMember
hi! link cppSTLnamespace cppNamespace
hi! link cppSTLconstant  cppSTLvariable
hi! link cppSTLiterator  cppSTLvariable
hi! link cppSTLexception cppSTLvariable

" vim-easytags
hi! link cPreProcTag  Constant
hi! link cTypeTag     cppNamespace
"hi! link cFunctionTag cFunction
hi! link cMemberTag   cMember
hi! link cEnumTag     cMemberTag

" Doxygen
set syntax=cpp.doxygen

augroup cpp_syntax
    autocmd!
    autocmd BufWinEnter *.cpp syntax keyword cppSTLconstant nullptr
augroup END
