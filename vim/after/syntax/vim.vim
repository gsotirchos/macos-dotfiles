syntax match vimComment "\".*" containedin=vim.*Comment
syn match Error "\\@\d*[<>]\=!\==\="  " not working


"hi! clear CursorLine
"hi! clear CursorColumn
hi! link vim9Comment     Normal
hi! link vimSep          MyParens
hi! link vimParenSep     vimSep
hi! link vimSetSep       vimSep
hi! link vimBracket      vimSep
hi! link vimSetRegion    vimSet
hi! link vimCmdSep       vimSep
hi! link vimMapModKey    PreProc
hi! link vimMapMod       vimMapModKey
hi! link vimFunction     Function
hi! link vimUserFunc     vimFunction
hi! link vimGroup        Function
hi! link vimGroupSpecial Function
hi! link vimHiGroup      vimGroup
hi! link vimEnvVar       Function
hi! link vimVar          Normal
hi! link vimGroupName    vimVar
hi! link vimOption       vimVar
hi! link vimOptionVarName vimVar
hi! link vimFTOption     PreProc
hi! link vimOper         Operator
hi! link vimContinue     Comment

" ALE
"hi! link ALEErrorLine   Normal
"hi! link ALEWarningLine Normal
