if get(g:, "coc_enabled", 0) == 0
    finish
endif

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <C-n>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent> <expr> <C-n> coc#pum#visible() ? coc#pum#next(1) : coc#refresh()
inoremap <silent> <expr> <C-p> coc#pum#visible() ? coc#pum#prev(1) : ''

" Make <Return> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent> <expr> <Return>
    \ coc#pum#visible() ?
        \ coc#pum#confirm() :
        \ '<C-g>u<Return><C-r>=coc#on_enter()<Return>'

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> <nowait> [g <Plug>(coc-diagnostic-prev)
nmap <silent> <nowait> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> <nowait> gd <Plug>(coc-definition)
nmap <silent> <nowait> gy <Plug>(coc-type-definition)
nmap <silent> <nowait> gi <Plug>(coc-implementation)
nmap <silent> <nowait> gr <Plug>(coc-references)

" Symbol renaming
nmap <Leader>rn <Plug>(coc-rename)

" Formatting selected code
xmap <Leader>f <Plug>(coc-format-selected)
nmap <Leader>f <Plug>(coc-format-selected)

" TODO
"" Applying code actions to the selected code block
"" Example: `<Leader>aap` for current paragraph
"xmap <Leader>a <Plug>(coc-codeaction-selected)
"nmap <Leader>a <Plug>(coc-codeaction-selected)
"
"" Remap keys for applying code actions at the cursor position
"nmap <Leader>ac <Plug>(coc-cofeaction-cursor)
"" Remap keys for apply code actions affect whole buffer
"nmap <Leader>as <Plug>(coc-codeaction-source)
"" Apply the most preferred quickfix action to fix diagnostic on the current line
"nmap <Leader>qf <Plug>(coc-fix-current)
"
"" Remap keys for applying refactor code actions
"nmap <silent> <Leader>rf <Plug>(coc-codeaction-refactor)
"xmap <silent> <Leader>rf <Plug>(coc-codeaction-refactor-selected)
"nmap <silent> <Leader>rs <Plug>(coc-codeaction-refactor-selected)

" Run the Code Lens action on the current line
nmap <Leader>cl <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

"" Remap <C-d> and <C-u> to scroll float windows/popups
"if has('nvim-0.4.0') || has('patch-8.2.0750')
"    nnoremap <silent> <nowait> <expr> <C-d> coc#float#has_scroll() ? coc#float#scroll(1) : '<C-d>'
"    nnoremap <silent> <nowait> <expr> <C-u> coc#float#has_scroll() ? coc#float#scroll(0) : '<C-u>'
"    inoremap <silent> <nowait> <expr> <C-d> coc#float#has_scroll() ? '<C-r>=coc#float#scroll(1)<Return>' : '<C-d>'
"    inoremap <silent> <nowait> <expr> <C-u> coc#float#has_scroll() ? '<C-r>=coc#float#scroll(0)<Return>' : '<C-u>'
"    vnoremap <silent> <nowait> <expr> <C-d> coc#float#has_scroll() ? coc#float#scroll(1) : '<C-d>'
"    vnoremap <silent> <nowait> <expr> <C-u> coc#float#has_scroll() ? coc#float#scroll(0) : '<C-u>'
"endif

" Use CTRL-S for selections ranges
" Requires 'textDocument/selectionRange' support of language server
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OrganizeImports :call CocActionAsync('runCommand', 'editor.action.organizeImport')

"" Add (Neo)Vim's native statusline support
"" NOTE: Please see `:h coc-status` for integrations with external plugins that
"" provide custom statusline: lightline.vim, vim-airline
"set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics
nnoremap <silent> <Leader><Space>a :<C-u>CocList diagnostics<Return>
" Manage extensions
nnoremap <silent> <Leader><Space>e :<C-u>CocList extensions<Return>
" Show commands
nnoremap <silent> <Leader><Space>c :<C-u>CocList commands<Return>
" Find symbol of current document
nnoremap <silent> <Leader><Space>o :<C-u>CocList outline<Return>
" Search workspace symbols
nnoremap <silent> <Leader><Space>s :<C-u>CocList -I symbols<Return>
" Do default action for next item
nnoremap <silent> <Leader><Space>j :<C-u>CocNext<Return>
" Do default action for previous item
nnoremap <silent> <Leader><Space>k :<C-u>CocPrev<Return>
" Resume latest coc list
nnoremap <silent> <Leader><Space>p :<C-u>CocListResume<Return>
