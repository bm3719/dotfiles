" .vimrc
" Bruce C. Miller
" Time-stamp: <2010-09-14 09:38:04 (bm3719)>
" Addons used: wombat color theme, haskellmode vimball, Consolas font 
" (win32), and terminus (*nix).

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""
" Various, miscellaneous, assorted, sundry settings.

set nocompatible                    " Use vim settings, rather then vi settings 
                                    " This must be first, because it changes other options 
                                    " as a side effect.
set backspace=indent,eol,start      " Allow backspacing over everything in insert mode.
set history=50		            " Keep 50 lines of command line history.
set ruler		            " Show the cursor position all the time.
set showcmd		            " Display incomplete commands.
set incsearch		            " Do incremental searching.
set softtabstop=2 shiftwidth=2 expandtab
set sm                              " Automatic brace matching.
set shellslash                      " On win32, use shellslash.
set grepprg=grep\ -nH\ $*           " Set grep to always display the filename.
set smartcase                       " Use intelligent case while searching. If
                                    " search string contains an upper case
                                    " letter, disable ignorecase.  
set nocindent                       " Don't use c style indenting.
set copyindent                      " Copy the previous indentation on autoindenting.
set linebreak                       " Break lines at the nearest word boundary.
set wrap                            " Enable line wrapping.
set showmatch                       " When closing a block, show the matching bracket.
set matchpairs+=<:>                 " Include angle brackets in matching.
set showmode                        " Show current mode in the status line.
set modeline                        " Look for embedded modelines at the top of the file.
set scrolloff=2                     " Start scrolling at this number of lines from the bottom.
set ttyfast                         " Indicates a fast terminal connection.
set wildmenu                        " Enables wildmenu tab completion.
set wildignore=*.swp,*.bak,*.pyc,*.class
set wildmode=longest:full,list:full " Type of wildmenu.
set viminfo='500,f1,:100,/100       " Massively detailed viminfo file.
set fileformats=unix,dos,mac        " Try to detect file formats.  
                                    " Unix for new files and autodetect for the rest.  
set shortmess=aTItoO                " Disable the splash screen. Get a job, Uganda.
                                    " Also abbreviates messages, like the `Press ENTER' ones.
set guioptions-=T                   " Remove toolbar.
"set guioptions-=m                  " Remove menu.
set backspace=indent,eol,start
set formatoptions=croq
set encoding=utf-8 fileencodings=   " Always use Unicode.
set ls=2                            " Show status line.
set vb t_vb=                        " Use visual bell instead of audible.
set writebackup                     " Keep backup while we are editing.
set clipboard+=unnamed              " Put yanks/etc on clipboard.
set go+=a                           " Default to selection copied to clipboard.
set hidden                          " Multiple buffer management.
set notitle                         " Prevent vim from overriding the terminal title.
"set paste                          " Disable auto-indent.
"set verbose=9                      " Turn this ON when we want to debug.

" these settings should only be enabled in gvim mode
if has("win32")
  set lines=48 columns=98           " Set default gui size.
  set number                        " Enable line numbering.
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

if has("win32")                     " Win32-specific settings.
  set guifont=Consolas:h10          " font to use
  "set guifont=Courier_New:h10:cANSI
  set grepprg=findstr\ /R\ /S\ /N   " Fix findstr for Win32.
  " Remove 't' flag from 'guioptions': no tearoff menu entries.
  let &guioptions = substitute(&guioptions, "t", "", "g") 
  set nobackup                      " Disable backup files.
  set backupdir=~/.vim              " Where backups go.
  set directory=~/.vim              " Where swap files go.
else                                " *nix-specific settings.
  set guifont=terminus:h14
  set backupdir=~/.vim              " Where backups go.
  set directory=~/.vim              " Where swap files go.
endif

if has("vms")
  set nobackup	                    " Do not keep a backup file, use versions instead.
else
  set backup		            " Keep a backup file.
endif

" This is an alternative that also works in block mode, but the deleted
" text is lost and it only works for putting the current register.
"vnoremap p "_dp

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END

else

  set autoindent		" Always set autoindenting on.
  " Use smart indenting when starting a new line.
  set smartindent

endif 

autocmd! bufwritepost _vimrc source $VIM/_vimrc

" Disable auto-comment insertion.
au FileType * setl fo-=cro

" This is an addon color theme.
if has("gui_running")
  " colorscheme industrial
  " Switched to wombat: http://files.werx.dk/wombat.vim
  colorscheme wombat
endif


""""""""""""""
" Key mappings

" Hit C-N twice to toggle line nums.
nmap <C-N><C-N> :set invnumber<CR>
imap <Insert> <Nop>             " disable insert key
" Don't use Ex mode, use Q for formatting.
map Q gq
map <F1> :previous<CR>          " map F1 to open previous buffer
map <F2> :next<CR>              " map F2 to open next buffer
" Choose right syntax highlighting with tab completion.
map <F3> :source $VIM/syntax/
map <F6> :b#<CR>                " fast buffer switching
" F9 toggles highlighting.
map <F9> :if has("syntax_items")<CR>syntax off<CR>else<CR>syntax on<CR>endif<CR><CR>

" Vim 7 specific mappings.
if version >= 700
  map <C-t> <Esc>:tabnew<CR>
  map <C-F4> <Esc>:tabclose<CR> 
endif

" Act more like a pager when invoked as one.
if (v:progname == "view")
    nmap <Space> <PageDown>
    nmap b       <PageUp>
    nmap q       :q<CR>
endif


"""""""""""""""""""""""""" 
" Language specific config

" C
" :make will expand to gcc file.c -o file
set makeprg=gcc\ -o\ %<\ %

" C#
" fold #regions
let b:match_words = '\s*#\s*region.*$:\s*#\s*endregion'

" Java 
let java_highlight_functions=1
 autocmd BufRead,BufNewFile *.java set makeprg=javac\ %
"autocmd BufRead BufNewFile *.java set makeprg=ant\ -emacs
