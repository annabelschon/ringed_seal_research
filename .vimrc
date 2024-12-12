
" I found these color schemes to be good.
"colorscheme morning
colorscheme default
"colorscheme elflord
"colorscheme delek
syntax on

set directory=/tmp
set number
set wrapmargin=5
set showmode
set ignorecase
set pastetoggle=<F2>
set ai
set expandtab

" v will reformat the current paragraph (the ^M's are <ctrl-v><cr>)
map v {0!}fmt -w 64
" V will run a spellchecker script
map V :w:!spellcheck % :e!
" = will center the text on the current line (the ^[ is <ctrl><esc>)
map = 70I $68hd0:s/  / /g$p

" # will convert '> ' to '*' in a forwarded document
" map # :%s/> /*/g

" # will toggle line numbers on and off
map \o# o:se nonu:se nu-:map \o# "wp

map \d# "w2dd
map \x# "xdd@x"xpk
map # ma3L\o#\x#\d#'a:


" after marking the begining of the section with an m (use mm), the
"   following can be used

" ctrl-x = cut
map  "zd'm

" ctrl-p = copy
map  "zy'm

" ctrl-v = paste
map  "zp


" ctrl-z will strip the ^M's from a DOS text file
map  :%s/.$//g

" ctrl-a doubles the line spacing, if you then type in <ctrl-v><cr><cr>
map  :%s/$/
