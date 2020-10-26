#lang brag

screen-program : [screen-line] ((/NEWLINE)* [screen-line])*
@screen-line : rect | rotate-row | rotate-column
rect : /"rect" number number
rotate-row : /"rotate row" number number
rotate-column : /"rotate column" number number
@number : INTEGER
