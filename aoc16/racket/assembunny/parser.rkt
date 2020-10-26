#lang brag

assembunny-program : [assembunny-line] ((/NEWLINE)* [assembunny-line])*
assembunny-line : inc | dec | jnz | cpy
@val : number | register
cpy : /"cpy" val val
jnz : /"jnz" val val
inc : /"inc" val
dec : /"dec" val
@register : REGISTER
@number : INTEGER
