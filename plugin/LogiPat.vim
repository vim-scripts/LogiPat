" LogiPat:
"   Author:  Charles E. Campbell, Jr.
"   Date:    May 23, 2005
"   Version: 1
"   Purpose: to do Boolean-logic based regular expression pattern matching
"   Usage: {{{1
"       :LogiPat ...
"
"         Boolean logic supported:
"            () grouping operators
"            !  not the following pattern
"            |  logical or
"            &  logical and
"            "..pattern.."
"  Example: {{{1
"		:LogiPat !("january"|"february")
"		  would match all strings not containing the strings january
"		  or february
"
" Luke 1:31-33
"		  Behold, you will conceive in your womb, and bring forth a son,
"		  and will call his name Jesus. He will be great, and will be
"		  called the Son of the Most High. The Lord God will give him the
"		  throne of his father, David, and he will reign over the house of
"		  Jacob forever. There will be no end to his kingdom.
"
" GetLatestVimScripts: 1290 1 :AutoInstall: LogiPat.vim

" ---------------------------------------------------------------------
" Load Once: {{{1
if &cp || exists("loaded_logipat")
 finish
endif
let g:loaded_LogiPat = "v1"
let s:keepcpo        = &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Public Interface: {{{1
com!         -nargs=* LogiPat      call   LogiPat(<q-args>,1)
silent! com  -nargs=* LP           call   LogiPat(<q-args>,1)
com!         -nargs=+ LogiPatFlags let  s:LogiPatFlags=<args>
silent! com! -nargs=+ LPF          let  s:LogiPatFlags=<args>

" =====================================================================
" Functions: {{{1

" ---------------------------------------------------------------------
" LogiPat: this function interprets the boolean-logic pattern {{{2
fun! LogiPat(pat,...)
"  call Dfunc("LogiPat(pat<".a:pat.">)")

  " LogiPat(pat,dosearch)
  if a:0 > 0
   let dosearch= a:1
  else
   let dosearch= 0
  endif

  let s:npatstack = 0
  let s:nopstack  = 0
  let s:preclvl   = 0
  let expr        = a:pat

  " Lexer/Parser
  while expr != ""
"   call Decho("expr<".expr.">")

   if expr =~ '^"'
   	" push a Pattern
   	let pat    = substitute(strpart(expr,1),'^\([^"]*\)".*$','\1','')
   	let patlen = strlen(pat) - 1
"	call Decho("pat<".pat."> patlen-1=".patlen)
	if patlen > 1 && strpart(pat,patlen,1) == '\\'
	 echoerr "LogiPat doesn't accept escaped backquotes in patterns (yet)"
"	 call Dret("LogiPat --error--")
     return '--error--'
	endif
	call s:LP_PatPush('.*'.pat.'.*')
	let patlen = patlen+3
	let expr   = strpart(expr,patlen)

   elseif expr =~ '^[!()|&]'
   	" push an operator
   	let op   = strpart(expr,0,1)
   	let expr = strpart(expr,strlen(op))
	call s:LP_OpPush(op)

   elseif expr =~ '^\s'
   	" skip whitespace
   	let expr= strpart(expr,1)

   else
   	echoerr "operator<".strpart(expr,0,1)."> not supported (yet)"
   	let expr= strpart(expr,1)
   endif

  endwhile

  " Final Execution
  call s:LP_OpPush('Z')

  let result= s:LP_PatPop(1)
"  call Decho("result=".result)

  " sanity checks and cleanup
  if s:npatstack > 0
   echoerr s:npatstack." patterns left on stack!"
   let s:npatstack= 0
  endif
  if s:nopstack > 0
   echoerr s:nopstack." operators left on stack!"
   let s:nopstack= 0
  endif

  " perform the indicated search
  if dosearch
   if exists("s:LogiPatFlags")
"  call Decho("search(result<".result."> LogiPatFlags<".LogiPatFlags.">)")
    call search(result,LogiPatFlags)
   else
"  call Decho("search(result<".result.">)")
    call search(result)
   endif
   let @/= result
  endif

"  call Dret("LogiPat ".result)
  return result
endfun

" ---------------------------------------------------------------------
" LP_PatPush: {{{2
fun! s:LP_PatPush(pat)
"  call Dfunc("LP_PatPush(pat<".a:pat.">)")
  let s:npatstack              = s:npatstack + 1
  let s:patstack_{s:npatstack} = a:pat
"  call s:StackLook("patpush") "Decho
"  call Dret("LP_PatPush : npatstack=".s:npatstack)
endfun

" ---------------------------------------------------------------------
" LP_PatPop: pop a number/variable from LogiPat's pattern stack {{{2
fun! s:LP_PatPop(lookup)
"  call Dfunc("LP_PatPop(lookup=".a:lookup.")")
  if s:npatstack > 0
   let ret         = s:patstack_{s:npatstack}
   let s:npatstack = s:npatstack - 1
  else
   let ret= "---error---"
   echoerr "(LogiPat) invalid expression"
  endif
"  call s:StackLook("patpop") "Decho
"  call Dret("LP_PatPop ".ret)
  return ret
endfun

" ---------------------------------------------------------------------
" LP_OpPush: {{{2
fun! s:LP_OpPush(op)
"  call Dfunc("LP_OpPush(op<".a:op.">)")

  " determine new operator's precedence level
  if a:op == '('
  	let s:preclvl= s:preclvl + 10
	let preclvl  = s:preclvl
  elseif a:op == ')'
  	let s:preclvl= s:preclvl - 10
   if s:preclvl < 0
    let s:preclvl= 0
    echoerr "too many )s"
   endif
   let preclvl= s:preclvl
  elseif a:op =~ '|'
   let preclvl= s:preclvl + 2
  elseif a:op =~ '&'
   let preclvl= s:preclvl + 4
  elseif a:op == '!'
   let preclvl= s:preclvl + 6
  elseif a:op == 'Z'
   let preclvl= -1
  else
   echoerr "expr<".expr."> not supported (yet)"
   let preclvl= s:preclvl
  endif

  " execute higher-precdence operators
  call s:LP_Execute(preclvl)

  " push new operator onto operator-stack
  if a:op =~ '!'
   let s:nopstack             = s:nopstack + 1
   let s:opprec_{s:nopstack}  = preclvl
   let s:opstack_{s:nopstack} = a:op
  elseif a:op =~ '|'
   let preclvl= s:preclvl + 1
   let s:nopstack             = s:nopstack + 1
   let s:opprec_{s:nopstack}  = preclvl
   let s:opstack_{s:nopstack} = a:op
  elseif a:op == '&'
   let preclvl= s:preclvl + 2
   let s:nopstack             = s:nopstack + 1
   let s:opprec_{s:nopstack}  = preclvl
   let s:opstack_{s:nopstack} = a:op
  endif

"  call s:StackLook("oppush") "Decho
"  call Dret("LP_OpPush")
endfun

" ---------------------------------------------------------------------
" LP_Execute: execute operators from opstack using pattern stack {{{2
fun! s:LP_Execute(preclvl)
"  call Dfunc("LP_Execute(preclvl=".a:preclvl.") npatstack=".s:npatstack." nopstack=".s:nopstack)

  " execute all higher precedence operators
  while s:nopstack > 0 && a:preclvl < s:opprec_{s:nopstack}
   let op= s:opstack_{s:nopstack}
"   call Decho("op<".op."> nop=".s:nopstack." [preclvl=".a:preclvl."] < [opprec_".s:nopstack."=".s:opprec_{s:nopstack}."]")

   let s:nopstack = s:nopstack - 1
 
   if     op == '!'
    let n1= s:LP_PatPop(1)
	call s:LP_PatPush(s:LP_Not(n1))
 
   elseif op == '|'
    let n1= s:LP_PatPop(1)
    let n2= s:LP_PatPop(1)
    call s:LP_PatPush(s:LP_Or(n2,n1))
 
   elseif op =~ '&'
    let n1= s:LP_PatPop(1)
    let n2= s:LP_PatPop(1)
    call s:LP_PatPush(s:LP_And(n2,n1))
   endif
 
"   call s:StackLook("execute") "Decho
  endwhile

"  call Dret("LP_Execute")
endfun

" ---------------------------------------------------------------------
" LP_Not: writes a logical-not for a pattern {{{2
fun! s:LP_Not(pat)
"  call Dfunc("LP_Not(pat<".a:pat.">)")
  if a:pat =~ '^\.\*' && a:pat =~ '\.\*$'
   let pat= substitute(a:pat,'^\.\*\(.*\)\.\*$','\1','')
   let ret= '^\%(\%('.pat.'\)\@!.\)*$'
  else
   let ret= '^\%(\%('.a:pat.'\)\@!.\)*$'
  endif
"  call Dret("LP_Not ".ret)
  return ret
endfun

" ---------------------------------------------------------------------
" LP_Or: writes a logical-or branch using two patterns {{{2
fun! s:LP_Or(pat1,pat2)
"  call Dfunc("LP_Or(pat1<".a:pat1."> pat2<".a:pat2.">)")
  let ret= a:pat1.'\|'.a:pat2
"  call Dret("LP_Or ".ret)
  return ret
endfun

" ---------------------------------------------------------------------
" LP_And: writes a logical-and concat using two patterns {{{2
fun! s:LP_And(pat1,pat2)
"  call Dfunc("LP_And(pat1<".a:pat1."> pat2<".a:pat2.">)")
  let ret= '\%('.a:pat1.'\&'.a:pat2.'\)'
"  call Dret("LP_And ".ret)
  return ret
endfun

" ---------------------------------------------------------------------
" StackLook: {{{2
fun! s:StackLook(description)
"  call Dfunc("StackLook(description<".a:description.">)")
  let iop = 1
  let ifp = 1
"  call Decho("Pattern                       Operator")

  " print both pattern and operator
  while ifp <= s:npatstack && iop <= s:nopstack
   let fp = s:patstack_{ifp}
   let op = s:opstack_{iop}." (P".s:opprec_{s:nopstack}.')'
   let fplen= strlen(fp)
   if fplen < 30
   	let fp= fp.strpart("                              ",1,30-fplen)
   endif
"   call Decho(fp.op)
   let ifp = ifp + 1
   let iop = iop + 1
  endwhile

  " print just pattern
  while ifp <= s:npatstack
   let fp  = s:patstack_{ifp}
"   call Decho(fp)
   let ifp = ifp + 1
  endwhile

  " print just operator
  while iop <= s:nopstack
   let op  = s:opstack_{iop}." (P".s:opprec_{s:nopstack}.')'
"   call Decho("                              ".op)
   let iop = iop + 1
  endwhile
"  call Dret("StackLook")
endfun

" ---------------------------------------------------------------------
"  Cleanup And Modeline: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
" HelpExtractor:
"  Author:	Charles E. Campbell, Jr.
"  Version:	3
"  Date:	Sep 09, 2004
"
"  History:
"    v2 Nov 24, 2003 : On Linux/Unix, will make a document directory
"                      if it doesn't exist yet
"
" GetLatestVimScripts: 748 1 HelpExtractor.vim
" ---------------------------------------------------------------------
set lz
let s:keepcpo= &cpo
set cpo&vim
let docdir = substitute(expand("<sfile>:r").".txt",'\<plugin[/\\].*$','doc','')
if !isdirectory(docdir)
 if has("win32")
  echoerr 'Please make '.docdir.' directory first'
  unlet docdir
  finish
 elseif !has("mac")
  exe "!mkdir ".docdir
 endif
endif

let curfile = expand("<sfile>:t:r")
let docfile = substitute(expand("<sfile>:r").".txt",'\<plugin\>','doc','')
exe "silent! 1new ".docfile
silent! %d
exe "silent! 0r ".expand("<sfile>:p")
silent! 1,/^" HelpExtractorDoc:$/d
exe 'silent! %s/%FILE%/'.curfile.'/ge'
exe 'silent! %s/%DATE%/'.strftime("%b %d, %Y").'/ge'
norm! Gdd
silent! wq!
exe "helptags ".substitute(docfile,'^\(.*doc.\).*$','\1','e')

exe "silent! 1new ".expand("<sfile>:p")
1
silent! /^" HelpExtractor:$/,$g/.*/d
silent! wq!

set nolz
unlet docdir
unlet curfile
"unlet docfile
let &cpo= s:keepcpo
unlet s:keepcpo
finish

" ---------------------------------------------------------------------
" Put the help after the HelpExtractorDoc label...
" HelpExtractorDoc:
*logipat.txt*	Logical Patterns				May 23, 2005

Author:  Charles E. Campbell, Jr.  <NdrOchip@ScampbellPfamily.AbizM>

==============================================================================
1. Contents							*logipat*

	1. Contents.................: |logipat-contents|
	2. LogiPat Manual...........: |logipat-manual|
	3. LogiPat Examples.........: |logipat-examples|
	4. Caveat...................: |logipat-caveat|
	5. LogiPat History..........: |logipat-history|

==============================================================================
2. LogiPat Manual			*logipat-manual* *logipat-man*


	*logipat-arg* *logipat-input* *logipat-pattern* *logipat-operators*
	Boolean logic patterns are composed of

			operators	! = not
					| = logical-or
					& = logical-and
			grouping	( ... )
			patterns	"pattern"

	:LogiPat {boolean-logic pattern}		*:LogiPat*
		:LogiPat is a command which takes a boolean-logic
		argument (|logipat-arg|).

	:LP {boolean-logic pattern}			*:LP*
		:LP is a shorthand command version of :LogiPat
		(|logipat-cmd|).

	:LogiPatFlags {search flags}			*LogiPat-flags*
		:LogiPatFlags {search flags}
		LogiPat uses the |search()| command.  The flags
		passed to that call to search() may be specified
		by the :LogiPatFlags command.

	:LPF {search flags}				*:LPF*
		:LPF is a shorthand version of :LogiPatFlags.

	:let pat=LogiPat({boolean-logic pattern})	*LogiPat()*
		If one calls LogiPat() directly, no search
		is done, but the transformation from the boolean
		logic pattern into a regular expression pattern
		is performed and returned.

==============================================================================
3. LogiPat Examples					*logipat-examples*

	LogiPat takes Boolean logic arguments and produces a regular
	expression which implements the choices.  A series of examples
	follow:
>
	:LogiPat "abc"
<		will search for lines containing the string "abc"
>
	:LogiPat !"abc"
<		will search for lines which don't contain the string "abc"
>
	:LogiPat "abc"|"def"
<		will search for lines which contain either the string
		"abc" or the string "def"
>
	:LogiPat !("abc"|"def")
<		will search for lines which don't contain either
		of the strings "abc" or "def"
>
	:LogiPat "abc"&"def"
<		will search for lines which contain both of the strings
		"abc" and "def"
>
	:let pat= LogiPat('!"abc"')
<		will return the regular expression which will match
		all lines not containing "abc".  The double quotes
		are needed to pass normal patterns to LogiPat, and
		differentiate such patterns from boolean logic
		operators.


==============================================================================
4. Caveat						*logipat-caveat*

	The "not" operator may be fragile; ie. it may not always play well
	with the & (logical-and) and | (logical-or) operators.  Please try out
	your patterns, possibly with :set hls, to insure that what is matching
	is what you want.

==============================================================================
3. LogiPat History					*logipat-history*

	v1 May 23, 2005	* initial release

==============================================================================
vim:tw=78:ts=8:ft=help
