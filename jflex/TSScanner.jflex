package Example;

import java_cup.runtime.SymbolFactory;
%%
%cup
%class TSScanner
%{
	public TSScanner(java.io.InputStream r, SymbolFactory sf){
		this(r);
		this.sf=sf;
	}
	private SymbolFactory sf;
%}
%eofval{
    return sf.newSymbol("EOF",sym.EOF);
%eofval}
%char
%line
%column

DIGIT    =	[0-9]
INTEGER  =	{DIGIT}+
FLOAT    =	({INTEGER}\.{INTEGER})|({INTEGER}(\.{INTEGER})?[eE][+-]?{INTEGER})|(\.{INTEGER})|((\.{INTEGER})?[eE][+-]?{INTEGER})
LETTER   =	[A-Za-z_]
FILECHAR =	[A-Za-z_\.]
VARMID   =	[:A-Za-z0-9_]
IDTAIL   =	[A-Za-z0-9_]
VARTAIL  =	{VARMID}*{IDTAIL}
VAR      =	[$%]{LETTER}{VARTAIL}*
ID       =	{LETTER}{IDTAIL}*
ILID     =	[$%]{DIGIT}+{LETTER}{VARTAIL}*
FILENAME =	{FILECHAR}+
SPACE    =	[ \t\v\f]
HEXDIGIT =	[a-fA-F0-9]

%%



";" { return sf.newSymbol("Semi",sym.SEMI); }
"+" { return sf.newSymbol("Plus",sym.PLUS); }
"*" { return sf.newSymbol("Times",sym.TIMES); }
"(" { return sf.newSymbol("Left Bracket",sym.LPAREN); }
")" { return sf.newSymbol("Right Bracket",sym.RPAREN); }
[0-9]+ { return sf.newSymbol("Integral Number",sym.NUMBER, new Integer(yytext())); }
[ \t\r\n\f] { /* ignore white space. */ }
. { System.err.println("Illegal character: "+yytext()); }



         ;
{SPACE}+ { }
"//"[^\n\r]*   ;
[\r]        ;
[\n]        {lineIndex++;}
/* 
\"(\\.|[^\\"\n\r])*\"      { return sf.newSymbol(Sc_ScanString(STRATOM)); }
\'(\\.|[^\\'\n\r])*\'      { return(Sc_ScanString(TAGATOM)); }
*/

<STRING> {
  \"                             { yybegin(YYINITIAL); 
                                   return sf.newSymbol(sym.STRATOM, 
                                   string.toString()); }
  [^\n\r\"\\]+                   { string.append( yytext() ); }
  \\t                            { string.append('\t'); }
  \\n                            { string.append('\n'); }

  \\r                            { string.append('\r'); }
  \\\"                           { string.append('\"'); }
  \\                             { string.append('\\'); }
}

"=="     {   return(CMDlval.i = opEQ); }
"!="     {   return(CMDlval.i = opNE);
">="     {   return(CMDlval.i = opGE);
"<="     {   return(CMDlval.i = opLE);
"&&"     {   return(CMDlval.i = opAND);
"||"     {   return(CMDlval.i = opOR);
"::"     {   return(CMDlval.i = opCOLONCOLON);
"--"     {   return(CMDlval.i = opMINUSMINUS);
"++"     {   return(CMDlval.i = opPLUSPLUS);
"$="     {   return(CMDlval.i = opSTREQ);
"!$="    {   return(CMDlval.i = opSTRNE);
"<<"     {   return(CMDlval.i = opSHL);
">>"     {   return(CMDlval.i = opSHR);
"+="     {   return(CMDlval.i = opPLASN);
"-="     {   return(CMDlval.i = opMIASN);
"*="     {   return(CMDlval.i = opMLASN);
"/="     {   return(CMDlval.i = opDVASN);
"%="     {   return(CMDlval.i = opMODASN);
"&="     {   return(CMDlval.i = opANDASN);
"^="     {   return(CMDlval.i = opXORASN);
"|="     {   return(CMDlval.i = opORASN);
"<<="    {   return(CMDlval.i = opSLASN);
">>="       return(CMDlval.i = opSRASN);
"NL"        {CMDlval.i = '\n'; return '@'; }
"TAB"       {CMDlval.i = '\t'; return '@'; }
"SPC"       {CMDlval.i = ' '; return '@'; }
"@"         {CMDlval.i = 0; return '@'; }
"/*"		{
         register int c = 0, l;
				for ( ; ; )
				{
            l = c;
            c = yyinput();

            // Is this an open comment?
            if ( c == EOF )
					{
               CMDerror( "unexpected end of file found in comment" );
               break;
					}

            // Increment line numbers.
            else if ( c == '\n' )
               lineIndex++;

            // Did we find the end of the comment?
            else if ( l == '*' && c == '/' )
						break;
					}
				}
"?" |
"[" |
"]" |
"(" | 
")" | 
"+" | 
"-" | 
"*" | 
"/" |
"<" | 
">" | 
"|" | 
"." | 
"!" |
":" | 
";" |
"{" | 
"}" | 
"," |
"&" |
"%" |
"^" |
"~" |
"=" {       return(CMDlval.i = CMDtext[0]); }
"or"        { CMDlval.i = lineIndex; return(rwCASEOR); }
"break"     { CMDlval.i = lineIndex; return(rwBREAK); }
"return"    { CMDlval.i = lineIndex; return(rwRETURN); }
"else"      { CMDlval.i = lineIndex; return(rwELSE); }
"while"     { CMDlval.i = lineIndex; return(rwWHILE); }
"do"        { CMDlval.i = lineIndex; return(rwDO); }
"if"        { CMDlval.i = lineIndex; return(rwIF); }
"for"       { CMDlval.i = lineIndex; return(rwFOR); }
"continue"  { CMDlval.i = lineIndex; return(rwCONTINUE); }
"function"  { CMDlval.i = lineIndex; return(rwDEFINE); }
"new"       { CMDlval.i = lineIndex; return(rwDECLARE); }
"datablock" { CMDlval.i = lineIndex; return(rwDATABLOCK); }
"case"      { CMDlval.i = lineIndex; return(rwCASE); }
"switch$"   { CMDlval.i = lineIndex; return(rwSWITCHSTR); }
"switch"    { CMDlval.i = lineIndex; return(rwSWITCH); }
"default"   { CMDlval.i = lineIndex; return(rwDEFAULT); }
"package"   { CMDlval.i = lineIndex; return(rwPACKAGE); }
"namespace" { CMDlval.i = lineIndex; return(rwNAMESPACE); }
"true"      { CMDlval.i = 1; return INTCONST; }
"false"     { CMDlval.i = 0; return INTCONST; }
{VAR}       return(Sc_ScanVar());
{ID}        { CMDtext[CMDleng] = 0; CMDlval.s = StringTable->insert(CMDtext); return(IDENT); }
0[xX]{HEXDIGIT}+ return(Sc_ScanHex());
{INTEGER}   { CMDtext[CMDleng] = 0; CMDlval.i = dAtoi(CMDtext); return INTCONST; }
{FLOAT}     return Sc_ScanNum();
{ILID}      return(ILLEGAL_TOKEN);
.           return(ILLEGAL_TOKEN);
