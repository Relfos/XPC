ANYCHAR	[ \t_a-zA-Z0-9:;!#.,=&*@><\\@$\[\]\^+\-]

NL (\r|\n|\r\n)	

%s INITIAL XCOMMENT1 XCOMMENT2 XNOTDEF


%%


"//".*				Begin End;
\{					Begin yypushstate(XCOMMENT1); End;
\(\*				Begin yypushstate(XCOMMENT2); End;

<XCOMMENT1>\}		Begin yypopstate(); End;
<XCOMMENT2>\*\)		Begin yypopstate(); End;

<XCOMMENT1>{ANYCHAR}		Begin End;
<XCOMMENT1>[*'')]			Begin End;
<XCOMMENT1>{NL}				Begin CheckSourceNewline(); End;
<XCOMMENT1>.				Begin End;


<XCOMMENT2>{ANYCHAR}		Begin End;
<XCOMMENT2>[*'')]			Begin End;
<XCOMMENT2>{NL}				Begin CheckSourceNewline(); End;
<XCOMMENT2>.				Begin End;

"{$i "[^}]+\}		Begin
						string ifile = GetDirectiveArg("i");
						string inctext = FetchInclude(ifile);
						
						if (inctext == null)
						Begin
                            pperror("Include file " + ifile + " not found");
							break;
						End;

						StringReader sr = new StringReader(inctext);
						yypushStream(sr);
						yydebug("Pushed stream from " + ifile + "");
					End;

	{ defines }
"{$define "[^}]+\}	Begin	AddDefine(GetDirectiveArg("define"));	End;

"{$undef "[^}]+\}	Begin	RemoveDefine(GetDirectiveArg("undef"));	End;

"{$ifdef "[^}]+\}	Begin	
                        if (!IsDefinedDir("ifdef")) Then
                            yypushstate(XNOTDEF);
					End;

"{$ifopt "[^}]+\}	Begin
                        if (!IsDefinedDir("ifopt"))
							yypushstate(XNOTDEF);
					End;

"{$ifndef "[^}]+\}	Begin
                        Boolean defined = IsDefined(GetDirectiveArg("ifndef"));
						defines.Push(!defined);
						if (defined)
							yypushstate(XNOTDEF);
					End;

"{$else"[^}]*\}		Begin
                        // currently in a defined section, switch to non-defined
						defines.Pop();
						defines.Push(false);
						yypushstate(XNOTDEF);
					End;

"{$endif"[^}]*\}	Begin defines.Pop(); End;


"{$"[^}]\}			Begin End;

<XNOTDEF>"//".*				Begin End;
<XNOTDEF>\{					Begin yypushstate(XCOMMENT1); End;
<XNOTDEF>\(\*				Begin yypushstate(XCOMMENT2); End;
 
<XNOTDEF>"{$ifdef "[^}]+\}	Begin defines.Push(true); End;

<XNOTDEF>"{$ifopt "[^}]+\}	Begin defines.Push(true); End;

<XNOTDEF>"{$ifndef "[^}]+\}	Begin defines.Push(true); End;

<XNOTDEF>"{$else"[^}]*\}	Begin	if (defines.Peek() == false)	// at the non-def section start, must leave
                                Begin	yypopstate();
							defines.Pop();
							defines.Push(true);
                                End; // else, leave the top def as true
					End;

<XNOTDEF>"{$endif"[^}]*\}	Begin	Boolean def = defines.Pop(); 
						if (def == false)
							yypopstate();
					End;
 
<XNOTDEF>{ANYCHAR}+			Begin { chomp up as much as possible in one match} End;
<XNOTDEF>{NL}				Begin CheckSourceNewline(); End;
<XNOTDEF>.					Begin  { ignore everything in a non-def section } End;


{ANYCHAR}+			Begin outBuilder.Append(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead); End;

'[^\n\r']*('|{NL})	Begin
                        if (zzBuffer[zzMarkedPos-1] != '\'')	// if last char not quote
							pperror("Unterminated string");
						else
							outBuilder.Append(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
					End;

\/|\(|\)			Begin outBuilder.Append(yycharat(0)); End;
{NL}				Begin CheckSourceNewline(); End;

.					Begin pperror("Unknown char: " + text + " (ASCII " + ((int) text[0]) +")"); End

