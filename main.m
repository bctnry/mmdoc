:- module main.

:- interface.

:- import_module io.

:- pred main(io.state::di, io.state::uo) is cc_multi.

:- implementation.

:- import_module bool, list, string, char, exception, maybe.
:- import_module mercury_term_lexer, int.

% removes linefeed at the end of a string if it presents.
:- func rmnl(string) = string.
:- pred rmnl(string::in, string::out) is det.
rmnl(S) = X :- rmnl(S, X).
rmnl(S, X) :- X = remove_suffix_if_present("\n", S).

:- type decl
   ---> decl(kind :: string,
			 name :: string,
			 body :: list(string)
			).

:- type doc_section
   ---> decl_with_doc(decl, list(string))
   ;    doc_only(list(string))
   ;    decl_only(decl)
   ;    submodule_doc(string, list(doc_section))
   ;    hr
   ;    grand_hr  % per stdlib convention, two hr
.

:- type doc
   ---> doc(title :: string,
			body :: list(doc_section)
		   )
.

:- type decl_outline
   ---> outline_item(decl)
   ;    submodule_outline(string, list(decl_outline))
.

%% retrieve the decl outline for the input module.
%% the outline is used for the rendering of the side bar.
:- func get_decl_outline(list(doc_section)) = list(decl_outline).
:- pred get_decl_outline(list(doc_section)::in, list(decl_outline)::out) is det.
get_decl_outline(S) = X :- get_decl_outline(S, X).
get_decl_outline([], []).
get_decl_outline([X|Xs], R) :-
	get_decl_outline(Xs, Rs),
	( ( X = submodule_doc(Name, SubMod),
		get_decl_outline(SubMod, SubModRes),
		R = [submodule_outline(Name, SubModRes)|Rs] );
	  ( X = decl_with_doc(Decl, _), R = [outline_item(Decl) | Rs] );
	  ( X = decl_only(Decl), R = [outline_item(Decl) | Rs] );
	  ( X = doc_only(_), R = Rs );
	  ( X = hr, R = Rs );
	  ( X = grand_hr, R = Rs )
	).

:- func get_decl_outline_from_doc(doc) = list(decl_outline).
:- pred get_decl_outline_from_doc(doc::in, list(decl_outline)::out) is det.
get_decl_outline_from_doc(doc(_, Body)) = X :- X = get_decl_outline(Body).
get_decl_outline_from_doc(doc(_, Body), R) :- get_decl_outline(Body, R).

:- func render_doc_section(doc_section) = string.
:- pred render_doc_section(doc_section::in, string::out) is det.
render_doc_section(S) = X :- render_doc_section(S, X).
render_doc_section(S, X) :- render_doc_section("", S, X).

:- func render_doc_section(string, doc_section) = string.
:- pred render_doc_section(string::in, doc_section::in, string::out) is det.
render_doc_section(ModName, S) = X :- render_doc_section(ModName, S, X).
render_doc_section(ModName, S, R) :-
	(S = hr, R = "<hr />");
	(S = grand_hr, R = "<hr class=\"grand\" />");
	(S = decl_only(Decl),
	 DeclFullName = (Decl ^ kind) ++ "_" ++ (Decl ^ name),
	 ( if ( (Decl ^ kind = "include_module"), ModName \= "" )
	   then (
		   NameLink = (
			   "<a href=\"./" ++ ModName ++ "." ++ (Decl ^ name) ++ ".html\">"
			   ++ (Decl ^ name) ++ "</a>"
		   ),
		   Body = ""
	   )
	   else (
		   NameLink = Decl ^ name,
		   Body = (
			   "<div class=\"decl-body\">"
			   ++ "<pre class=\"decl-source-text\">"
			   ++ join_list("\n", (Decl ^ body))
			   ++ "</pre>"
			   ++ "</div>"
		   )
	   )
	 ),
	 R = (
		 "<div class=\"decl\">"
		 ++ "<div class=\"decl-head\">"
		 ++ "<a class=\"decl-link\" name=\"" ++ DeclFullName ++ "\" href=\"#"  ++ DeclFullName ++ "\">#</a> <a href=\"#top\">^</a>"
		 ++ "<span class=\"decl-type\">" ++ (Decl ^ kind) ++ "</span> "
		 ++ "<span class=\"decl-name\">" ++ NameLink ++ "</span>"
		 ++ "</div>"
		 ++ Body
		 ++ "</div>"
	 )
	);
	(S = doc_only(L),
	 R = "<div class=\"doc\">" ++ join_list("<br />\n", L) ++ "</div>");
	(S = decl_with_doc(Decl, Doc),
	 DeclFullName = (Decl ^ kind) ++ "_" ++ (Decl ^ name),
	 R = (
		 "<div class=\"decl\">"
		 ++ "<div class=\"decl-head\">"
		 ++ "<a class=\"decl-link\" name=\"" ++ DeclFullName ++ "\" href=\"#"  ++ DeclFullName ++ "\">#</a> <a href=\"#top\">^</a>"
		 ++ "<span class=\"decl-type\">" ++ (Decl ^ kind) ++ "</span> "
		 ++ "<span class=\"decl-name\">" ++ (Decl ^ name) ++ "</span>"
		 ++ "</div>"
	     ++ "<div class=\"decl-body\">"
		 ++ "<pre class=\"decl-source-text\">"
		 ++ join_list("\n", (Decl ^ body))
		 ++ "</pre>"
		 ++ "<div class=\"decl-doc\">"
		 ++ join_list("<br />\n", Doc)
	     ++ "</div>"
	     ++ "</div>"
		 ++ "</div>"
	 )
	);
	(S = submodule_doc(SubModName, SubModDoc),
	 R = (
		 "<div class=\"submodule\">"
		 ++ "<div class=\"submodule-header\">" ++ SubModName ++ "</div>"
	     ++ render_doc_section_list(SubModName, SubModDoc)
		 ++ "</div>"
	 )
	).

:- func render_decl_outline_item(decl_outline) = string.
:- pred render_decl_outline_item(decl_outline::in, string::out) is det.
render_decl_outline_item(S) = X :- render_decl_outline_item(S, X).
render_decl_outline_item(outline_item(Decl), X) :-
	DeclFullName = (Decl ^ kind) ++ "_" ++ (Decl ^ name),
	X = (
		"<div class=\"decl-outline-item\"><a class=\"decl-outline-item-link\" "
		++ "href=\"#" ++ DeclFullName ++ "\">"
		++ (Decl ^ kind) ++ " " ++ (Decl ^ name) ++ "</a></div>"
	).
render_decl_outline_item(submodule_outline(Name, DeclList), X) :-
	X = (
		"<div class=\"decl-outline-submodule\">"
		++ "<div class=\"decl-outline-submodule-header\">"
		++ Name
		++ "</div>"
		++ "<div class=\"decl-outline-submodule-list\">"
		++ join_list("\n", map(render_decl_outline_item, DeclList))
		++ "</div>"
		++ "</div>"
	).

% "decl list" means the outline list here.
:- func render_decl_list(list(decl_outline)) = string.
:- pred render_decl_list(list(decl_outline)::in, string::out) is det.
render_decl_list(S) = X :- render_decl_list(S, X).
render_decl_list(S, R) :-
	R = (
		"<div class=\"decl-list\">"
		++ join_list("\n", map(render_decl_outline_item, S))
		++ "</div>"
	).

:- pred is_comment(string::in) is semidet.
is_comment(S) :- prefix(string.lstrip(S), "%").

:- pred char_eq(char::in, char::in) is semidet.
char_eq(C0, C) :- C0 = C.

:- pred is_hr(string::in) is semidet.
is_hr(S) :-
	Stripped = strip(S),
	prefix(Stripped, "%"), suffix(Stripped, "%"),
	Z = remove_suffix_if_present(
			remove_prefix_if_present(Stripped, "%"),
			"%"
		),
	prefix_length(char_eq('-'), Z) = length(Z).

%% decl doc, the doc comment format used in stdlib. the difference
%% being starting with spaces and coming immediately before a decl.
:- pred is_decl_doc(string::in) is semidet.
is_decl_doc(S) :- is_strictly_decl_doc(S); is_mmdoc_line(S).
:- pred is_strictly_decl_doc(string::in) is semidet.
is_strictly_decl_doc(S) :- prefix(S, "    %").

%% normal doc is just "normal", with no space prefix. used in stdlib
%% at the beginning of modules as a kind of introduction. we should
%% know that in stdlib there's no conscious doc comment vs. normal
%% comment separation. maybe they've decided it's not aligning with
%% the mercury philosophy.
:- pred is_normal_doc(string::in) is semidet.
is_normal_doc(S) :- is_strictly_normal_doc(S); is_mmdoc_line(S).
:- pred is_strictly_normal_doc(string::in) is semidet.
is_strictly_normal_doc(S) :- prefix(S, "%"), not is_hr(S).

%% this is our own convention - comment lines that starts with
%% two %s is considered a part of a doc comment. they can be used
%% in both normal docs and accompanying docs.
:- pred is_mmdoc_line(string::in) is semidet.
is_mmdoc_line(S) :- prefix(lstrip(S), "%%").

:- pred is_empty(string::in) is semidet.
is_empty(S) :- strip(S) = "".
:- pred is_not_empty(string::in) is semidet.
is_not_empty(S) :- strip(S) \= "".

%% grand_hr, i.e. two hrs in a row. used in stdlib to visually
%% separate sections.
:- pred has_grand_hr(list(string)::in) is semidet.
has_grand_hr([X1, X2|_]) :- is_hr(X1), is_hr(X2).

:- pred has_hr(list(string)::in) is semidet.
has_hr([X|_]) :- is_hr(X).

:- pred is_decl(string::in) is semidet.
is_decl(X) :- prefix(lstrip(X):string, ":-").

:- pred is_non_empty_common_line(string::in) is semidet.
is_non_empty_common_line(S) :-
	is_not_empty(S), not is_comment(S), not is_decl(S).

:- pred take_decl(list(string), list(string), list(string)).
:- mode take_decl(in, out, out) is det.
take_decl([], [], []).
take_decl([X|Xs], D, R) :-
	if is_decl(X)
	then (
		take_while(is_non_empty_common_line, Xs, Ds, Rs),
		map(rmnl, [X|Ds], D), R = Rs
	)
	else (D = [], R = [X|Xs]).

:- pred remove_normal_doc_prefix(string::in, string::out) is det.
remove_normal_doc_prefix(S, Out) :-
	S0 = rmnl(S),
	( if ( is_mmdoc_line(S0) )
	  then ( det_remove_prefix("%%", S0, Out) )
	  else if ( is_strictly_normal_doc(S0) )
	  then ( det_remove_prefix("%", S0, Out) )
	  else ( throw("invalid value: normal doc line does not have required prefix.") )
	).

:- pred take_normal_doc(list(string), list(string), list(string)).
:- mode take_normal_doc(in, out, out) is det.
take_normal_doc(X, D, R) :-
	take_while(is_normal_doc, X, Ds, R),
    map(remove_normal_doc_prefix, Ds, D).

:- pred remove_decl_doc_prefix(string::in, string::out) is det.
remove_decl_doc_prefix(S, Out) :-
	S0 = rmnl(S),
	( if is_strictly_decl_doc(S0)
	  then det_remove_prefix("    %", S0, Out)
	  else (
		  if is_mmdoc_line(S0)
	      then det_remove_prefix("%%", S0, Out)
	      else throw("invalid value: decl doc line does not have required prefix.")
	  )
	).

:- pred take_decl_doc(list(string), list(string), list(string)).
:- mode take_decl_doc(in, out, out) is det.
take_decl_doc(X, D, R) :-
	take_while(is_decl_doc, X, Ds, R),
	map(remove_decl_doc_prefix, Ds, D).

%% a line is not important if it's neither a decl nor a comment.
:- pred not_important(string::in) is semidet.
not_important(S) :-
	not is_mmdoc_line(S),
	not is_strictly_decl_doc(S),
	not is_strictly_normal_doc(S),
	not is_decl(S).
	
:- pred skip_not_important(list(string), list(string)).
:- mode skip_not_important(in, out) is det.
skip_not_important(X, R) :-
	drop_while(not_important, X, R).

:- pred parse_grand_hr(list(string), doc_section, list(string)).
:- mode parse_grand_hr(in, out, out) is semidet.
parse_grand_hr(S, R, Next) :-
	has_grand_hr(S), S = [_, _|Next], R = grand_hr.

:- pred parse_hr(list(string), doc_section, list(string)).
:- mode parse_hr(in, out, out) is semidet.
parse_hr([X|Next], R, Next) :-
	is_hr(X), R = hr.


:- pred parse_normal_doc(list(string), doc_section, list(string)).
:- mode parse_normal_doc(in, out, out) is semidet.
parse_normal_doc(S, R, Next) :-
	take_normal_doc(S, Doc, Rest),
	Doc \= [], Next = Rest, R = doc_only(Doc).

:- func parse_include_module(string) = string.
parse_include_module(S) = X :-
	R0 = remove_prefix_if_present(":-", strip(S)),
	R1 = remove_prefix_if_present("include_module", strip(R0)),
	R2 = remove_suffix_if_present(".", R1),
	X = strip(R2).

:- func parse_module_decl(string) = string.
parse_module_decl(S) = X :-
	R0 = remove_prefix_if_present(":-", strip(S)),
	R1 = remove_prefix_if_present("module", strip(R0)),
	R2 = remove_suffix_if_present(".", R1),
	X = strip(R2).

:- pred read_decl_name_type(string, decl).
:- mode read_decl_name_type(in, out) is cc_multi.
read_decl_name_type(S, Res) :-
	S0 = lstrip(remove_prefix_if_present(":-", lstrip(S))),
	string_get_token_list(S0, TokenList, posn(0,0,0), _),
	( ( TokenList = token_cons(name(X), _, token_cons(name(Y), _, _)),
		( ( X = "include_module",
			Res = decl("include_module", parse_include_module(S0), []) );
		  ( X = "module",
			Res = decl("module", parse_module_decl(S0), []) );
		  Res = decl(X, Y, []) )
	  );
	  ( TokenList = token_cons(name(X), _, _),
		( ( X = "implementation",
			Res = decl("implementation", "", []) );
		  ( X = "interface",
			Res = decl("interface", "", []) );
		  ( Res = decl(X, "", []) )
		)
	  );
	  ( TokenList = _, Res = decl("", "", []) )
	).

:- pred parse_decl_doc(list(string), doc_section, list(string)).
:- mode parse_decl_doc(in, out, out) is cc_multi.
parse_decl_doc(S, R, Next) :-
	take_decl_doc(S, Doc, Rest0),
	( ( Doc = [],  % no doc, maybe only decl?
		take_decl(S, Decl, Rest1),
		( ( Decl = [],  % no decl also...
			fail );
		  ( Decl = [DeclFirstLine | _],
			read_decl_name_type(DeclFirstLine, decl(Kind, Name, _)),
			Next = Rest1, R = decl_only(decl(Kind, Name, Decl))
		  )  % decl only.
		)
	  );
	  ( take_decl(Rest0, Decl, Rest1),
		( ( Decl = [], % has doc but no decl...
			Next = Rest1, R = doc_only(Doc) );
		  ( Decl = [DeclFirstLine | _],
			read_decl_name_type(DeclFirstLine, decl(Kind, Name, _)),
			Next = Rest1, R = decl_with_doc(decl(Kind, Name, Decl), Doc)
		  )
		)
	  )
	).

:- pred parse_doc_section(list(string), maybe(doc_section), list(string)).
:- mode parse_doc_section(in, out, out) is cc_multi.
parse_doc_section([], no, []).
parse_doc_section([X|Xs], RR, Next) :-
	S = [X|Xs],
	( ( parse_grand_hr(S, R, Next) );
	  ( parse_hr(S, R, Next) );
	  ( parse_normal_doc(S, R, Next) );
	  ( parse_decl_doc(S, R, Next ) )
	),
	RR = yes(R).

:- pred parse_doc_section_with_skip(list(string), maybe(doc_section), list(string)).
:- mode parse_doc_section_with_skip(in, out, out) is cc_multi.
parse_doc_section_with_skip([], no, []).
parse_doc_section_with_skip([X|Xs], R, Next) :-
	skip_not_important([X|Xs], Skipped1),
	parse_doc_section(Skipped1, R, Rest1),
	skip_not_important(Rest1, Next).

:- pred parse_doc_section_list(list(string), list(doc_section)).
:- mode parse_doc_section_list(in, out) is cc_multi.
parse_doc_section_list([], []).
parse_doc_section_list(S, Res) :-
	parse_doc_section_with_skip(S, R0, Next0),
	( ( R0 = no, Res = [] );
	  ( R0 = yes(R),
		parse_doc_section_list(Next0, Rs),
		Res = [R|Rs] )
	).

:- pred is_submodule_decl(decl::in) is semidet.
is_submodule_decl(decl(Type, _, _)) :- Type = "module".

:- pred take_submodule_acc(
	   list(doc_section)::in,
	   int::in,
	   list(doc_section)::in,
	   list(doc_section)::out,
	   list(doc_section)::out
   ) is det.
take_submodule_acc(S, Layer, Acc, Res, Rest) :-
	( ( S = [], Res = reverse(Acc), Rest = [] );
	  ( S = [X|Xs],
		( if ( Layer < 0 )
		  then ( Res = reverse(Acc), Rest = S )
		  else (
			  ( X = decl_only(decl(Kind, _, _)),
				( if ( Kind = "module" )
				  then ( take_submodule_acc(Xs, Layer+1, [X|Acc], Res, Rest) )
				  else if ( Kind = "end_module" )
				  then ( take_submodule_acc(Xs, Layer-1, [X|Acc], Res, Rest) )
				  else ( take_submodule_acc(Xs, Layer, [X|Acc], Res, Rest) )
				)
			  );
			  ( X = decl_with_doc(decl(Kind, _, _), _),
				( if ( Kind = "module" )
				  then ( take_submodule_acc(Xs, Layer+1, [X|Acc], Res, Rest) )
				  else if ( Kind = "end_module" )
				  then ( take_submodule_acc(Xs, Layer-1, [X|Acc], Res, Rest) )
				  else ( take_submodule_acc(Xs, Layer, [X|Acc], Res, Rest) )
				)
			  );
			  ( ( X = hr; X = grand_hr; X = doc_only(_); X = submodule_doc(_, _) ),
				take_submodule_acc(Xs, Layer, [X|Acc], Res, Rest ) )
		  )
		)
	  )
	).

:- pred take_submodule_doc_section_list(
	   list(doc_section)::in,
	   doc_section::out,
	   list(doc_section)::out) is semidet.
take_submodule_doc_section_list(S, SubMod, Rest) :-
	( ( S = [decl_only(decl(Kind, Name, _))|R] );
	  ( S = [decl_with_doc(decl(Kind, Name, _), _)|R] ) ),
	Kind = "module",
	take_submodule_acc(R, 0, [], T, L),
	restruct_submod(T, SubModList),
	Filtered = negated_filter(
				   (pred(K::in) is semidet :-
						( ( K = decl_only(decl(KK, _, _));
							K = decl_with_doc(decl(KK, _, _), _) ),
						  ( KK = "interface"; KK = "end_module" )
						)
				   ),
				   SubModList
			   ),
	SubMod = submodule_doc(Name, Filtered),
	Rest = L.

:- pred restruct_submod(list(doc_section), list(doc_section)).
:- mode restruct_submod(in, out) is det.
restruct_submod(S, R) :-
	( ( S = [], R = [] );
	  ( S = [_|_],
		( if ( take_submodule_doc_section_list(S, Rs, Rr) )
		  then ( restruct_submod(Rr, RrRes), R = [Rs|RrRes] )
		  else ( S = [Rs|Rr], restruct_submod(Rr, RrRes), R = [Rs|RrRes] )
		)
	  )
	).

:- pred find_module_name(list(doc_section)::in, string::out) is cc_multi.
find_module_name(S, R) :-
	( ( S = [], R = "" );
	  ( S = [X|Xs],
		( ( ( X = decl_only(decl(Kind, Name, _));
			  X = decl_with_doc(decl(Kind, Name, _), _) ),
			( if ( Kind = "module" )
			  then ( R = Name )
			  else ( find_module_name(Xs, R) )
			)
		  );
		  ( find_module_name(Xs, R) )
		)
	  )
	).

:- pred skip_after_implementation(list(doc_section)::in, list(doc_section)::out) is det.
skip_after_implementation(S, R) :-
	( ( S = [], R = [] );
	  ( S = [X|Xs],
		( ( ( X = decl_only(decl(Kind, _, _));
			  X = decl_with_doc(decl(Kind, _, _), _) ),
			( if ( Kind = "implementation" )
			  then ( R = [] )
			  else ( skip_after_implementation(Xs, Rs),
					 R = [X|Rs] )
			)
		  );
		  ( ( X = hr; X = grand_hr; X = doc_only(_) ),
			skip_after_implementation(Xs, Rs),
			R = [X|Rs]
		  );
		  ( X = submodule_doc(Name, SubModBody),
			skip_after_implementation(SubModBody, NewSMB),
			skip_after_implementation(Xs, Rs),
			R = [submodule_doc(Name, NewSMB)|Rs]
		  )
		)
	  )
	).

:- pred parse_doc(list(string), doc).
:- mode parse_doc(in, out) is cc_multi.
parse_doc(S, Res) :-
	parse_doc_section_list(S, L),
	restruct_submod(L, RL),
	skip_after_implementation(RL, RRL),
	find_module_name(RRL, Name),
	Res = doc(Name, RRL).

:- pred read_all_lines(
	   io.text_input_stream::in,
	   res(list(string))::out,
	   io::di,
	   io::uo) is det.
read_all_lines(S, Lines, !IO) :-
	read_line_as_string(S, R, !IO),
	( ( R = eof, Lines = ok([]) );
	  ( R = error(Err), Lines = error(Err) );
	  ( R = ok(Line),
		read_all_lines(S, ResRest, !IO),
		( ( ResRest = error(Err), Lines = error(Err) );
		  ( ResRest = ok(Rest),
			Lines = ok([Line | Rest])
		  )
		)
	  )
	).

:- func render_doc_section_list(string, list(doc_section)) = string.
:- pred render_doc_section_list(string, list(doc_section), string).
:- mode render_doc_section_list(in, in, out) is det.
render_doc_section_list(ModName, S) = X :- render_doc_section_list(ModName, S, X).
render_doc_section_list(ModName, S, X) :-
	map(render_doc_section(ModName), S, Ss),
	X = join_list("\n", Ss).

:- func render_doc(doc) = string.
:- pred render_doc(doc::in, string::out) is det.
render_doc(S) = X :- render_doc(S, X).
render_doc(S, X) :-
	Decls = get_decl_outline_from_doc(S),
	DeclListStr = render_decl_list(Decls),
	render_doc_section_list(S ^ title, S ^ body, BodyRes),
	X = (
		"<!DOCTYPE html>"
		++ "<html>"
		++ "<head><title>" ++ (S ^ title) ++ "</title>"
		++ "<style>"

        ++ "* { box-sizing: border-box; }\n"
		++ "a { color: white; }\n"
        ++ "body { background-color: #1f1f1f; color: white; padding: 1em; }\n"
		++ "main { display: flex; }\n"
		++ ".decl { margin-top: 2rem; margin-bottom: 1em; }\n"
		++ ".decl-head a { color: white; margin-right: 0.5em; }\n"
        ++ ".decl-head { font-size: 1em; background-color: #3f3f3f; font-weight: bold; padding: 0.2em; padding-left: 0.5em; }\n"
		++ ".decl-body { font-size: 1em; margin-left: 2em; }\n"
		++ ".decl-list { margin: 1em; background-color: #1f1f1f; } "
		++ ".decl-list a { color: white; }\n"
		++ ".doc-main { margin: 1em; }\n"
	    ++ ".submodule { border-top: 2px white solid; border-bottom: 2px white solid; }\n"
		++ ".submodule-header { font-size: 2rem; font-weight: bold; }\n"
		++ ".decl-outline-submodule-list { margin-left: 1em; }\n"
		++ ".doc-main { flex-grow: 2; }\n"
	    ++ "</style></head>"
	    ++ "<body><main>"
		++ DeclListStr
		++ "<div class=\"doc-main\"><a name=\"top\"></a>" ++ BodyRes ++ "</div>"
		++ "</main>"
		++ "<hr />"
		++ "<div class=\"footer\">Generated with <a href=\"https://github.com/bctnry/mmdoc\">mmdoc</a>.</div>"
		++ "</body>"
		++ "</html>"
	).
				
:- pred read_and_parse(string::in, doc::out, io::di, io::uo) is cc_multi.
read_and_parse(FilePath, Doc, !IO) :-
	open_input(FilePath, RIn, !IO),
	( ( RIn = error(Err), throw(error_message(Err):string) );
	  ( RIn = ok(InStream),
		read_all_lines(InStream, ReadRes, !IO),
		close_input(InStream, !IO),
		( ( ReadRes = error(Err), throw(error_message(Err):string) );
		  ( ReadRes = ok(Lines),
			parse_doc(Lines, ParseRes),
			Doc = ParseRes
		  )
		)
	  )
	).

:- pred handle(list(string)::in, text_output_stream::in, io::di, io::uo) is cc_multi.
handle(Lines, OutStream, !IO) :-
	parse_doc(Lines, Res),
	render_doc(Res, RenderedRes),
	write_string(OutStream, RenderedRes, !IO).

main(!IO) :-
	command_line_arguments(Argv, !IO),
	( if ( length(Argv) = 2 )
	  then (
		  det_index0(Argv, 0, InputFileName),
		  det_index0(Argv, 1, OutputFileName),
		  open_input(InputFileName, RIn, !IO),
		  ( ( RIn = error(Err), error_message(Err, ErrMsg), throw(ErrMsg) );
			( RIn = ok(InStream),
			  read_all_lines(InStream, ReadRes, !IO),
			  ( ( ReadRes = error(Err), throw(error_message(Err):string) );
				( ReadRes = ok(Lines),
				  open_output(OutputFileName, ROut, !IO),
				  ( ( ROut = error(Err), throw(error_message(Err):string) );
					( ROut = ok(OutStream),
					  handle(Lines, OutStream, !IO),
					  close_output(OutStream, !IO)
					)
				  )
				)
			  ),
			  close_input(InStream, !IO)
			)
		  )
	  )
	  else (
		  write_string("Usage: mmdoc [input] [output]\n", !IO)
	  )
	).


