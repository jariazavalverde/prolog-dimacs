/**
  * 
  * FILENAME: dimacs_cnf.pl
  * DESCRIPTION: This module contains predicates for parsing CNF problems in DIMACS format.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-dimacs
  * UPDATED: 09.12.2019
  * 
  **/

:- module(dimacs_cnf, [
	dimacs_cnf_parse/4,
	dimacs_cnf_read/4
]).



% PUBLIC PREDICATES

% dimacs_cnf_parse/4
% dimacs_cnf_parse(+Chars, ?Nvars, ?Nclauses, ?Clauses)
%
% dimacs_cnf_read(Chars, Nvars, Nclauses, Clauses) succeeds when Chars is a list
% of characters describing a cnf problem Clauses with Nvars variables and Nclauses clauses.
dimacs_cnf_parse(Chars, Nvars, Nclauses, Clauses) :-
	dimacs_cnf(Nvars, Nclauses, Clauses, Chars, []).

% dimacs_cnf_read/4
% dimacs_cnf_read(+Path, ?Nvars, ?Nclauses, ?Clauses)
%
% dimacs_cnf_read(Path, Nvars, Nclauses, Clauses) succeeds when Path is a file
% describing a cnf problem Clauses with Nvars variables and Nclauses clauses.
dimacs_cnf_read(File, Nvars, Nclauses, Clauses) :-
	open(File, read, Stream),
	stream_to_list(Stream, Chars),
	close(Stream),
	dimacs_cnf_parse(Chars, Nvars, Nclauses, Clauses).



% UTILS

% stream_to_list/2
% stream_to_list(+Stream, ?Chars)
%
% stream_to_list(Stream, Chars) succeeds when Chars is a list of characters
% taken from Stream.
stream_to_list(Stream, []) :-
	at_end_of_stream(Stream), !.
stream_to_list(Stream, [Char|Input]) :-
	get_code(Stream, Code),
	char_code(Char, Code),
	stream_to_list(Stream, Input).



% GRAMMAR

% dimacs_cnf_whitespace/2, dimacs_cnf_whitespaces/2, dimacs_cnf_comment_line/2
% Parses whitespaces and comment lines.
dimacs_cnf_whitespace --> [' '].
dimacs_cnf_whitespace --> ['\n'].
dimacs_cnf_whitespace --> ['\t'].
dimacs_cnf_whitespace --> [c, '\n'].
dimacs_cnf_whitespace --> [c, ' '], dimacs_cnf_comment_line.
dimacs_cnf_whitespaces --> dimacs_cnf_whitespace, !, dimacs_cnf_whitespaces.
dimacs_cnf_whitespaces --> [].
dimacs_cnf_comment_line --> [X], {X \= '\n'}, !, dimacs_cnf_comment_line.
dimacs_cnf_comment_line --> [].

% dimacs_cnf_digits/3
% Parses a sequence of digits.
dimacs_cnf_digits([X|Xs]) --> [X], {char_code(X, C), C >= 48, C =< 57}, !, dimacs_cnf_digits(Xs).
dimacs_cnf_digits([]) --> [].

% dimacs_cnf_natural/3
% Parses a non-empty sequence of digits.
% Returns an integer.
dimacs_cnf_natural(N) --> dimacs_cnf_digits(Xs), {Xs \= [], number_chars(N, Xs)}.

% dimacs_cnf_literal/3
% Parses a literal (a natural number or the negation of a natural number).
% Returns a literal.
dimacs_cnf_literal(L) --> dimacs_cnf_natural(L), !.
dimacs_cnf_literal(not(L)) --> [-], dimacs_cnf_natural(L).

% dimacs_cnf_program_line/4
% Parses the program line.
% Returns the number of variables and the number of clauses.
dimacs_cnf_program_line(Nvars, Nclauses) -->
    dimacs_cnf_whitespaces, [p],
    dimacs_cnf_whitespaces, [c,n,f],
    dimacs_cnf_whitespaces, dimacs_cnf_natural(Nvars),
    dimacs_cnf_whitespaces, dimacs_cnf_natural(Nclauses).

% dimacs_cnf_clause/3
% Parses a clause (a sequence of literals ending by 0)
% Returns a list of literals.
dimacs_cnf_clause([X|Xs]) -->
    dimacs_cnf_whitespaces,
    dimacs_cnf_literal(X), {X \= 0}, !,
    dimacs_cnf_clause(Xs).
dimacs_cnf_clause([]) --> dimacs_cnf_whitespaces, dimacs_cnf_literal(0).

% dimacs_cnf_clauses/3
% Parses a sequence of clauses.
% Returns a list of clauses.
dimacs_cnf_clauses([X|Xs]) --> dimacs_cnf_clause(X), !, dimacs_cnf_clauses(Xs).
dimacs_cnf_clauses([]) --> [].

% dimacs_cnf/5
% Parses a cnf file.
% Returns the number of variables, the number of clauses and the list of clauses.
dimacs_cnf(Nvars, Nclauses, Clauses) -->
    dimacs_cnf_program_line(Nvars, Nclauses),
    dimacs_cnf_clauses(Clauses),
    dimacs_cnf_whitespaces.