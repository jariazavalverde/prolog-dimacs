/**
  * 
  * FILENAME: dimacs_edge.pl
  * DESCRIPTION: This module contains predicates for parsing undirected graphs in DIMACS format.
  * AUTHORS: José Antonio Riaza Valverde <riaza.valverde@gmail.com>
  * GITHUB: https://github.com/jariazavalverde/prolog-dimacs
  * UPDATED: 09.12.2019
  * 
  **/

:- module(dimacs_edge, [
	dimacs_edge_parse/4,
	dimacs_edge_read/4
]).
	


% PUBLIC PREDICATES

% dimacs_edge_parse/4
% dimacs_edge_parse(+Chars, ?Nnodes, ?Nedges, ?Edges)
%
% dimacs_edge_parse(Chars, Nnodes, Nedges, Edges) succeeds when Chars is a list
% of characters describing an undirected graph Edges with Nnodes nodes and Nedges Edges.
dimacs_edge_parse(Chars, Nnodes, Nedges, Edges) :-
	dimacs_edge(Nnodes, Nedges, Edges, Chars, []).

% dimacs_edge_read/4
% dimacs_edge_read(+Path, ?Nnodes, ?Nedges, ?Edges)
%
% dimacs_edge_read(Path, Nnodes, Nedges, Edges) succeeds when Path is a file
% describing an undirected graph Edges with Nnodes nodes and Nedges edges.
dimacs_edge_read(File, Nnodes, Nedges, Edges) :-
	open(File, read, Stream),
	stream_to_list(Stream, Chars),
	close(Stream),
	dimacs_edge_parse(Chars, Nnodes, Nedges, Edges).



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

% dimacs_edge_whitespace/2, dimacs_edge_whitespaces/2, dimacs_edge_comment_line/2
% Parses whitespaces and comment lines.
dimacs_edge_whitespace --> [' '].
dimacs_edge_whitespace --> ['\n'].
dimacs_edge_whitespace --> ['\t'].
dimacs_edge_whitespace --> [c, '\n'].
dimacs_edge_whitespace --> [c, ' '], dimacs_edge_comment_line.
dimacs_edge_whitespaces --> dimacs_edge_whitespace, !, dimacs_edge_whitespaces.
dimacs_edge_whitespaces --> [].
dimacs_edge_comment_line --> [X], {X \= '\n'}, !, dimacs_edge_comment_line.
dimacs_edge_comment_line --> [].

% dimacs_edge_digits/3
% Parses a sequence of digits.
dimacs_edge_digits([X|Xs]) --> [X], {char_code(X, C), C >= 48, C =< 57}, !, dimacs_edge_digits(Xs).
dimacs_edge_digits([]) --> [].

% dimacs_edge_natural/3
% Parses a non-empty sequence of digits.
% Returns an integer.
dimacs_edge_natural(N) --> dimacs_edge_digits(Xs), {Xs \= [], number_chars(N, Xs)}.

% dimacs_edge_problem_line/4
% Parses the problem line.
% Returns the number of nodes and the number of edges.
dimacs_edge_problem_line(Nnodes, Nedges) -->
	dimacs_edge_whitespaces, [p],
	dimacs_edge_whitespaces, [e,d,g,e],
	dimacs_edge_whitespaces, dimacs_edge_natural(Nnodes),
	dimacs_edge_whitespaces, dimacs_edge_natural(Nedges).

% dimacs_edge_edge/3
% Parses an edge.
% Returns an edge.
dimacs_edge_edge(e(U,V)) -->
	dimacs_edge_whitespaces, [e],
	dimacs_edge_whitespaces, dimacs_edge_natural(U),
	dimacs_edge_whitespaces, dimacs_edge_natural(V).

% dimacs_edge_edges/3
% Parses a sequence of edges.
% Returns a list of edges.
dimacs_edge_edges([X|Xs]) --> dimacs_edge_edge(X), !, dimacs_edge_edges(Xs).
dimacs_edge_edges([]) --> [].

% dimacs_edge/5
% Parses a DIMACS file.
% Returns the number of nodes, the number of edges and the list of edges.
dimacs_edge(Nnodes, Nedges, Edges) -->
	dimacs_edge_problem_line(Nnodes, Nedges),
	dimacs_edge_edges(Edges),
	dimacs_edge_whitespaces.