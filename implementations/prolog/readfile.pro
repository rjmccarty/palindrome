#! /usr/bin/swipl -f -q

:- initialization main.

main :-
    open('../../testfile.txt', read, Str),
    read_file(Str,Lines),
    close(Str),
    maplist(format('~w~n'), Lines),
%!    format('~w\n', [Lines]).
%!    write(Lines), nl.
    halt(0).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.
