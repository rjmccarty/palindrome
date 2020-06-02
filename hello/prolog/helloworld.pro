#! /usr/bin/swipl -f -q

:- initialization main.

main :-
%!  current_prolog_flag(argv, Argv),
%!  format('Hello World!, argv:~w\n', [Argv]),
  format('Hello world!~n'),
  halt(0).
