#!/usr/bin/env escript
-module(readfile).
-export([main/1]).

main([]) ->
   {ok, File} = file:open("../../testfile.txt",[read]),
   Txt = file:read(File,1024 * 1024), 
   io:fwrite("~p~n",[Txt]).
