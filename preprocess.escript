#!/usr/bin/env escript
-mode(compile).

%% TODO reuse this facility when loading scripts in mzbench_sup

main(File) ->
  put(file_dir, filename:dirname(File)),
  Res = walk(slurp(File), fun(F) -> F end, fun on_form/1),
  io:format("~p", [Res]).

walk(Form, Pre, Post) ->
  Form1 = Pre(Form),
  Form2 = case Form1 of
    _ when is_list(Form1) ->
      lists:map(fun(F) -> walk(F, Pre, Post) end, Form1);
    _ when is_tuple(Form1) ->
      list_to_tuple(walk(tuple_to_list(Form1), Pre, Post));
    _ ->
      Form1
  end,
  Post(Form2).

on_form({include_resource, N, F}) ->
  {resource, N, slurp(get(file_dir) ++ "/" ++ F)};
on_form({env, N}) ->
  os:getenv(N);
on_form(F) -> F.
  
slurp(File) ->
  {ok, [Content]} = file:consult(File),
  Content.