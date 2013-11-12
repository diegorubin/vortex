%% -*- coding: utf-8 -*-
%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

-module(vortex_core_utils).
-author('Diego Rubin <rubin.diego@gmail.com>').

-export([remove_accent/1, find_value/2, in_list/3, in_list/2, put_on_list_if_not_have/2, list_len/1, int_to_string/1, force_string_list/1, uniconvert/1]).

remove_accent("") -> "";
remove_accent(String) ->
  remove_accent(String, "").

remove_accent([], String) ->
  lists:reverse(String);

remove_accent(Rest, String) ->
  [C|NewRest] = Rest,

  ValueA = find_value(C, [$ã, $á, $à]),
  ValueAA = find_value(C, [$Ã, $Á, $À]),
  ValueE = find_value(C, [$ẽ, $é, $è]),
  ValueEE = find_value(C, [$Ẽ, $É, $È]),
  ValueI = find_value(C, [$ĩ, $í, $ì]),
  ValueII = find_value(C, [$Ĩ, $Í, $Ì]),
  ValueO = find_value(C, [$õ, $ó, $ò]),
  ValueOO = find_value(C, [$Õ, $Ó, $Ò]),
  ValueU = find_value(C, [$ũ, $ú, $ù]),
  ValueUU = find_value(C, [$Ũ, $Ú, $Ù]),

  if is_number(ValueA) -> remove_accent(NewRest, [$a|String]);
     is_number(ValueAA) -> remove_accent(NewRest, [$A|String]);
     is_number(ValueE) -> remove_accent(NewRest, [$e|String]);
     is_number(ValueEE) -> remove_accent(NewRest, [$E|String]);
     is_number(ValueI) -> remove_accent(NewRest, [$i|String]);
     is_number(ValueII) -> remove_accent(NewRest, [$I|String]);
     is_number(ValueO) -> remove_accent(NewRest, [$o|String]);
     is_number(ValueOO) -> remove_accent(NewRest, [$O|String]);
     is_number(ValueU) -> remove_accent(NewRest, [$u|String]);
     is_number(ValueUU) -> remove_accent(NewRest, [$U|String]);
     true -> remove_accent(NewRest, [C|String])
  end.

in_list([], _) ->
  false;
in_list(List, Item) ->
  [Head | Tail] = List,
  in_list(Head, Tail, Item).

in_list(Elem, _, Item) when Elem == Item -> true;
in_list(_, [], _Item) -> false;
in_list(_, Rest, Item) ->
  [Head | Tail] = Rest,
  in_list(Head, Tail, Item).

put_on_list_if_not_have(List, Elem) ->
  case in_list(List, Elem) of
  true -> List;
  false -> [Elem | List]
  end.
  
int_to_string(Int) ->
  lists:flatten(io_lib:format("~p", [Int])).

force_string_list(String) ->
  lists:flatten(io_lib:format("~s", [String])).

list_len(List) ->
  list_len(0, List).

list_len(Total, []) ->
  Total;
list_len(Total, List) ->
  [_|Rest] = List,
  list_len(Total + 1, Rest).

uniconvert(String) ->
  try xmerl_ucs:from_utf8(String) of
    _ ->             
      list_to_binary(String)
  catch
    exit:{ucs,{bad_utf8_character_code}} -> 
      list_to_binary(xmerl_ucs:to_utf8(String))
  end.

%% private functions
find_value(Key, List) ->
  find_value(Key, List, 0).

find_value(_, [], _Index) ->
  nothing;
find_value(Key, List, Index) ->
  [C|Rest] = List,
  
  case C == Key of
    true ->  
      Index;
    false ->
      find_value(Key, Rest, Index + 1)
  end.

