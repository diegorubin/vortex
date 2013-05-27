-module(vortex_indexer).
-author('rubin.diego@gmail.com').

-export([index/1]).

index(Page) ->
  CleanedPage = clean(Page),

  Words = string:tokens(CleanedPage, ";,. ?"),
  count(lists:sort([ string:to_lower(X) || X <- Words ])).
  

% - clean
%   Remove tags html.
clean(Page) ->
  re:replace(Page, "<[^>]*>", "", [global]).

% - count
% - Pontua as palavras
count(Words) ->

  count(Words, []).

count([], Counteds) ->
  Counteds;

count(Words, Counteds) ->

  [Word|_] = Words,

  NewCounteds = lists:append(Counteds, [{Word, count_tokens(Word, Words)}]),

  % - Remove elementos ja contados.
  RestWords = lists:dropwhile(fun(Elem) -> Elem =:= Word end, Words),

  count(RestWords, NewCounteds).

count_tokens(Word, Words) ->
  count_tokens(Word, Words, 0).

count_tokens(Word, [Head|Rest], Total) when Word =:= Head ->
  count_tokens(Word, Rest, Total + 1);
count_tokens(_Word, _Words, Total) ->
  Total.
  

