-module(vortex_indexer).
-author('rubin.diego@gmail.com').

-export([index/1]).

index(Page) ->
  CleanedPage = clean(Page),

  Words = string:tokens(CleanedPage, ";,. ?"),
  count([ string:to_lower(X) || X <- Words ]).
  

% - clean
%   Remove tags html.
clean(Page) ->
  re:replace(Page, "<[^>]*>", "", [global]).

% - count
% - Pontua as palavras
count(Words) ->
  Words.

