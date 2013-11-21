-module(vortex_indexer_dwarf).
-author('rubin.diego@gmail.com').

-behaviour(gen_server).

-export([init/1, start_link/0, handle_cast/2]).

% not implemented
-export([terminate/2, handle_call/3, code_change/3, handle_info/2]).

% methods
-export([get_body/1, get_tokens/1, remove_stopwords/1]).

-define(STOPWORDS, [
  "último", "é", "acerca", "agora", "algmas", "alguns",
  "ali", "ambos", "antes", "apontar", "aquela", "aquelas",
  "aquele", "aqueles", "aqui", "atrás", "bem", "bom", "cada",
  "caminho", "cima", "com", "como", "comprido", "conhecido",
  "corrente", "das", "debaixo", "dentro", "desde",
  "desligado", "deve", "devem", "deverá", "direita", "diz",
  "dizer", "dois", "dos", "e", "ela", "ele", "eles", "em",
  "enquanto", "então", "está", "estão", "estado", "estar",
  "estará", "este", "estes", "esteve", "estive", "estivemos"
  "estiveram", "eu", "fará", "faz", "fazer", "fazia",
  "fez", "fim", "foi", "fora", "horas", "iniciar", "inicio",
  "ir", "irá", "ista", "iste", "isto", "ligado", "maioria",
  "maiorias", "mais", "mas", "mesmo", "meu", "muito",
  "muitos", "nós", "não", "nome", "nosso", "novo", "o",
  "onde", "os", "ou", "outro", "para", "parte", "pegar",
  "pelo", "pessoas", "pode", "poderá 	podia", "por",
  "porque", "povo", "promeiro", "quê", "qual", "qualquer",
  "quando", "quem", "quieto", "são", "saber", "sem", "ser",
  "seu", "somente", "têm", "tal", "também", "tem", "tempo",
  "tenho", "tentar", "tentaram", "tente", "tentei", "teu",
  "teve", "tipo", "tive", "todos", "trabalhar", "trabalho",
  "tu", "um", "uma", "umas", "uns", "usa", "usar", "valor",
  "veja", "ver", "verdade", "verdadeiro", "você"
]).

init(_Args) ->
  {ok, []}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

handle_cast(_Arg, State) ->
  {stop, normal, State}.

handle_info(timeout, State) -> {stop, shutdown, State}.

handle_call(_Arg, _Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

% methods
get_body(Page) ->
  WithOutNewLine = re:replace(Page, "\n", "", [global, {return, binary}]),
  Result = re:run(WithOutNewLine, "<body>(.*?)</body>", [{capture, [1], list}]),

  case Result of
    {match, [Body]} -> 
      string:strip(Body);
    _ -> ""
  end.

get_tokens(Page) ->
  string:tokens(get_body(Page), ",.?!:;(){} ").

remove_stopwords(Words) ->
  remove_stopwords(Words, []).

remove_stopwords([], List) ->
  lists:reverse(List);
remove_stopwords(Words, List) ->

  [Word | RestWords] = Words,

  case lists:member(Word, ?STOPWORDS) of
    true ->
      remove_stopwords(RestWords, List);
    _ -> remove_stopwords(RestWords, [Word|List])
  end.

