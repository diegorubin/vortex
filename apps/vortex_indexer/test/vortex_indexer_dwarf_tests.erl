%% -*- coding: utf-8 -*-
-module(vortex_indexer_dwarf_tests).

-include_lib("eunit/include/eunit.hrl").

get_body_test_() ->
  [{"get only body of text",
    fun() -> 

      Page = "<html>
               <head>
                 <title>teste</title>
               </head>
               <body>
                 uma pagina simples
               </body>
              </html>",

      Result = vortex_indexer_dwarf:get_body(Page),
      ?assertEqual("uma pagina simples", Result)
    end}].

get_tokens_test_() ->
  [{"get tokens of body",
    fun() -> 

      Page = "<html>
               <head>
                 <title>teste</title>
               </head>
               <body>
                 uma pagina simples
               </body>
              </html>",

      Result = vortex_indexer_dwarf:get_tokens(Page),
      ?assertEqual(["uma", "pagina", "simples"], Result)
    end}].

remove_stopwords_test_() ->
  [{"remove stop words",
    fun() -> 
      Words = ["uma", "pagina", "simples"],
      Result = vortex_indexer_dwarf:remove_stopwords(Words),
      ?assertEqual(["pagina", "simples"], Result)
    end}].

