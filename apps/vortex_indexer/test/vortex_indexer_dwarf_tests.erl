%% -*- coding: utf-8 -*-
-module(vortex_indexer_dwarf_tests).

-include_lib("eunit/include/eunit.hrl").

remove_accent_test_() ->
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
