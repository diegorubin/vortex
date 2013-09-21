-module(vortex_page_tests).

-include_lib("eunit/include/eunit.hrl").

to_json_test_() ->
  [{"create page struct",
    fun() -> 

      {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
      ReadAt = vortex_json:to_json([{year, Year}, {month, Month}, {day, Day}, {hour, Hour}, {minute, Minute}, {second, Second}]),
      Result = vortex_page:to_page("http://diegorubin.com",
                                   "Página Pessoal", "<html></html>"),

      ?assertEqual({page, [{domain, "http://diegorubin.com"},
                           {title, "Página Pessoal"},
                           {body, "<html></html>"},
                           {readat, ReadAt}]}, Result)
    end}].

