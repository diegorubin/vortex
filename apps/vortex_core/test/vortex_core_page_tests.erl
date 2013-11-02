-module(vortex_core_page_tests).

-include_lib("eunit/include/eunit.hrl").

to_page_test_() ->
  [{"create page struct",
    fun() -> 

      {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
      ReadAt = vortex_core_json:to_json([{year, Year}, {month, Month}, {day, Day}, {hour, Hour}, {minute, Minute}, {second, Second}]),
      Result = vortex_core_page:to_page("http://diegorubin.com",
                                   "Pagina Pessoal", "<html></html>"),

      ?assertEqual({page, [{domain, "http://diegorubin.com"},
                           {title, "Pagina Pessoal"},
                           {body, "<html></html>"},
                           {readat, ReadAt}]}, Result)
    end}].

save_test_() ->
  [{"create page struct",
    fun() -> 
      {page, PageData} = vortex_core_page:to_page("http://diegorubin.com",
                                   "Pagina Pessoal", "<html></html>"),
      Page = {page, [{key, vortex_core_page:url_to_key("http://diegorubin.com/about/diegorubin")} | PageData]},
      Result = vortex_core_page:save({page, PageData}, "http://diegorubin.com/about/diegorubin"),
      vortex_core_page:delete("http://diegorubin.com/about/diegorubin"),
      ?assertEqual(Page, Result)
    end}].

update_test_() ->
  [{"update page struct",
    fun() -> 
      {page, PageData} = vortex_core_page:to_page("http://diegorubin.com",
                                   "Página Pessoal", "<html></html>"),

      vortex_core_page:save({page, PageData}, "http://diegorubin.com/"),

      {page, NewPageData} = vortex_core_page:to_page("http://diegorubin.com",
                                   "Minha", "<html></html>"),
      Page = {page, [{key, vortex_core_page:url_to_key("http://diegorubin.com/")} | NewPageData]},
      Result = vortex_core_page:save(Page, "http://diegorubin.com/"),

      vortex_core_page:delete("http://diegorubin.com/"),
      ?assertEqual(Page, Result)
    end}].

