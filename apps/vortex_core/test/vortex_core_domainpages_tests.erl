-module(vortex_core_domainpages_tests).

-include_lib("eunit/include/eunit.hrl").

clear_index_test_() ->
  [{"clear index of domain",
    fun() -> 
      Result = vortex_core_domainpages:clear_index(<<"diegorubin.com">>),
      ?assertEqual(ok, Result)
    end}].

save_test_() ->
  [{"add page in index of domain",
    fun() -> 
      Result = vortex_core_domainpages:add_page_in_domain_list("diegorubin.com", "http://diegorubin.com/"),
      ?assertEqual(["http://diegorubin.com/"], Result)
    end}].

