{application,vortex_web,
             [{description,"Interface web para o indexador Vortex"},
              {vsn,"0.0.1"},
              {modules,[sample_dtl,snippet_dtl,vortex_web,vortex_web_app,
                        vortex_web_page,vortex_web_resource,vortex_web_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,inets,crypto,mochiweb,webmachine]},
              {mod,{vortex_web_app,[]}},
              {env,[]}]}.