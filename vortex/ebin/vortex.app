{application,vortex,
             [{description,"Vortex Server"},
              {vsn,"1.0"},
              {registered,[vortex]},
              {mod,{vortex_app,[]}},
              {applications,[kernel,stdlib]},
              {modules,[spooky,spooky_server,spooky_sessions,spooky_state,
                        spooky_static,spooky_test,uuid,vortex,vortex_app,
                        vortex_server]}]}.
