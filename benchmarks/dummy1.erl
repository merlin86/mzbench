[
 {include_resource, text, "text.txt"},
 {use_graphite, "p2.virt.tld:2003"},
 {pool, [{size, 3},
         {worker_type, dummy_worker}],
  [{loop, [{time, {5, sec}},
           {rate, {1, rps}}],
    [{print, {env, "xterm"}}]}]},

 {pool, [{size, 60},
         {worker_type, dummy_worker}],
  [{print, {choose, {resource, text}}},
   {print, "BAR"},
   {print, "QUUX"}]}
].
