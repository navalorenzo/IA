

%  Chiude i predicati di pruning nel caso di nessun pruning

path_search:start_pruning.
path_search:prune(_,_) :- fail.
path_search:store_closed(_,_).

