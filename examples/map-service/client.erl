-module(client).
-export([get_elem/2, set_elem/3, get_size/1, get_key_list/1, del_elem/2, elem_exist/2]).

get_elem(Server, Key) ->
    Server ! {self(), {get, Key}},
    receive
        {_, Map} -> Map
    end.

set_elem(Server, Key, Value) ->
    Server ! {self(), {set, Key, Value}},
    receive
        {_, Obj} -> Obj
    end.

get_size(Server) ->
    Server ! {self(), {size}},
    receive
        {_, Size} -> Size
    end.

get_key_list(Server) ->
    Server ! {self(), {keys}},
    receive
        {_, KeyList} -> KeyList
    end.

del_elem(Server, Key) ->
    Server ! {self(), {delete, Key}},
    receive
        {_, Obj} -> Obj
    end.

elem_exist(Server, Key) ->
    Server ! {self(), {exist, Key}},
    receive
        {_, Obj} -> Obj
    end.
