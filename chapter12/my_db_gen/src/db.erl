-module(db).

-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Db) -> ok.

write(Key, Element, Db) ->
    [{Key, Element} | delete(Key, Db)].

delete(Key, Db) ->
    lists:keydelete(Key, 1, Db).

read(Key, Db) ->
    Result = lists:keyfind(Key, 1, Db),
    case Result of
        {Key, Element}    -> {ok, Element} ;
        false -> {error, instance}
    end.

match(Element, Db) ->
    [Key || {Key, _Elem} <- lists:filter(fun({_Key, Elem}) -> Elem =:= Element end, Db)].
