-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Db) -> ok.

delete(Key, Db) ->
    delete_acc(Key, Db, []).

delete_acc(_Key, [], Acc)                      -> Acc ;
delete_acc(Key, [{Key, _Element} | Tail], Acc) ->
    delete_acc(Key, Tail, Acc) ;
delete_acc(Key, [Head | Tail], Acc)            ->
    delete_acc(Key, Tail, [Head | Acc]).

write(Key, Element, Db) -> [{Key, Element} | delete(Key, Db)].

read(_Key, [])                      -> {error, instance} ;
read(Key, [{Key, Element} | _Tail]) -> {ok, Element} ;
read(Key, [ _Head | Tail])          -> read(Key, Tail).

match(Element, Db) ->
    match_acc(Element, Db, []).

match_acc(_Element, [], Acc)                     -> Acc ;
match_acc(Element, [{Key, Element} | Tail], Acc) ->
    match_acc(Element, Tail, [Key | Acc]) ;
match_acc(Element, [_Head | Tail], Acc)          ->
    match_acc(Element, Tail, Acc).

