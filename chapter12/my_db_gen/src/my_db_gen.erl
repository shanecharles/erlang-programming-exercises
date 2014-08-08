%%%-------------------------------------------------------------------
%%% @author Shane Charles <shanecharles@burningicesolutions.com>
%%% @copyright (C) 2014, Burning Ice Solutions Inc.
%%% @doc
%%%
%%% @end
%%% Created : 2014-08-05 14:18:15.945007
%%%-------------------------------------------------------------------
-module(my_db_gen).

-behaviour(gen_server).

%% API
-export([start/0, stop/0, write/2, read/1, match/1, delete/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
%%         handle_info/2,
         terminate/2
%%         code_change/3
        ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% Delete an entry with the Key supplied
%%
%% @spec delete(Key) -> ok
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc
%% returns a list of keys containing the element
%%
%% @spec match(Element) -> [Key1, Key2, ..., KeyN]
%% @end
%%--------------------------------------------------------------------
match(Element) ->
    gen_server:call(?SERVER, {match, Element}).

%%--------------------------------------------------------------------
%% @doc
%% returns the element associated with the key
%%
%% @spec read(Key) -> {ok, Element} | {error, instance}
%% @end
%%--------------------------------------------------------------------
read(Key) ->
    gen_server:call(?SERVER, {read, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Writes the key/value pair to the database
%%
%% @spec write(Key, Element) -> ok
%% @end
%%--------------------------------------------------------------------
write(Key, Element) ->
    gen_server:call(?SERVER, {write, Key, Element}).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    exit(whereis(?SERVER), shutdown),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    {ok, _Pid} = gen_server:start({local, ?SERVER}, ?MODULE, [], []),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        {ok, db:new()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({delete, Key}, _From, State)         ->
        {reply, ok, db:delete(Key, State)} ;
handle_call({match, Element}, _From, State)      ->
        Reply = db:match(Element, State),
        {reply, Reply, State} ;
handle_call({read, Key}, _From, State)           ->
        Reply = db:read(Key, State),
        {reply, Reply, State} ;
handle_call({write, Key, Element}, _From, State) ->
        New_state = db:write(Key, Element, State), 
        {reply, ok, New_state}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%handle_info(_Info, State) ->
%%        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
        db:destroy(State),
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
%%code_change(_OldVsn, State, _Extra) ->
%%        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




