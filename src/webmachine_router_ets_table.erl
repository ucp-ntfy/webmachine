%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(webmachine_router_ets_table).

-behaviour(gen_server).

%% API
-export([start_link/0,
         current_owner/0,
         become_owner/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, webmachine_router).

-record(state, {current_owner :: pid() | 'undefined'}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

become_owner() ->
    gen_server:call(?SERVER, {become_owner, self()}).

current_owner() ->
    gen_server:call(?SERVER, current_owner).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ?TABLE = ets:new(?TABLE, table_opts()),
    {ok, #state{current_owner = undefined}}.

handle_call({become_owner, Pid}, _From, #state{current_owner = undefined} = State) ->
    true = link(Pid),
    true = ets:give_away(webmachine_router, Pid, undefined),
    Reply = ok,
    {reply, Reply, State#state{current_owner = Pid}};

handle_call(current_owner, _From, #state{current_owner = Owner} = State) ->
    {reply, Owner, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, #state{current_owner = Pid} = State) ->
    {noreply, State#state{current_owner = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
table_opts() ->
    [named_table, set, {keypos, 1},
     {read_concurrency, true},
     {heir, self(), undefined}].
