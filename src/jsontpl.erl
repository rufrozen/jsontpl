%%=====================================================================
%% Copyright (c) 2011 Igor Timurov, <t051200@yandex.ru>
%% All Rights Reserved.
%%=====================================================================

-module(jsontpl).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    build/2,
    encode/1,
    decode/1
]).

%%
%% API Functions
%%

build(Project, FileName) ->
    jsontpl_builder:build(Project, FileName).

encode(JsonObject) ->
    couchbeam_util:json_encode(JsonObject).

decode(Binary) ->
    couchbeam_util:json_decode(Binary).


%%
%% Local Functions
%%

