%%=====================================================================
%% Copyright (c) 2011 Igor Timurov, <t051200@yandex.ru>
%% All Rights Reserved.
%%=====================================================================

-module(jsontpl_types).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    parse/4,
    pack/4
]).

%%
%% API Functions
%%

parse(JsonObj, Key, Type, Default) ->
    Value = couchbeam_doc:get_value(Key, JsonObj),
    to_type(Type, Value, Default).

pack(JsonObj, _Key, _Type, null) ->
    JsonObj;
pack(JsonObj, _Key, _Type, undefined) ->
    JsonObj;
pack(JsonObj, Key, Type, Data) ->
    Value = from_type(Type, Data),
    couchbeam_doc:set_value(Key, Value, JsonObj).

%%
%% Local Functions
%%

from_type(any, Value) ->
    Value;
from_type(bool, Value) when is_boolean(Value) ->
    Value;
from_type(float, Value) when is_float(Value) ->
    Value;
from_type(integer, Value) when is_integer(Value) ->
    Value;
from_type(atom, Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
from_type(string, Value) when is_binary(Value) ->
    Value;
from_type(string, Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
from_type(string, Value) when is_float(Value) ->
    list_to_binary(float_to_list(Value));
from_type(string, Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
from_type(jsonstring, Value) when is_binary(Value)  ->
    jsontpl:decode(Value);
from_type({hash, _Size}, Value) when is_integer(Value) ->
    Value;
from_type({tpl, Module, Fun}, Value) ->
    Module:Fun({[]}, Value);
from_type([Type], List) when is_list(List) ->
    [from_type(Type, Item) || Item <- List];
from_type({_LeftType, RightType}, {LeftItem, RightItem}) ->
    {from_type(string, LeftItem), from_type(RightType, RightItem)}.

%% trivial type
to_type(_Type, null, Default) ->
    Default;
to_type(_Type, undefined, Default) ->
    Default;
to_type(any, Value, _Default) ->
    Value;
to_type(bool, Value, _Default) when is_boolean(Value) ->
    Value;
to_type(bool, <<"true">>, _Default) ->
    true;
to_type(bool, <<"false">>, _Default) ->
    false;
to_type(float, Value, _Default) when is_number(Value) ->
    float(Value);
to_type(float, Value, _Default) when is_binary(Value) ->
    list_to_float(binary_to_list(Value));
to_type(integer, Value, _Default) when is_number(Value) ->
    round(Value);
to_type(integer, Value, _Default) when is_binary(Value) ->
    list_to_integer(binary_to_list(Value));
to_type(atom, Value, _Default) when is_binary(Value) ->
    binary_to_atom(Value, latin1);
to_type(atom, Value, _Default) when is_atom(Value) ->
    Value;
to_type(string, Value, _Default) when is_binary(Value) ->
    Value;
to_type(jsonstring, Value, _Default) ->
    iolist_to_binary(jsontpl:encode(Value));
%% complex type
to_type({tpl, Module, Fun}, Value, _Default) ->
    Module:Fun(Value);
to_type({hash, Size}, Value, _Default) ->
    erlang:phash2(Value, Size);
to_type([Type], List, Default) when is_list(List) ->
    [to_type(Type, Item, Default) || Item <- List];
to_type([Type], {List}, Default) when is_list(List) ->
    [to_type(Type, Item, Default) || Item <- List];
to_type({LeftType, RightType}, {LeftItem, RightItem}, Default) ->
    {to_type(LeftType, LeftItem, Default), to_type(RightType, RightItem, Default)}.
