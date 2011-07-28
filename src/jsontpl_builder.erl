%%=====================================================================
%% Copyright (c) 2011 Igor Timurov, <t051200@yandex.ru>
%% All Rights Reserved.
%%=====================================================================

-module(jsontpl_builder).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([build/2]).

%%
%% API Functions
%%
build(Project, FileName) ->
    build(Project, FileName, template_list(Project)).
    
template_list(Project) ->
    filelib:fold_files(templates_dir(Project), "\.tpl$", true, fun(F,Acc) -> [F|Acc]end, []).

build(Project, FileName, TplFiles) ->
    Templates = lists:foldl(fun open_tpl/2, [], TplFiles),
    HrlFileName = filename:join(include_dir(Project), FileName ++ ".hrl"),
    {ok, Hrl} = file:open(HrlFileName, [write]),
    ErlFileName = filename:join(src_dir(Project), FileName ++ ".erl"),
    {ok, Erl} = file:open(ErlFileName, [write]),
    io:format(Hrl, "~s", [title()]),
    module_title(Erl, FileName),
    io:format(Erl, "-export([~s]).~n~n", [export_string(Templates)]),
    lists:foreach(
        fun(Template) -> 
            build2(Template, Hrl, Erl)
        end, Templates),
    file:close(Hrl),
    file:close(Erl).

build2(Template, Hrl, Erl) ->
    NewTemplaet = convert_template(Template),
    define_record(NewTemplaet, Hrl),
    define_parser(NewTemplaet, Erl),
    define_packer(NewTemplaet, Erl).

define_record({Record, Fields}, Hrl) ->
    io:format(Hrl, "-record(~w, {~n", [Record]),
    define_record_fields(Fields, Hrl),
    io:format(Hrl, "}).~n~n", []).    

define_record_fields([], _Hrl) ->
    ok;
define_record_fields([{Tag, _, _, Default} | Fields], Hrl) ->
    io:format(Hrl, "    ~p = ~p~s~n", [Tag, Default, comma(Fields)]),
    define_record_fields(Fields, Hrl).

define_parser({Record, Fields}, Erl) ->
    io:format(Erl, "~w(JsonObj) ->~n", [Record]),
    io:format(Erl, "    #~w{~n", [Record]),
    define_parser_fields(Fields, Erl),
    io:format(Erl, "    }.~n~n", []).

define_parser_fields([], _Erl) ->
    ok;
define_parser_fields([{Tag, Name, Type, Default} | Fields], Erl) ->
    io:format(Erl, "        ~s = jsontpl_types:parse(JsonObj, ~p, ~w, ~p)~s~n", [Tag, Name, Type, Default, comma(Fields)]),
    define_parser_fields(Fields, Erl).

define_packer({Record, Fields}, Erl) ->
    io:format(Erl, "~w(JsonObj0, Object) ->~n", [Record]),
    define_packer_fields(Fields, Record, 0, Erl),
    io:format(Erl, "~n~n", []).

define_packer_fields([], _Record, Id, Erl) ->
    io:format(Erl, "    JsonObj~p.", [Id]);
define_packer_fields([{Tag, Name, Type, _Default} | Fields], Record, Id, Erl) ->
    NextId = Id+1,
    io:format(Erl, "    JsonObj~p = jsontpl_types:pack(JsonObj~p, ~p, ~p, Object#~s.~s),~n", [NextId, Id, Name, Type, Record, Tag]),
    define_packer_fields(Fields, Record, NextId, Erl).
    
%%
%% Local Functions
%%

convert_template({Record, Fields}) ->
    {Record, lists:map(fun({_Tag, _Name, _Type, _Default} = A) -> A;
                          ({Tag, Type, Default}) -> {Tag, atom_to_binary(Tag, latin1), Type, Default} end, Fields)}.   

open_tpl(File, Acc) ->
    try
        {ok, FileTemplates} = file:consult(File),
        Acc ++ FileTemplates
    catch
        error:Reason ->
            io:format("error: ~p~nfile: ~p~n", [Reason, File]),
            Acc
    end.

title() ->
    "%% Author: Jsontpl compiler\n"
    "%% Description:\n"
    "%%     This file was generated.\n"
    "%%     DO NOT EDIT IT DIRECTLY!\n\n".

module_title(Erl, ModuleName) ->
    io:format(Erl, "~s", [title()]),
    io:format(Erl, "-module(~s).~n~n", [ModuleName]),
    io:format(Erl, "-include(\"~s.hrl\").~n~n", [ModuleName]).

app_dir(Project) ->
    code:lib_dir(Project).

templates_dir(Project) ->
    filename:join(priv_dir(Project), "jsontpl").

include_dir(Project) ->
    filename:join(app_dir(Project), "include").

priv_dir(Project) ->
    code:priv_dir(Project).

src_dir(Project) ->
    filename:join(app_dir(Project), "src").

comma([]) ->
    "";
comma(_List) ->
    ",".

export_string(Templates) ->
    RecordStrings = ["\n    " ++ atom_to_list(Record) ++ "/1," ++
                     "\n    " ++ atom_to_list(Record) ++ "/2" 
                    || {Record, _Fields} <- Templates],
    string:join(RecordStrings, ",") ++ "\n".

