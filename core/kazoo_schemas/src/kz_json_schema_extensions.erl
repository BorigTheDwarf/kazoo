%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc Module for extending schema validation
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_schema_extensions).

-export([extra_validator/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(INVALID_STORAGE_ATTACHMENT_REFERENCE(R), <<"invalid reference '", R/binary, "' to attachments">>).
-define(INVALID_STORAGE_CONNECTION_REFERENCE(R), <<"invalid reference '", R/binary, "' to connections">>).

-spec extra_validator(jesse:json_term(), jesse_state:state()) -> jesse_state:state().
extra_validator(Value, State) ->
    Schema = jesse_state:get_current_schema(State),
    %%Str = lists:flatten(lists:duplicate(100, "=")),
    %%lager:debug("~n~p~nValue: ~p~nSchema: ~p~nState: ~p~n~p", [Str, Value, Schema, State, Str]),
    State1 = check_params_stability_level(Value, Schema, State),
    %%lager:debug("~n~p~nValue: ~p~nSchema: ~p~n~p", [Str, Value, Schema, Str]),
    %case is_binary(Value) orelse is_atom(Value) orelse is_number(Value) orelse is_list(Value) of
    %    false ->
    %        {ValList} = Value,
    %        {Elem, _} = hd(ValList),
    %        case lists:member(Elem, [<<"dialog_subscribed_mwi_prefix">> ,<<"hangups_to_monitor">>
    %                                ,<<"inactivity_timeout_s">> ,<<"attempt_failure_count">>
    %                                ,<<"aging_expiry_d">> ,<<"enable">>, <<"default_enabled">>
    %                                ])  of
    %            true ->
    %                ok;
    %            false ->
    %                %%Str = lists:flatten(lists:duplicate(100, "=")),
    %                lager:debug("~n~p~nValue: ~p~nSchema: ~p~nState: ~p~n~p",
    %                            [Str, Value, Schema, State, Str]),
    %                check_params_stability_level(Value, Schema, State)
    %        end;
    %    _ ->
    %        ok
    %end,
    case kz_json:is_true(<<"kazoo-validation">>, Schema, 'false') of
        'true' -> extra_validation(Value, State1);
        'false' -> State1
    end.

-spec extra_validation(jesse:json_term(), jesse_state:state()) -> jesse_state:state().
extra_validation(Value, State) ->
    SchemaId = jesse_state:get_current_schema_id(State),
    Path = lists:reverse(jesse_state:get_current_path(State)),

    ElementId = kz_term:to_binary(lists:last(Path)),
    Keys = [SchemaId, ElementId],
    Key = kz_binary:join(lists:filter(fun kz_term:is_not_empty/1, Keys), <<".">>),
    extra_validation(Key, Value, State).

extra_validation(<<"metaflow.data">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    [_Data | Path] = jesse_state:get_current_path(State),
    Module = jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, 'undefined'),
    lager:debug("validating metaflow action '~s' with data ~p", [Module, Value]),
    validate_module_data(<<"metaflows.", Module/binary>>, Value, State);

extra_validation(<<"metaflow.module">>, Value, State) ->
    lager:debug("validating metaflow action '~s'", [Value]),
    Schema = <<"metaflows.", Value/binary>>,
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema -> State1;
                 _OtherSchema -> jesse_error:handle_data_invalid('external_error', <<"unable to find metaflow schema for module ", Value/binary>>, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"callflows.action.data">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    [_Data | Path] = jesse_state:get_current_path(State),
    case jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, undefined) of
        'undefined' -> State;
        Module -> validate_module_data(<<"callflows.", Module/binary>>, Value, State)
    end;
extra_validation(<<"callflows.action.module">>, Value, State) ->
    lager:debug("validating callflow action '~s'", [Value]),
    Schema = <<"callflows.", Value/binary>>,
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema -> State1;
                 _OtherSchema -> jesse_error:handle_data_invalid(external_error, <<"unable to find callflow schema for module ", Value/binary>>, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"storage.plan.database.document.connection">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    Keys = kz_json:get_keys(<<"connections">>, JObj),
    case lists:member(Value, Keys) of
        'true' -> State;
        'false' -> jesse_error:handle_data_invalid('external_error'
                                                  ,?INVALID_STORAGE_CONNECTION_REFERENCE(Value)
                                                  ,State
                                                  )
    end;
extra_validation(<<"storage.plan.database.attachment.handler">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    Keys = kz_json:get_keys(<<"attachments">>, JObj),
    case lists:member(Value, Keys) of
        'true' -> State;
        'false' -> jesse_error:handle_data_invalid('external_error'
                                                  ,?INVALID_STORAGE_ATTACHMENT_REFERENCE(Value)
                                                  ,State
                                                  )
    end;
extra_validation(<<"storage.attachment.google_drive.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(<<"storage.attachment.google_storage.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(<<"storage.attachment.onedrive.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(<<"storage.attachment.dropbox.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(_Key, _Value, State) ->
    lager:debug("extra validation of ~s not handled for value ~p", [_Key, _Value]),
    State.

validate_module_data(Schema, Value, State) ->
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema ->
                     SchemaObj = jesse_state:get_current_schema(State1),
                     jesse_schema_validator:validate_with_state(SchemaObj, Value, State1);
                 _OtherSchema -> State1
             end,
    jesse_state:undo_resolve_ref(State2, State).

validate_attachment_oauth_doc_id(Value, State) ->
    lager:debug("Validating oauth_doc_id: ~s", [Value]),
    case kz_datamgr:open_doc(<<"system_auth">>, Value) of
        {ok, _Obj} ->
            State;
        {error, not_found} ->
            ErrorMsg = <<"Invalid oauth_doc_id: ", Value/binary>>,
            lager:debug("~s", [ErrorMsg]),
            jesse_error:handle_data_invalid('external_error', ErrorMsg, State)
    end.

check_params_stability_level(Value, Schema, State) ->
    case {kz_json:get_value(<<"properties">>, Schema),
          kz_json:get_value(<<"id">>, Schema)} of
        {'undefined', _} ->
            %lager:debug("Skipping schema with out `properties' property"),
            State;
        {Properties, Id} when Id /= <<"system_config.omnipresence">>
                              andalso Id /= <<"system_config.webhooks">>
                              andalso Id /= <<"system_config.hangups">> ->
            {ok, CBConfig} = kz_datamgr:open_cache_doc(<<"system_config">>, <<"crossbar">>),
            SystemSL = kz_json:get_binary_value([<<"default">>, <<"stability_level">>], CBConfig),
            SystemSLInt = stability_level_to_int(SystemSL),
            lager:debug("Id: ~p, System stability_level(~p): ~p", [Id, SystemSLInt, SystemSL]),
            Fun = fun(Key, _Val, Acc) ->
                      SearchKey = [Key, <<"stability_level">>],
                      PropSL = kz_json:get_value(SearchKey, Properties, <<"stable">>),
                      lager:debug("~p SL: ~p", [Key, PropSL]),
                      case  stability_level_to_int(PropSL) < SystemSLInt of
                          true -> [Key | Acc];
                          false -> Acc
                      end
                  end,
            case kz_json:foldl(Fun, [], Value) of
                [] ->
                    State;
                InvalidKeys ->
                    InvalidKeysBin = binary:list_to_bin(lists:join(<<", ">>, InvalidKeys)),
                    %lager:debug("InvalidKeys found: ~p, InvalidKeysBin: ~p", [InvalidKeys, InvalidKeysBin]),
                    ErrorMsg = <<"Invalid keys with lower stability level than ",
                                 SystemSL/binary, " : ", InvalidKeysBin/binary>>,
                    lager:debug("~s", [ErrorMsg]),
                    jesse_error:handle_data_invalid('external_error', ErrorMsg, State)
            end;
        _ ->
            State
    end.

-spec stability_level_to_int(kz_term:ne_binary()) -> pos_integer().
stability_level_to_int(<<"stable">>) -> 3;
stability_level_to_int(<<"beta">>) -> 2;
stability_level_to_int(<<"alpha">>) -> 1.
