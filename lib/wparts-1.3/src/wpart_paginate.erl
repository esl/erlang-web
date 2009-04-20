%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc 
%%% Wpart responsible for pagination of the site
%%% side effects - sets the value for the key specified by "as" atrribute or
%%% "item" by default
%%% sets the keys "prev_page" and "next_page" with the page numbers
%%%
%%% attributes of the wpart:paginate tag:
%%% action - defines the returning value of tag expanding. By default is set to
%%%          "page" - which narrows the list to the interesting part
%%%          other options are "next_link" and "prev_link" which return
%%%          the links (a href) to the next and previous page of the collection.
%%%          "numbered_links" - for list of N numbered links (for accessing the
%%%          wanted page of the collection
%%%    
%%% list - checked only when action == "page", paginates the list held 
%%%        under given key in e_dict (it must be manually set in the controller)
%%%
%%% as - checked only when action == "page", saves the part of list we are
%%%      interested in under the given key, so it is possible to fetch the contents of
%%%      the list inside the wpart:paginate tag
%%%
%%% per_page - checked only when action == "page", defines the maximum length of the list
%%%            we pass to the inside of the tag
%%%
%%% text - checked only when action == "next_page | "prev_page", defines the text which should
%%%        be displayed as the clickable link <a >TEXT</a>
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_paginate).

-export([handle_call/1]).

-include_lib("xmerl/include/xmerl.hrl").

-spec(handle_call/1 :: (tuple()) -> tuple()).	     
handle_call(E) ->
    Action = wpart:has_attribute("attribute::action", "page", E),
    
    action(Action, E).

-spec(action/2 :: (string(), tuple()) -> tuple()).	     
action("page", E) ->
    case wpart:has_attribute("attribute::list", E) of
	false ->
	    #xmlText{value = ""};
	ListName ->
	    List = wpart:fget(ListName),
	    As = wpart:has_attribute("attribute::as", "item", E),
	    PerPage = list_to_integer(wpart:has_attribute("attribute::per_page", "10", E)),

	    Start = case wpart:fget("get:page_no") of
			undefined ->
			    1;
			N ->
			    list_to_integer(N)
		    end,
	    NewList = lists:sublist(List, (Start-1)*PerPage+1, PerPage),

	    if
		Start > 1 ->
		    eptic:fset("prev_page", integer_to_list(Start-1));
		true ->
		    ok
	    end,
	    if
		Start*PerPage+1 < length(List) ->
		    eptic:fset("next_page", integer_to_list(Start+1));
		true ->
		    ok
	    end,

	    get_page(NewList, As, E#xmlElement.content)
    end;

action("numbered_links", E) ->
    case wpart:has_attribute("attribute::list", E) of
	false ->
	    #xmlText{value = ""};
	ListName ->
	    List = wpart:fget(ListName),
	    PerPage = list_to_integer(wpart:has_attribute("attribute::per_page", "10", E)),
	    Start = case wpart:fget("get:page_no") of
			undefined ->
			    1;
			N ->
			    list_to_integer(N)
		    end,
	    ListLen = length(List),
	    Trim = list_to_integer(wpart:has_attribute("attribute::trim", integer_to_list(ListLen), E)),

	    PrevPages = if
			    Start > 1, 
			    Trim+1 >= Start ->
				lists:map(fun create_numbered_links/1,
					  lists:seq(1, Start-1));
			    Start > 1 ->
				["... " | lists:map(fun create_numbered_links/1,
						    lists:seq(Start-Trim, Start-1))];
			    true ->
				[]
			end,
	    Pages = ((ListLen-1) div PerPage),
	    NextPages = if
			    Start < Pages+1,
			    Start+Trim > Pages ->
				lists:map(fun create_numbered_links/1,
					  lists:seq(Start+1, Pages+1));
			    Start < Pages+1 ->
				lists:map(fun create_numbered_links/1,
					  lists:seq(Start+1, Start+Trim)) ++ [" ..."];
			    true ->
				[]
			end,

	    #xmlText{value = PrevPages ++ integer_to_list(Start) ++ " " ++ NextPages,
		     type = cdata}
    end;

action("next_link", E) ->
    Text = wpart:has_attribute("attribute::text", "Next page", E),

    case wpart:fget("next_page") of
	undefined ->
	    #xmlText{value = Text};
	NextPage ->
	    Link = modify_get_params(NextPage),
	    #xmlText{value = "<a href=\"" ++ Link ++ "\">" ++
		     Text ++ "</a>",
		     type = cdata}
    end;

action("prev_link", E) ->
    Text = wpart:has_attribute("attribute::text", "Previous page", E),

    case wpart:fget("prev_page") of
	undefined ->
	    #xmlText{value = Text};
	PrevPage ->
	    Link = modify_get_params(PrevPage),
	    #xmlText{value = "<a href=\"" ++ Link ++ "\">" ++
		     Text ++ "</a>", 
		     type = cdata}
    end.

-spec(get_page/3 :: (list(), string(), string()) -> string()).	     
get_page(List, As, Content) ->
    wpart:fset(As, List),
    wpart:eval(Content).

-spec(modify_get_params/1 :: (string()) -> (string())).	     
modify_get_params(NewVal) ->
    Get = wpart:fget("get"),
    NewGet = [{"page_no", NewVal} | lists:keydelete("page_no", 1, Get)],
    
    Folded = lists:foldl(fun({Key, Val}, Acc) ->
				 [$&, Key, $=, Val | Acc]
			 end, [], NewGet),
    [$& | Rest] = lists:flatten(Folded),
    [$? | Rest].

-spec(create_numbered_links/1 :: (integer()) -> string()).	     
create_numbered_links(N) ->
    SN = integer_to_list(N),
    LinkAddress = modify_get_params(SN),

    "<a href=\"" ++ LinkAddress ++ "\">" ++ SN ++ "</a> ".
