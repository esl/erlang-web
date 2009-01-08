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
%%% File    : wtpl.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : 
%%%
%%%-------------------------------------------------------------------

-module(wtpl).
-export([build_template/1, build_template/2]).

-include_lib("xmerl/include/xmerl.hrl").

%% @doc The same as build_template(E, []).
build_template(E) ->
    build_template(E, []).

%% @doc Main template system function.
%% <p>There are three types of wtpl tags:<br/>
%% <ul><li><b>&lt;wtpl:parent path=Path&gt;</b> - this tag declares, which template file is the parent of current one. Path attribute points the place, where the parent file is stored.</li>
%% <li><b>&lt;wtpl:include name=Name&gt;</b> - include tag declares the slot: a place where the content should be inserted to. Include tag creates something like a hole in the template - which should be filled up with corresponding wtpl:content content</li>
%% <li><b>&lt;wtpl:content name=Name&gt;</b> - content tag surrounds the content, which should be inserted in place of the corresponding wtpl:include tag</li></ul></p>
%% <p>There are two ways to expand wtpl tag:<br/>
%% <ul><li>place it in your template file and make e_mod_yaws render it</li>
%% <li>expand it in controller - by pointing the .tpl file and
%% providing the list of content tuples. </li></ul>
%% When you want to expand the tag you should always remember about the limitation of one root element of the XHTML structre (you can e.g. iterate on each child or simply surround all the childs with div tag).</p>
%% <p>For example, when you want to create some kind of site you can do it in this way:<br/>
%% <ul><li>Create base file (the scaffold): which will define the standard look of your site. E.g.: <br/>
%% <pre>&lt;html&gt;
%%  &lt;body&gt;
%%    Some sort of header<br/>     &lt;wtpl:include name="slot1"/&gt;
%%       
%%    &lt;wtpl:include name="slot2"/&gt;
%%  &lt;/body&gt;
%%&lt;/html&gt;</pre>Let's name it "main.html"<br/></li>
%% <li>Then create the proxy file (of course you can skip this point or 
%% repeat it as long as you want):
%% <pre> &lt;wtpl:parent path="main.html"&gt;
%%  &lt;wtpl:content name="slot1"&gt;
%%    &lt;h1&gt;This is the first slot&lt;/h1&gt;
%%    &lt;wtpl:include name="slot3"/&gt;
%%  &lt;/wtpl:content&gt;
%%&lt;/wtpl:parent&gt;
%% </pre>And name it "proxy.tpl".<br/>
%% Now, when we want to render proxy.tpl, wtpl engine will find each 
%% occurence of wtpl:content tag, load main.html (because of the 
%% path attribute) and try to fill all the includes with contents he has.
%% So he will fill slot1, but during the filling, he will create new 
%% include: slot3 as well.<br/></li>
%% <li>So now we have to create the last file:
%% <pre> &lt;wtpl:parent path="proxy.tpl"&gt;
%%  &lt;wtpl:content name="slot2"&gt;
%%    &lt;h2&gt;This is the second slot&lt;/h2&gt;
%%  &lt;/wtpl:content&gt;
%%  &lt;wtpl:content name="slot3"&gt;
%%    &lt;h3&gt;This is the third slot&lt;/h3&gt;
%%  &lt;/wtpl:content&gt;
%%&lt;/wtpl:parent&gt;
%% </pre>
%% This final file will fill the slot2 from main.html and slot3 from
%% proxy.html.
%% </li>
%% </ul>
%% <br/></p>
build_template(Filename, Contents) when is_list(Filename) ->
    E = e_cache:read_file(Filename),
    build_template(E, Contents);
build_template(E, Contents) ->
    wpart_xs:process_xml(expand(E, Contents)).

expand(#xmlElement{name = 'wtpl:parent'} = E, Contents) ->
    E2 = E#xmlElement{content = fill_slots(E, Contents)},
    Slots = xmerl_xs:select("descendant::wtpl:content", E2),

    Values = lists:map(fun(X) ->
			       Name = wpartlib:attribute("attribute::name",
							 X),
			       Content = X#xmlElement.content,

			       {Name, Content}
		       end, Slots),

    Path = wpartlib:attribute("attribute::path", E2),
    case xmerl_scan:file(Path) of
        {#xmlElement{} = Parent, _} ->
            Expanded = expand(Parent, lists:flatten([Values | Contents])),

            Expanded#xmlElement{
              content = fill_slots(Expanded#xmlElement.content, Contents)};
        {error, enoent} ->
	    case xmerl_scan:file(filename:join(e_conf:template_root(), Path)) of
		{#xmlElement{} = Parent, _} ->
		    Expanded = expand(Parent, lists:flatten([Values | Contents])),
		    
		    Expanded#xmlElement{
		      content = fill_slots(Expanded#xmlElement.content, Contents)};
		{error, enoent} ->
		    erlang:error({error, enoent}, Path)
	    end
    end;
expand(#xmlElement{name = 'wtpl:include'} = E, Contents) ->
    fill_slots([E], Contents);
expand(E, Contents) ->
    E#xmlElement{content = fill_slots(E#xmlElement.content, Contents)}.

fill_slots([#xmlElement{name = 'wtpl:include'} = E | Rest], Contents) ->
    Name = wpartlib:attribute("attribute::name", E),
    {value, {_, Val}} = lists:keysearch(Name, 1, Contents),

    [Val | fill_slots(Rest, Contents)];
fill_slots([#xmlElement{content = C} = E | Rest], Contents)
  when length(C) == 0 ->
    [E | fill_slots(Rest, Contents)];
fill_slots([#xmlElement{content = C} = E | Rest], Contents) ->
    [E#xmlElement{content = fill_slots(C, Contents)} | 
     fill_slots(Rest, Contents)];
fill_slots([E | Rest], Contents) ->
    lists:flatten([E, fill_slots(Rest, Contents)]);
fill_slots(#xmlElement{content = C, name = 'wtpl:parent'}, Contents) ->
    lists:flatten(fill_slots(C, Contents));
fill_slots([], _) ->
    [].
