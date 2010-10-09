%% Basic concurrent phone-book
%%
%% @author Ken Friis Larsen <kflarsen@diku.dk>
%% @copyright Ken Friis Larsen 2009-2010

-module(phonebook).
-export([start/0, add/2, list_all/1, update/2, delete/2, lookup/2]).

%% Interface

start() -> spawn(fun() -> loop(dict:new()) end).

add(Pid, Contact) ->
    rpc(Pid, {add, Contact}).

list_all(Pid) ->
    rpc(Pid, list_all).

update(Pid, Contact) ->
    rpc(Pid, {update, Contact}).
    
delete(Pid, Contact) ->
    rpc(Pid, {delete, Contact}).
    
lookup(Pid, Name) ->
    rpc(Pid, {lookup, Name}).

%% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

loop(Contacts) ->
    receive
	{From, Request} ->
	    {Res, Updated} = handle_request(Request, Contacts),
	    From ! {self(), Res},
	    loop(Updated)
    end.

%% 
handle_request(Request, Contacts) ->
    case Request of
        {add, Contact} -> 
            {Name,_,_} = Contact,
            case dict:is_key(Name, Contacts) of 
                false -> {ok, dict:store(Name, Contact, Contacts)};
                true  -> {{error, Name, is_already_there},
                          Contacts}
            end;
        list_all ->
            List = dict:to_list(Contacts),
            {{ok, lists:map(fun({_, C}) -> C end, List)},
             Contacts};
        {update, Contact} ->
	        {Name,_,_} = Contact,
            {ok,
             dict:store(Name, Contact, Contacts)};
        {delete, Contact} ->
            {Name,_,_} = Contact,
            {ok,
             dict:erase(Name, Contacts)};
        {lookup, Name} ->
            case dict:is_key(Name, Contacts) of
                true -> {dict:fetch(Name, Contacts), Contacts};
                false -> {{error, Name, not_found},
                          Contacts}
            end;
        Other ->
            {{error,unknown_request, Other},
             Contacts}
    end.

