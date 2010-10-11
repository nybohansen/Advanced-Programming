%% Basic concurrent phone-book
%%
%% @author Ken Friis Larsen <kflarsen@diku.dk>
%% @copyright Ken Friis Larsen 2009-2010

-module (phonebook).
-export ([jointest/0, test/0, start/1, add/2, list_all/1, update/2, delete/2, lookup/2, join/2]).

%% Interface
start(Name) -> GUID = erlang:md5(Name),
               spawn(fun() -> loop(dict:new(), 
                                   Name, 
                                   GUID, 
                                   spawn(fun() -> emptyLoop() end),
                                   spawn(fun() -> emptyLoop() end)) end).
    
emptyLoop() ->
    receive
        {From, _} -> From ! {self(), end_of_line}
    end,
    emptyLoop().

add(Pid, Contact) ->
    {Name,_,_} = Contact,
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {add, Contact}).

list_all(Pid) ->
    rpc(Pid, list_all).

update(Pid, Contact) ->
    {Name,_,_} = Contact,
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {update, Contact}).
    
delete(Pid, Contact) ->
    {Name,_,_} = Contact,
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {delete, Contact}).
    
lookup(Pid, Name) ->
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {lookup, Name}).

join(NewPeer, InNetPeer) -> 
    rpc(NewPeer, {join, InNetPeer, both}).
    
join(NewPeer, InNetPeer, Dir) -> 
    rpc(NewPeer, {join, InNetPeer, Dir}).

splitContacts(Pid, AtGUID) ->
    rpc(Pid, {split_contacts, AtGUID}).

%% Internal implementation

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

loop(Contacts, Name, GUID, Left, Right) ->
    receive
	{From, Request} ->
	    %io:format("~p ~p ~n my left is ~p~n my right is ~p~n my contacts are ~p~n~n", [Name, GUID, Left, Right, dict:to_list(Contacts)]),
	    {Res, Updated, NewLeft, NewRight} = handle_request(Request, Contacts, GUID, Left, Right),
	    From ! {self(), Res}
    end,
    loop(Updated, Name, GUID, NewLeft, NewRight).
    
getGUID(Pid) ->
    {_, _, GUID} = rpc(Pid, get_info),
    GUID.
    
getActive(Pid) ->
    rpc(Pid, get_info).

getNeighbour(Pid, Neighbour) ->
    {Left, Right, _} = rpc(Pid, get_info),
    case Neighbour of
        left -> Left;
        right -> Right
    end.

joinEmAll(InNetPeer, Left, Right, Dir) ->
    case Dir of
        left  -> join(Left,  InNetPeer, left);
        right -> join(Right, InNetPeer, right);
        _     -> join(Left,  InNetPeer, left),
                 join(Right, InNetPeer, right)
    end.
    
findCorrectPeer(InNetPeer, GUID) ->
    case GUID < getGUID(InNetPeer) of
        true -> LeftPeer = getNeighbour(InNetPeer, left),
                case getActive(LeftPeer) of
                    end_of_line -> InNetPeer;
                    _ -> findCorrectPeer(LeftPeer, GUID)
                end;
        false -> RightPeer = getNeighbour(InNetPeer, right),
                 case getActive(RightPeer) of
                     end_of_line -> InNetPeer;
                     _ -> case GUID < getGUID(RightPeer) of
                              true -> InNetPeer;
                              false -> findCorrectPeer(RightPeer, GUID)
                          end
                 end
    end.

newNeighbours(Pid, GUID, NewPeer) ->
    rpc(Pid, {new_neighbours, GUID, NewPeer}).

handle_request(Request, Contacts, GUID, Left, Right) ->
    case Request of
        {add, Contact} -> 
            {Name,_,_} = Contact,
            case dict:is_key(Name, Contacts) of 
                true  -> {{error, Name, is_already_there},
                          Contacts, 
                          Left, 
                          Right};
                false -> {ok, 
                          dict:store(Name, Contact, Contacts), 
                          Left, 
                          Right}
            end;
        list_all ->
            List = dict:to_list(Contacts),
            {{ok, lists:map(fun({_, C}) -> C end, List)},
             Contacts, 
             Left, 
             Right};
        {update, Contact} ->
	        {Name,_,_} = Contact,
            {ok,
             dict:store(Name, Contact, Contacts), 
             Left, 
             Right};
        {delete, Contact} ->
            {Name,_,_} = Contact,
            {ok,
             dict:erase(Name, Contacts), 
             Left, 
             Right};
        {lookup, Name} ->
            case dict:is_key(Name, Contacts) of
                true -> {dict:fetch(Name, Contacts), 
                         Contacts, 
                         Left, 
                         Right};
                false -> {{error, Name, not_found}, 
                          Contacts, 
                          Left, 
                          Right}
            end;
        {split_contacts, AtGUID} ->
            case AtGUID < GUID of
                true -> ToHigh = fun(Name, _) -> erlang:md5(Name) < GUID end;
                false -> ToHigh = fun(Name, _) -> AtGUID < erlang:md5(Name) end
            end,
            Higher = dict:filter(ToHigh, Contacts),
            Lower = dict:filter(fun(N,C) -> not ToHigh(N,C) end, Contacts),  
            {Higher,
             Lower, 
             Left,
             Right};
        {new_neighbours, AtGUID, NewPeer} ->
            case AtGUID < GUID of
                true -> {{Left, self()},
                         Contacts,
                         NewPeer,
                         Right};
                false -> {{Right, self()},
                          Contacts,
                          Left,
                          NewPeer}
            end;
        get_info ->
            {{Left, Right, GUID},
             Contacts,
             Left,
             Right};
        {join, InNetPeer, Dir} ->
            joinEmAll(InNetPeer, Left, Right, Dir),
            Peer = findCorrectPeer(InNetPeer, GUID),
            dict:map(fun(_, C) -> add(InNetPeer, C) end, Contacts),
            NewContacts = splitContacts(Peer, GUID),
            {NewLeft, NewRight} = newNeighbours(Peer, GUID, self()),
            {ok, 
             NewContacts, 
             NewLeft, 
             NewRight};
        Other ->
            {{error, unknown_request, Other},
             Contacts, 
             Left, 
             Right}
    end.

%% Test
jointest() -> P1 = start("P1"),
              add(P1, {"Kasper", "Somewhere on Amager", "4321-012"}),
              P2 = start("P2"),
              add(P2, {"Ken", "Ryumgaårdvej", "21424"}),
              join(P2, P1).

test() -> P1 = start("P1"),
          add(P1, {"Kasper", "Somewhere on Amager", "4321-012"}),
          add(P1, {"Mads Ohm Larsen", "Planet Earth", "555-DONT-CALL"}),
          P2 = start("P2"),
          join(P2, P1),
          add(P2, {"Ken", "Ryumgaårdvej", "21424"}),
          lookup(P1, "Ken").