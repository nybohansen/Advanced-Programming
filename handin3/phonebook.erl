-module (phonebook).
-export ([jointest/0, test/0, start/1, add/2, list_all/1, update/2, delete/2, lookup/2, join/2]).

%% Start a new node
start(Name) -> GUID = erlang:md5(Name),
               spawn(fun() -> loop(dict:new(), 
                                   Name, 
                                   GUID, 
                                   spawn(fun() -> emptyLoop() end),
                                   spawn(fun() -> emptyLoop() end)) end).

%% Loop which always responds with end_of_line
emptyLoop() ->
    receive
        {From, _} -> From ! {self(), end_of_line}
    end,
    emptyLoop().

%% Add a contact to the given Pid
add(Pid, Contact) ->
    {Name,_,_} = Contact,
    %% Find the correct peer for the contact
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {add, Contact}).

%% List all the contacts
list_all(Pid) ->
    rpc(Pid, {list_all, both}).
    
%% Utility method for the list_all-call
list_all(Pid, Dir) ->
    case isActive(Pid) of
        true  -> rpc(Pid, {list_all, Dir});
        false -> {ok, []}
    end.

%% Update a contact
update(Pid, Contact) ->
    {Name,_,_} = Contact,
    %% Find the correct peer for the contact
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {update, Contact}).

%% Delete a contact
delete(Pid, Contact) ->
    {Name,_,_} = Contact,
    %% Find the correct peer for the contact
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {delete, Contact}).
    
%% Lookup a contact
lookup(Pid, Name) ->
    %% Find the correct peer for the contact
    Peer = findCorrectPeer(Pid, erlang:md5(Name)),
    rpc(Peer, {lookup, Name}).

%% Join the network via InNetPeer
join(NewPeer, InNetPeer) -> 
    rpc(NewPeer, {join, InNetPeer, both}).
    
%% Utility method for the join-call
join(NewPeer, InNetPeer, Dir) -> 
    rpc(NewPeer, {join, InNetPeer, Dir}).

%% Split the contacts at AtGUID
splitContacts(Pid, AtGUID) ->
    rpc(Pid, {split_contacts, AtGUID}).
    
%% Remote Procedure Call takes care of all the communication
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

%% Our main loop
loop(Contacts, Name, GUID, Left, Right) ->
    receive
	{From, Request} ->
	    {Res, Updated, NewLeft, NewRight} = handle_request(Request, Contacts, GUID, Left, Right),
	    From ! {self(), Res}
    end,
    loop(Updated, Name, GUID, NewLeft, NewRight).

%% Get the GUID of Pid    
getGUID(Pid) ->
    {_, _, GUID} = rpc(Pid, get_info),
    GUID.
    
%% Checks if the Pid is an active peer in the network
isActive(Pid) ->
    case rpc(Pid, get_info) of
        end_of_line -> false;
        _           -> true
    end.

%% Get either left or right neighbour of Pid
getNeighbour(Pid, Neighbour) ->
    {Left, Right, _} = rpc(Pid, get_info),
    case Neighbour of
        left  -> Left;
        right -> Right
    end.

%% Find the correct peer given GUID
findCorrectPeer(InNetPeer, GUID) ->
    case GUID < getGUID(InNetPeer) of
        true -> LeftPeer = getNeighbour(InNetPeer, left),
                case isActive(LeftPeer) of
                    true  -> findCorrectPeer(LeftPeer, GUID);
                    false -> InNetPeer
                end;
        false -> RightPeer = getNeighbour(InNetPeer, right),
                 case isActive(RightPeer) of
                     true  -> case getGUID(RightPeer) < GUID of
                                  true  -> findCorrectPeer(RightPeer, GUID);
                                  false -> InNetPeer
                              end;
                     false -> InNetPeer
                 end
    end.

%% Find the new neighbours for NewPeer given GUID
newNeighbours(Pid, GUID, NewPeer) ->
    rpc(Pid, {new_neighbours, GUID, NewPeer}).

%% Handles all the requests from the remote procedure calls
handle_request(Request, Contacts, GUID, Left, Right) ->
    case Request of
        %% The add method already finds the correct peer, so we just add the contact
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
        %% We list all the contacts, by asking all the other peers about their contacts
        {list_all, Dir} ->
            case Dir of
                left  -> {_, NewList}  = list_all(Left, left);
                right -> {_, NewList}  = list_all(Right, right);
                _     -> {_, NewListL} = list_all(Left, left),
                         {_, NewListR} = list_all(Right, right),
                         NewList = [] ++ NewListL ++ NewListR
            end,
            List = lists:map(fun({_, C}) -> C end, dict:to_list(Contacts)),
            {{ok, List ++ NewList},
             Contacts, 
             Left, 
             Right};
        %% The update method already finds the correct peer, so we just update the contact
        {update, Contact} ->
	        {Name,_,_} = Contact,
            {ok,
             dict:store(Name, Contact, Contacts), 
             Left, 
             Right};
        %% The delete method already finds the correct peer, so we just delete the contact
        {delete, Contact} ->
            {Name,_,_} = Contact,
            {ok,
             dict:erase(Name, Contacts), 
             Left, 
             Right};
        %% The lookup method already finds the correct peer, so we just lookup the contact
        {lookup, Name} ->
            case dict:is_key(Name, Contacts) of
                true  -> {{ok, dict:fetch(Name, Contacts)}, 
                          Contacts, 
                          Left, 
                          Right};
                false -> {{error, Name, not_found}, 
                          Contacts, 
                          Left, 
                          Right}
            end;
        %% Lets join InNetPeer's network
        {join, InNetPeer, Dir} ->
            %% Add all our contacts to InNetPeer's network
            dict:map(fun(_, C) -> add(InNetPeer, C) end, Contacts),
            %% Grab the correct peer, split his contacts, find new neighbours
            Peer                = findCorrectPeer(InNetPeer, GUID),
            NewContacts         = splitContacts(Peer, GUID),
            {NewLeft, NewRight} = newNeighbours(Peer, GUID, self()),
            %% Tell all our own peers to join the same network
            case Dir of
                left  -> join(Left,  InNetPeer, left);
                right -> join(Right, InNetPeer, right);
                _     -> join(Left,  InNetPeer, left),
                         join(Right, InNetPeer, right)
            end,
            {ok, 
             NewContacts, 
             NewLeft, 
             NewRight};            
        
        %%%%
        %% Utility rpcs
        %%%%
        %% Split the contacts into two parts, one lower than own GUID and one higher
        {split_contacts, AtGUID} ->
            case AtGUID < GUID of
                true  -> ToHigh = fun(Name, _) -> erlang:md5(Name) < GUID end;
                false -> ToHigh = fun(Name, _) -> AtGUID < erlang:md5(Name) end
            end,
            Higher = dict:filter(ToHigh, Contacts),
            Lower  = dict:filter(fun(N,C) -> not ToHigh(N,C) end, Contacts),  
            {Higher,
             Lower, 
             Left,
             Right};
        %% Find new neighbours
        {new_neighbours, AtGUID, NewPeer} ->
            case AtGUID < GUID of
                true -> {{Left, self()},
                         Contacts,
                         NewPeer,
                         Right};
                false -> {{self(), Right},
                          Contacts,
                          Left,
                          NewPeer}
            end;
        %% Get info about neighbours and GUID
        get_info ->
            {{Left, Right, GUID},
             Contacts,
             Left,
             Right};
        %% We made an illegal rpc
        Other ->
            {{error, unknown_request, Other},
             Contacts, 
             Left, 
             Right}
    end.

%% Test
test() -> P1 = start("P1"),
          add(P1, {"Donald Duck", "Duckburg", "1313-13-1313"}),
          add(P1, {"Huey Duck", "Duckburg, at Donalds", "555-HUEY-DUCK"}),
          P2 = start("P2"),
          add(P2, {"Dewey Duck", "Duckburg, at Donalds", "555-DEWE-DUCK"}),
          add(P2, {"Louie Duck", "Duckburg, at Donalds", "555-LOUI-DUCK"}),
          join(P2, P1),
          P3 = start("P3"),
          add(P3, {"Scrooge McDuck", "Duckburg, at his money bin", "1"}),
          P4 = start("P4"),
          add(P4, {"Daisy Duck", "Unknown", "12345678"}),
          join(P3, P4),
          join(P4, P1),
          list_all(P4).
          % Doesn't work with P3 and P4? Endless loop

jointest() -> P1 = start("P1"),
              P2 = start("P2"),
              P3 = start("P3"),
              P4 = start("P4"),
              join(P2, P1),
              join(P4, P3),
              join(P3, P2),
              add(P1, {"Donald Duck", "Duckburg", "1313-13-1313"}),
              lookup(P4, "Donald Duck").
              % Endless loop for P4