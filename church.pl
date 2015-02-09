:- use_module(library(socket)).

:- dynamic church_thread/1, debug/1, auth_count/1, nick/1.


debug_message(Msg, Args) :-
    debug, !,
    display('debug: '),
    format(Msg, Args),
    nl.
debug_message(_, _).

connect(Server, Port) :- 
    \+ church_thread(_),
    thread_create(connect_helper(Server, Port), Tid, []),
    assert(church_thread(Tid)).

initialize_globals :-
    retractall(auth_count(_)),
    assert(auth_count(0)),
    retractall(nick(_)),
    assert(nick(church)).

connect_helper(Server, Port) :-
    initialize_globals,
    setup_call_catcher_cleanup(
        tcp_socket(Socket),
        (tcp_connect(Socket, Server:Port, Stream), chat_to_server(Stream)),
        (exception(E), format("exception: ~w~n", E)),
        (retractall(church_thread(_)), tcp_close_socket(Socket))).

chat_to_server(StreamPair) :-
    get_char(StreamPair, Char),
    eat_nl(StreamPair, Char, NewChar),
    read_line(StreamPair, NewChar, Line),!,
    maplist(atom_codes, Line, TmpStrLine),
    flatten(TmpStrLine, StrLine),
    atom_codes(DebugLine, StrLine),
    debug_message("message: ~w", [DebugLine]),
    catch(parse_server_msg(Msg, StrLine, []),
          Exception, 
          handle_error(StreamPair, Exception)),!,
    debug_message("~w", Msg),
    handle_message(Msg, StreamPair),
    chat_to_server(StreamPair).

handle_error(_StreamPair, irc_error(422, _, _)) :-
    debug_message("No MOTD provided", []).
handle_error(StreamPair, irc_error(433, _, _)) :-
    nick(Nick),
    !, retractall(nick(_)),
    atom_concat(Nick, '_', NewNick),
    debug_message("updating nickname to ~w", [NewNick]),
    assert(nick(NewNick)),
    register_nick(StreamPair).
handle_error(StreamPair, irc_error(451, _, _)) :- 
    !, register_user(StreamPair), join_bots(StreamPair).
handle_error(_StreamPair, irc_error(462, _, _)) :-
    !, debug_message("Reregister attempted and failed", []).
handle_error(_StreamPair, Exception) :- 
    debug_message("rethrowing: ~w", [Exception]),
    throw(Exception).

update_auth_count :-
    auth_count(X),
    retractall(auth_count(_)),
    Y is X + 1,
    assert(auth_count(Y)).

register_user(Stream) :- 
    stream_pair(Stream, _, WStream),
    set_stream(WStream, buffer(line)),
    debug_message('Sending User church 8 * : Church Bot', []),
    format(WStream, "USER church 8 * : Church Bot~n", []).

register_nick(Stream) :-
    stream_pair(Stream, _, WStream),
    set_stream(WStream, buffer(line)),
    nick(Nick),
    debug_message('Sending NICK ~w', Nick),
    format(WStream, "NICK ~w~n", [Nick]).

join_bots(Stream) :- 
    stream_pair(Stream, _, WStream),
    set_stream(WStream, buffer(line)),
    debug_message('Joining #bots~n', []),
    format(WStream, "Join #bots~n", []).

handle_message(info(Code, _, Msg), _Stream) :-
    debug_message('info message(~w): ~w', [Code, Msg]).
handle_message(channel_msg(Code, _, Msg), _Stream) :-
    debug_message('channel message(~w): ~w', [Code, Msg]).
handle_message(mode(Mode), _Stream) :-
    debug_message('mode set: ~w', [Mode]).
handle_message(join(Chan), _Stream) :-
    debug_message('joined ~w', [Chan]).
handle_message(topic(_, Channel, Topic), _Stream) :-
    debug_message('topic(~w): ~w', [Channel, Topic]).
handle_message(notice(_, 'AUTH', _), Stream) :-
    auth_count(3),
    register_nick(Stream),
    register_user(Stream),
    join_bots(Stream),
    update_auth_count.
handle_message(notice(_, 'AUTH', _), _Stream) :- update_auth_count.
handle_message(ping(Reply), Stream) :-
    stream_pair(Stream, _, WStream),
    set_stream(WStream, buffer(line)),
    debug_message('Sending PONG ~w', [Reply]),
    format(WStream, "PONG :~w~n", Reply).
handle_message(X, _) :- throw(no_message_handler(X)).

read_line(_Stream, end_of_file, []) :- throw(exception(end_of_stream)).
read_line(_Stream, '\n', []) :- !.
read_line(_Stream, '\r', []) :- !.
read_line(Stream, H, [H|T]) :- 
    get_char(Stream, Char),
    read_line(Stream, Char, T).

eat_nl(_Stream, end_of_file, _) :- throw(exception(end_of_stream)).
eat_nl(Stream, C, R) :-
    (C = '\n'; C = '\r'),
    get_char(Stream, L), !,
    eat_nl(Stream, L, R).
eat_nl(_Stream, R, R).

parse_server_msg(ping(Reply)) --> 
    "PING :", nonnl(SReply), eatnl, 
    { atom_codes(Reply, SReply) }.
parse_server_msg(notice(Server, Type, Message)) -->
    ":", nonspace(SServer), " NOTICE ", nonspace(SType), " :*** ", 
    nonnl(SMessage), eatnl, { atom_codes(Server, SServer), 
    atom_codes(Type, SType), atom_codes(Message, SMessage) }.
parse_server_msg(info(Code, Server, Message)) -->
    ":", nonspace(SServer), " ", get_number(SNum), " ", nonspace(_), " ",
    maybe_colon, nonnl(SMessage), eatnl, { atom_codes(Server, SServer), 
    number_codes(Code, SNum), atom_codes(Message, SMessage),
    Code < 100 }.
parse_server_msg(channel_msg(Code, Server, Message)) -->
    ":", nonspace(SServer), " ", get_number(SCode), " ", nonspace(_), " ",
    maybe_colon, nonnl(SMessage), eatnl, { atom_codes(Server, SServer),
    number_codes(Code, SCode), atom_codes(Message, SMessage),
    Code >= 200, Code < 300 }.
parse_server_msg(mode(Mode)) -->
    { nick(Nick), atom_codes(Nick, SNick) },
    ":", SNick, " MODE ", SNick, " :", nonnl(SMode), eatnl, 
    { atom_codes(Mode, SMode) }.
parse_server_msg(join(Channel)) -->
    { nick(Nick), atom_codes(Nick, SNick) },
    ":", SNick, "!", nonspace(_), " JOIN :", nonspace(SChannel), eatnl,
    { atom_codes(Channel, SChannel) }.
parse_server_msg(topic(Server, Channel, Topic)) -->
    { nick(Nick), atom_codes(Nick, SNick) },
    ":", nonspace(SServer), " 332 ", SNick, " ", nonspace(SChannel), " :",
    nonnl(STopic), { atom_codes(Server, SServer), 
    atom_codes(Channel, SChannel), atom_codes(Topic, STopic) }.

% error cases
parse_server_msg(false) -->
    ":", nonspace(SServer), " ", get_number(SNum), " ", nonnl(SMessage),
    eatnl, { number_codes(Num, SNum), atom_codes(Message, SMessage),
    atom_codes(Server, SServer), Num >= 400, Num < 500,
    throw(irc_error(Num, Server, Message)) }.
parse_server_msg(false, Msg, _Rem) :- 
    atom_codes(M, Msg),
    throw(exception(unhandled_irc_message, M)).

maybe_colon --> ":".
maybe_colon --> [].

nonspace([X|Y]) --> [X], { \+ is_whitespace(X) }, !, nonspace(Y).
nonspace([]) --> [].
nonnl([X|Y]) --> [X], { \+ is_newline(X) }, !, nonnl(Y).
nonnl([]) --> [].

eatnl --> [X], { \+ is_newline(X) }, !, eatnl.
eatnl --> [].

is_whitespace(32).
is_whitespace(9).
is_whitespace(X) :- is_newline(X).

is_newline(10).
is_newline(13).

get_number([X|Y]) --> [X], { X >= 48, X =< 57 }, !, get_number(Y).
get_number([]) --> [].
