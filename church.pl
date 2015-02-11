:- use_module(library(socket)).

:- dynamic church_thread/1, debug/1, auth_count/1, nick/1, server_stream/2.


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
    tcp_socket(Socket),
    catch(handle_connection(Socket, Server, Port),
          Exception,
          format("exception: ~w~n", [Exception])),
    cleanup_connection(Socket).

% am using the tcp_connect/2 as CentOS 6.5 doesn't support tcp_connect/3
handle_connection(Socket, Server, Port) :-
    tcp_connect(Socket, Server:Port),
    tcp_open_socket(Socket, InStream, OutStream),
    set_stream(OutStream, buffer(line)),
    retractall(server_stream(_, _)),
    assert(server_stream(InStream, OutStream)),
    chat_to_server.

get_instream(InStream) :- server_stream(InStream, _).
get_outstream(OutStream) :- server_stream(_, OutStream).

cleanup_connection(Socket) :-
    server_stream(_InStream, OutStream),
    format(OutStream, "QUIT~n", []),
    retractall(church_thread(_)),
    retractall(server_stream(_, _)),
    tcp_close_socket(Socket).

chat_to_server :-
    get_instream(InStream),
    get_char(InStream, Char),
    eat_nl(InStream, Char, NewChar),
    read_line(InStream, NewChar, Line),!,
    maplist(atom_codes, Line, TmpStrLine),
    flatten(TmpStrLine, StrLine),
    atom_codes(DebugLine, StrLine),
    debug_message("message: ~w", [DebugLine]),
    catch(parse_server_msg(Msg, StrLine, []),
          Exception, 
          handle_error(Exception)),!,
    debug_message("~w", Msg),
    handle_message(Msg),
    chat_to_server.

append_nick(Tail) :- 
    nick(Nick),
    !, retractall(nick(_)),
    atom_concat(Nick, Tail, NewNick),
    debug_message("updating nickname to ~w", [NewNick]),
    assert(nick(NewNick)).

handle_error(irc_error(422, _, _)) :- !, debug_message("No MOTD provided", []).
handle_error(irc_error(433, _, _)) :- !, append_nick('_'), register_nick.
handle_error(irc_error(451, _, _)) :- !, register_user, join_bots.
handle_error(irc_error(462, _, _)) :-
    !, debug_message("Reregister attempted and failed", []).
handle_error(Exception) :- 
    debug_message("rethrowing: ~w", [Exception]),
    throw(Exception).

update_auth_count :-
    auth_count(X),
    retractall(auth_count(_)),
    Y is X + 1,
    assert(auth_count(Y)).

register_user :- 
    get_outstream(Stream),
    debug_message('Sending User church 8 * : Church Bot', []),
    format(Stream, "USER church 8 * : Church Bot~n", []).

register_nick :-
    get_outstream(Stream),
    nick(Nick),
    debug_message('Sending NICK ~w', Nick),
    format(Stream, "NICK ~w~n", [Nick]).

join_bots :-
    get_outstream(Stream),
    debug_message('Joining #bots~n', []),
    format(Stream, "Join #bots~n", []).

handle_message(info(Code, _, Msg)) :-
    debug_message('info message(~w): ~w', [Code, Msg]).
handle_message(channel_msg(Code, _, Msg)) :-
    debug_message('channel message(~w): ~w', [Code, Msg]).
handle_message(mode(Mode)) :- debug_message('mode set: ~w', [Mode]).
handle_message(join(Chan)) :- debug_message('joined ~w', [Chan]).
handle_message(topic(_, Channel, Topic)) :-
    debug_message('topic(~w): ~w', [Channel, Topic]).
handle_message(notice(_, 'AUTH', _)) :-
    auth_count(3),
    register_nick,
    register_user,
    join_bots,
    update_auth_count.
handle_message(notice(_, 'AUTH', _)) :- update_auth_count.
handle_message(notice(Nick, User, Channel, Notice)) :-
    debug_message('notice(~w) ~w(~w): ~w', [ Channel, Nick, User, Notice ]).
handle_message(ping(Reply)) :-
    get_outstream(Stream),
    debug_message('Sending PONG ~w', [Reply]),
    format(Stream, "PONG :~w~n", Reply).
handle_message(privmsg(Nick, User, Channel, Message)) :-
    debug_message("~w ~w(~w): ~w", [ Channel, Nick, User, Message ]).
handle_message(X) :- throw(no_message_handler(X)).

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
parse_server_msg(channel_msg(Code, Server, Message)) -->
    ":", nonspace(SServer), " ", get_number(SCode), " ", nonspace(_), " ",
    maybe_colon, nonnl(SMessage), eatnl, { atom_codes(Server, SServer),
    number_codes(Code, SCode), atom_codes(Message, SMessage),
    Code >= 200, Code < 400 }.
parse_server_msg(privmsg(Nick, User, Channel, Message)) -->
    { atom_codes('!', [C|_]) },
    ":", notchar(C, SNick), "!", nonspace(SUser), " PRIVMSG ", 
    nonspace(SChannel), " :", nonnl(SMessage), eatnl, 
    { atom_codes(Nick, SNick), atom_codes(User, SUser), 
    atom_codes(Channel, SChannel), atom_codes(Message, SMessage) }.
parse_server_msg(notice(Nick, User, Channel, Notice)) -->
    { atom_codes('!', [C|_]) }, ":", notchar(C, SNick), "!", nonspace(SUser), " NOTICE ",
    nonspace(SChannel), " :", nonspace(SNotice), eatnl,
    { atom_codes(Nick, SNick), atom_codes(User, SUser),
    atom_codes(Channel, SChannel), atom_codes(Notice, SNotice) }.

% error cases
parse_server_msg(false) -->
    ":", nonspace(SServer), " ", get_number(SNum), " ", nonnl(SMessage),
    eatnl, { number_codes(Num, SNum), atom_codes(Message, SMessage),
    atom_codes(Server, SServer), Num >= 400, Num < 500,
    throw(irc_error(Num, Server, Message)) }.
parse_server_msg(false, Msg, _Rem) :- 
    atom_codes(M, Msg),
    throw(exception(unhandled_irc_message, M)).

notchar(C, [H|T]) --> [H], { H \= C }, !, notchar(C, T).
notchar(_, []) --> [].

notcichar(C, [H|T]) --> [H], { (H >= 97 -> I is H - 32; I = H ),
                               (C >= 97 -> D is C - 32; D = C ),
                               D \= I }, !, notcichar(C, T).
notcichar(_, []) --> [].

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
