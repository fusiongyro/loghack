:- use_module(library(tty)).
:- use_module(library(random)).

initial_state(state(player(10,10), Rooms)) :- generate_dungeon(Rooms).

main :- initial_state(State), main(State).

main(quit)  :- tty_clear, write('Thanks for playing!'), nl, !.
main(State) :-
    draw(State),
    gather_input(Input),
    once(evaluate(State, Input, NewState)),
    main(NewState).

draw(state(Player, Rooms)) :-
    tty_clear,
    maplist(draw_room, Rooms),
    draw_player(Player).

draw_player(player(X,Y)) :-
    format('~T~w~T', [goto(X,Y), '☻', goto(0,25)]).

gather_input(Action) :-
    get_single_char(Start),
    (Start = 27 ->
        get_single_char(91),
        get_single_char(Code), char_code(Char, Code),
        Key = esc(Char)
    ;
        char_code(Key, Start)
    ),
    once(key_action(Key, Action)).

draw_room(room(X,Y,Width,Height,Lit)) :-
    format(string(R1), '┌~`─t~*|┐', [Width, Width]),
    format('~T~w', [goto(X,Y), R1]),
    succ(Y,Y1),
    YMax is Y+Height,
    forall(between(Y1,YMax,I),
           (format(string(RI), '│~`.t~*|│', [Width, Width]),
               format('~T~w', [goto(X,I), RI]))),
    format(string(RMax), '└~`─t~*|┘', [Width, Width]),
    format('~T~w', [goto(X,YMax), RMax]).

room(room(7,6,24,10,lit)).

key_action(esc('A'), move(up)).
key_action(esc('B'), move(down)).
key_action(esc('D'), move(left)).
key_action(esc('C'), move(right)).
key_action(i,           inventory).
key_action(q,           quit).
key_action(_,           noop).

evaluate(state(Player, Rooms), move(Dir), state(Player1, Rooms)) :- evaluate(Player, move(Dir), Player1).
evaluate(State, noop, State).
evaluate(_,     quit, quit).
evaluate(State, inventory, State) :-
    write('You aren''t carrying anything!'), nl.

evaluate(player(X1, Y),  move(left),  player(X0, Y))  :- succ(X0, X1).
evaluate(player(X0, Y),  move(right), player(X1, Y))  :- succ(X0, X1).
evaluate(player(X,  Y1), move(up),    player(X,  Y0)) :- succ(Y0, Y1).
evaluate(player(X,  Y0), move(down),  player(X,  Y1)) :- succ(Y0, Y1).

generate_dungeon(Rooms) :- generate_dungeon([], 10, Rooms).

overlaps(room(X1, Y1, W1, H1, _), room(X3, Y3, W3, H3, _)) :-
    X2 is X1 + W1, Y2 is Y1 + H1,
    X4 is X3 + W3, Y4 is Y3 + H3,
    X3 =< X2, Y3 =< Y2, X1 =< X4, Y1 =< Y4.

generate_room(room(X,Y,Width,Height,Lit)) :-
    random_between(2,40,Width),
    random_between(2,20,Height),
    XMax is 80 - Width,
    YMax is 25 - Height,
    random_between(0,XMax,X),
    random_between(0,YMax,Y),
    random_member(Lit, [lit,unlit]).

generate_dungeon(Rooms, 0, Rooms).
generate_dungeon(RoomsMadeSoFar, TriesRemaining, Rooms) :-
    generate_room(Room),
    % if the new room doesn't overlap, 
    (forall(member(R0, RoomsMadeSoFar),
            \+ overlaps(Room, R0)) ->
        % append it to the ist of rooms and proceed
        generate_dungeon([Room|RoomsMadeSoFar], TriesRemaining, Rooms)
    ;
        % otherwise, decrement the tries remaining and try again
        succ(TriesLeft, TriesRemaining),
        generate_dungeon(RoomsMadeSoFar, TriesLeft, Rooms)
    ).

room_render_test :-
    tty_clear,
    generate_dungeon(Rooms),
    maplist(draw_room, Rooms),
    format('~T', [goto(0,25)]).
