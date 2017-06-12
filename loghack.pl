:- use_module(library(tty)).
:- use_module(library(random)).

initial_state(player(10,10)).

main :- initial_state(State), main(State).

main(quit)  :- write('Thanks for playing!'), nl, !.
main(State) :-
    draw(State),
    gather_input(Input),
    once(evaluate(State, Input, NewState)),
    main(NewState).

draw(player(X,Y)) :-
    tty_clear,
    forall(room(Room), draw_room(Room)),
    draw_player(player(X,Y)).

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
    format('~l~T┌~`─t~*|┐', [[goto(X,Y)], Width, Width]),
    succ(Y,Y1),
    YMax is Y+Height,
    forall(between(Y1,YMax,I),
           format('~l~T│~`.t~*|│', [[goto(X,I)], Width, Width])),
    format('~l~T└~`─t~*|┘', [[goto(X,YMax)], Width, Width]).

room(room(7,6,24,10,lit)).

key_action(esc('A'), move_up).
key_action(esc('B'), move_down).
key_action(esc('D'), move_left).
key_action(esc('C'), move_right).
key_action(i,           inventory).
key_action(q,           quit).
key_action(_,           noop).

evaluate(player(X1, Y),  move_left,  player(X0, Y))  :- succ(X0, X1).
evaluate(player(X0, Y),  move_right, player(X1, Y))  :- succ(X0, X1).
evaluate(player(X,  Y1), move_up,    player(X,  Y0)) :- succ(Y0, Y1).
evaluate(player(X,  Y0), move_down,  player(X,  Y1)) :- succ(Y0, Y1).
evaluate(State, inventory, State) :-
    write('You aren''t carrying anything!'), nl.
evaluate(State, noop, State).
evaluate(_,     quit, quit).

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
    (forall(member(R0, RoomsMadeSoFar),
           \+ overlaps(Room, R0)) ->
        generate_dungeon([Room|RoomsMadeSoFar], TriesRemaining, Rooms)
    ;
        succ(TriesLeft, TriesRemaining),
        generate_dungeon(RoomsMadeSoFar, TriesLeft, Rooms)
    ).
    