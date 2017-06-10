:- use_module(library(tty)).

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

