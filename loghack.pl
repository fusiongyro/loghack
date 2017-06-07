:- use_module(library(tty)).

initial_state(player(10,10)).

main :- initial_state(State), main(State).

main(State) :-
    draw(State),
    gather_input(Input),
    once(evaluate(State, Input, NewState)),
    main(NewState).

draw(player(X,Y)) :-
    tty_clear,
    format('~T~w~T', [goto(X,Y),'@',goto(0,25)]).

gather_input(Action) :-
    get_single_char(Char),
    once(interpret(Char, Action)).

interpret(97, move_left).
interpret(101, move_right).
interpret(44, move_up).
interpret(111, move_down).
interpret(_, noop).

evaluate(player(X1, Y), move_left,  player(X0, Y)) :- succ(X0, X1).
evaluate(player(X0, Y), move_right, player(X1, Y)) :- succ(X0, X1).
evaluate(player(X, Y1), move_up,    player(X, Y0)) :- succ(Y0, Y1).
evaluate(player(X, Y0), move_down,  player(X, Y1)) :- succ(Y0, Y1).
evaluate(State, noop, State).
