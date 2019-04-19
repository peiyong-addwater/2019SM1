% Question One: correspond(E1, L1, E2, L2)
correspond(E1, [E1|_], E2, [E2|_]).
correspond(E1, [_|Tail1], E2, [_|Tail2]) :-
    correspond(E1, Tail1, E2, Tail2).

% append rule:
append([],C,C).
append([A|B],C,[A|BC]):- append(B,C,BC).

% Check whether all lists in a list are of the same length.
samelength([]).
samelength([_]).
samelength([E1,E2|Tail]) :-
    length(E1,N),
    length(E2,N),
    samelength([E1|Tail]).

% Question Two: interleave(Ls, L)
getHead([], Yrest,[],Yrest).
getHead([[Ele|Xs]|XXs], [Ele|Ys], [Xs|XXrest],Yrest):-
    getHead(XXs,Ys,XXrest,Yrest).
interleave([],[]).
interleave([[]|Xs],[]):-
    interleave(Xs,[]).
interleave([X|Xs], [Y|Ys]):-
    getHead([X|Xs],[Y|Ys],Xrest,Yrest),
    interleave(Xrest,Yrest).

% Question Three: partial eval(Expr0, Var, Val, Expr)
partial_eval(+(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is +(C,D));(\+ (number(C),number(D)),E = +(C,D))).
partial_eval(-(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is -(C,D));(\+ (number(C),number(D)),E = -(C,D))).
partial_eval(*(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is *(C,D));(\+ (number(C),number(D)),E = *(C,D))).
partial_eval(/(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is /(C,D));(\+ (number(C),number(D)),E = /(C,D))).
partial_eval(//(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is //(C,D));(\+ (number(C),number(D)),E = //(C,D))).
partial_eval(Expr0,Var,Val,Expr):-
    ground(Expr0),
    ground(Var),
    ground(Val),
    \+ground(Expr),
    atom(Var),
    number(Val),
    ((Expr0 = Var,Val = Expr); (Expr0 \= Var,Expr0 = Expr, (atom(Expr0); number(Expr0)))).