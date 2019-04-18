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
