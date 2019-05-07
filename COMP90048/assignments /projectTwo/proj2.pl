%  File     : proj2.pl
%  Author   : Peiyong Wang (peiyongw@student.unimelb.edu.au)
%  ID       : 955986

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maths Puzzles                                                                                    %                           
% A maths puzzle is a square grid of squares, each to be filled in with a                          %                           
% single digit 1–9 (zero is not permitted) satisfying these constraints:                           %                           
% each row and each column contains no repeated digits;                                            %                           
% all squares on the diagonal line from upper left to lower right contain the same value; and      %                           
% the heading of reach row and column (leftmost square in a row and topmost square in a column)    %                           
% holds either the sum or the product of all the digits in that row or column                      %                           
% Note that the row and column headings are not considered to be part of the row or column,        %                           
% and so may be filled with a number larger than a single digit.                                   %                           
% The upper left corner of the puzzle is not meaningful.                                           %                           
% When the puzzle is originally posed, most or all of the squares will be empty,                   %                           
% with the headings filled in. The goal of the puzzle is to fill in all the                        %                           
% squares according to the rules. A proper maths puzzle will have at most one solution.            %                           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(library(clpfd)).

%puzzle_solution: shows the solution of the puzzle.
puzzle_solution([Head|Tail]):-
    diag_constraint(Tail, 1, _),
    puzzle_rows_constraint(Tail),
    puzzle_columns_constriant([Head|Tail]),
    maplist(label, [Head|Tail]).

% product_constraint: takes the product of a list and only lets our domain be between 1-9 as in constraints
product_constraint([], 1).
product_constraint([Head|Tail], Value):- %Product of the elements in the row and leave the result in the value.
    product_constraint(Tail,Value1),
    Head #>= 1, Head #=< 9,
	Value #= Value1 * Head.

% sum_constraint: calculate the sum of the list in the row and bound the result to the value
sum_constraint([],0).
sum_constraint([Head|Tail], Value):-
    sum_constraint(Tail, Value1),
    Head #>= 1, Head #=< 9,
    Value #= Value1 + Head.
    

% row_constraint: check whether a row follows the sum or product constraints.
row_constraint([Head|Tail]):-
    sum_constraint(Tail, Head); product_constraint(Tail, Head).

%all_distinct_tail: check whether the tail elements are different from each other
all_distinct_tail([_|Tail]):-
    all_distinct(Tail).

%puzzle_columns_constriant: check the tail of the given puzzle (transposed) falls in the constraints
 puzzle_columns_constriant([]).
 puzzle_columns_constriant([Head|Tail]):-
    transpose([Head|Tail],[_|TransposedTail]),
    puzzle_rows_constraint(TransposedTail).

%puzzle_rows_constraint: check the tail of the given puzzle falls in the constraints
puzzle_rows_constraint([]).
puzzle_rows_constraint([HeadList|Tail]):-
    row_constraint(HeadList),
    all_distinct_tail(HeadList),
    puzzle_rows_constraint(Tail).

%diag_constraint: check whether the diag(puzzle) satisfy the restrictions.
diag_constraint([],_,_).
diag_constraint([Head|PuzzleTail],N,Diag):-
    getElem(Head, N, Diag),
    N0 #= N+1,
    diag_constraint(PuzzleTail, N0, Diag).

%Retrieve an element of a list by index.
getElem([Head|_], 0, Head).
getElem([_|Tail], Count, Elem):-
    Count1 #= Count - 1,
    getElem(Tail, Count1, Elem).