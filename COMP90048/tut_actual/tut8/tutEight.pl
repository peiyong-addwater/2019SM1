sumlist(List, Sum):-sumlist(List, 0, Sum).
sumlist([], Sum, Sum).
sumlist([Head|Tail], Sum0, Sum):-
	Sum1 is Sum0 + Head,
	sumlist(Tail, Sum1, Sum).	
