start([0, 0]).
touchdown([3, 4]).
orc([1,1]).
orc([1,0]).
orc([0,1]).
human([3,3]).

cost(From, Cost) :-
    human(From),
    Cost is 0;
    not(human(From)),
    Cost is 1.
    
dfs(From, To, _, [(From, To)], C):-
	adjacent(From, To),
    cost(From, C).

dfs(From, To, VisitedNodes, [(From, X)|TailPath], Cost):-
    cost(From, C),
    adjacent(From, X), 
    not(member(X, VisitedNodes)),
    dfs(X, To, [From|VisitedNodes], TailPath, Cost1),
    Cost is Cost1 + C.

free_place(Pos) :-
    not(orc(Pos)),
    in_map(Pos).

throw_up([X1, Y1], [X2, Y2]) :-
	X3 is X1,
    Y3 is Y1 + 1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_up([X3, Y3], [X2, Y2])
    ).

throw_down([X1, Y1], [X2, Y2]) :-
	X3 is X1,
    Y3 is Y1 - 1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_down([X3, Y3], [X2, Y2])
    ).
    
throw_left([X1, Y1], [X2, Y2]) :-
	X3 is X1 - 1,
    Y3 is Y1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_left([X3, Y3], [X2, Y2])
    ).

throw_right([X1, Y1], [X2, Y2]) :-
	X3 is X1 + 1,
    Y3 is Y1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_right([X3, Y3], [X2, Y2])
    ).

throw_up_right([X1, Y1], [X2, Y2]) :-
	X3 is X1 + 1,
    Y3 is Y1 + 1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_up_right([X3, Y3], [X2, Y2])
    ).

throw_down_right([X1, Y1], [X2, Y2]) :-
	X3 is X1 + 1,
    Y3 is Y1 - 1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_down_right([X3, Y3], [X2, Y2])
    ).

throw_down_left([X1, Y1], [X2, Y2]) :-
	X3 is X1 - 1,
    Y3 is Y1 - 1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_down_left([X3, Y3], [X2, Y2])
    ).

throw_up_left([X1, Y1], [X2, Y2]) :-
	X3 is X1 - 1,
    Y3 is Y1 + 1,
    free_place([X3, Y3]),
    (   
    	human([X3, Y3]) ->  
    		X2 is X3,
        	Y2 is Y3;
    		throw_up_left([X3, Y3], [X2, Y2])
    ).


pass([X1, Y1], [X2, Y2]) :-	   throw_up([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_down([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_right([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_left([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_up_left([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_down_left([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_down_right([X1, Y1], [X2, Y2]).
pass([X1, Y1], [X2, Y2]) :-    throw_up_right([X1, Y1], [X2, Y2]).

adjacent([X1, Y1], [X2, Y2]) :-
    (   
    	adj([X1, Y1], [X2, Y2])
    	;
    	pass([X1, Y1], [X2, Y2])
        
    ),
    not(orc([X2, Y2])).

backtrack(Length, Path) :-
    start(S),
    touchdown(T),
    setof([Cost, Path], dfs(S, T, [], Path, Cost), Bag),
    gethead(Bag, L, P),
    Length = L,
    Path = P.



random_search(Length, Path) :-
    start(S),
    touchdown(T),
    setof([Cost, RPath], random_move(S, T, [], RPath, Cost, 0), Bag),
    gethead(Bag, L, P),
    Length = L,
    Path = P.

random_move(From, To, _, [(From, To)], C, _) :-
    adjacent(From, To),
    cost(From, C).

random_move(From, To, Visited, [(From, T)|Tail], Cost, Iter) :-
    Iter < 100 ->  
    (   
    bagof(X, adjacent(From, X), Bag),
    random_member(T, Bag),
    cost(From, C),
    It is Iter + 1,
    random_move(T, To, [From|Visited], Tail, Cost1, It),
    Cost is Cost1 + C).

greedy_search(Length, Path) :-
    start(S),
    touchdown(T),
    setof([Cost, GPath], greedy_move(S, T, [], GPath, Cost), Bag),
    gethead(Bag, L, P),
    Length = L,
    Path = P.

last_greedy_move(From, To, _, [(From, To)], C) :-
    adjacent(From, To),
    cost(From, C).

greedy_move(From, To, Visited, [(From, P)|Tail], Cost) :-
    (   (   touchdown(T),
    T = To) ->
    	last_greedy_move(From, To, Visited, [(From, P)|Tail], Cost)),!;
    (setof([L, P], nearest_member(From, [L, P]), Bag),
    gethead(Bag, L, P),
    cost(From, C),
    greedy_move(P, To, [From|Visited], Tail, Cost1),
    Cost is Cost1 + C).

nearest_member([X1, Y1], Pair) :-
    touchdown([XT, YT]),
    adjacent([X1, Y1], [X, Y]),
    DX is X-XT, DY is Y-YT,
    absolute(DX, ADX),
    absolute(DY, ADY),
    L is ADX + ADY,
    Pair = [L, [X, Y]].
    


gethead([[L,P]|_], Len,Path):-
    Len = L,
    Path = P.

adj([0, 0], [1, 0]).
adj([0, 0], [0, 1]).
adj([1, 0], [0, 0]).
adj([0, 1], [0, 0]).
adj([0, 1], [1, 1]).
adj([0, 1], [0, 2]).
adj([1, 1], [0, 1]).
adj([0, 2], [0, 1]).
adj([0, 2], [1, 2]).
adj([0, 2], [0, 3]).
adj([1, 2], [0, 2]).
adj([0, 3], [0, 2]).
adj([0, 3], [1, 3]).
adj([0, 3], [0, 4]).
adj([1, 3], [0, 3]).
adj([0, 4], [0, 3]).
adj([1, 0], [2, 0]).
adj([1, 0], [1, 1]).
adj([2, 0], [1, 0]).
adj([1, 1], [1, 0]).
adj([1, 1], [2, 1]).
adj([1, 1], [1, 2]).
adj([2, 1], [1, 1]).
adj([1, 2], [1, 1]).
adj([1, 2], [2, 2]).
adj([1, 2], [1, 3]).
adj([2, 2], [1, 2]).
adj([1, 3], [1, 2]).
adj([1, 3], [2, 3]).
adj([1, 3], [1, 4]).
adj([2, 3], [1, 3]).
adj([1, 4], [1, 3]).
adj([2, 0], [3, 0]).
adj([2, 0], [2, 1]).
adj([3, 0], [2, 0]).
adj([2, 1], [2, 0]).
adj([2, 1], [3, 1]).
adj([2, 1], [2, 2]).
adj([3, 1], [2, 1]).
adj([2, 2], [2, 1]).
adj([2, 2], [3, 2]).
adj([2, 2], [2, 3]).
adj([3, 2], [2, 2]).
adj([2, 3], [2, 2]).
adj([2, 3], [3, 3]).
adj([2, 3], [2, 4]).
adj([3, 3], [2, 3]).
adj([2, 4], [2, 3]).
adj([3, 0], [4, 0]).
adj([3, 0], [3, 1]).
adj([4, 0], [3, 0]).
adj([3, 1], [3, 0]).
adj([3, 1], [4, 1]).
adj([3, 1], [3, 2]).
adj([4, 1], [3, 1]).
adj([3, 2], [3, 1]).
adj([3, 2], [4, 2]).
adj([3, 2], [3, 3]).
adj([4, 2], [3, 2]).
adj([3, 3], [3, 2]).
adj([3, 3], [4, 3]).
adj([3, 3], [3, 4]).
adj([4, 3], [3, 3]).
adj([3, 4], [3, 3]).

in_map([0,0]).
in_map([0,1]).
in_map([0,2]).
in_map([0,3]).
in_map([0,4]).
in_map([1,0]).
in_map([1,1]).
in_map([1,2]).
in_map([1,3]).
in_map([1,4]).
in_map([2,0]).
in_map([2,1]).
in_map([2,2]).
in_map([2,3]).
in_map([2,4]).
in_map([3,0]).
in_map([3,1]).
in_map([3,2]).
in_map([3,3]).
in_map([3,4]).
in_map([4,0]).
in_map([4,1]).
in_map([4,2]).
in_map([4,3]).
in_map([4,4]).

absolute(-4,4).
absolute(-3,3).
absolute(-2,2).
absolute(-1,1).
absolute(4,4).
absolute(3,3).
absolute(2,2).
absolute(1,1).
absolute(0,0).