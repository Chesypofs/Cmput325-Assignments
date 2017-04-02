% Name: William Hodgson
% Id: 1413104
% Course: Cmput 325
% Section: B1
% Assignment #3

/* -------------------------------------------------------------------
 Question 1

 xreverse(+L, ?R)

 Given a list L, R is a list that is the reverse of list L.
------------------------------------------------------------------- */

xreverse([],[]).
xreverse([Item|End1], List) :- xreverse(End1, End2), removeLast(List, End2, Item).

/* -------------------------------------------------------------------
 Question 2

 xunique(+L, ?O)

 Given a list L of atoms, O is a list of the unique elements in L,
 in the order that they first appear.
 Uses xuniqueHelper(+L, ?O, []) where [] is used to keep track of
 the atoms that have already been found.
------------------------------------------------------------------- */

xunique(A, B) :- xuniqueHelper(A, B, []).

xuniqueHelper([], [], _).
% This is the first time we have seen Item.
xuniqueHelper([Item|End1], [Item|End2], List) :-
	xuniqueHelper(End1, End2, [Item|List]),
	notInList(Item, End2).
% Item has already been seen before, it should be in List3.
xuniqueHelper([Item|End1], List2, List3) :-
	inList(Item, List3),
	xuniqueHelper(End1, List2, List3).

/* -------------------------------------------------------------------
 inList(+A, +L)

 True if atom A is in the list L.

 notInList(+A, +L)

 True if atom A is not in the list L.
------------------------------------------------------------------- */

inList(Item, [Item|_]).
inList(Item, [_|L]) :- inList(Item, L).

notInList(_, []).
notInList(Item1, [Item2|L]) :- Item1 \= Item2, notInList(Item1, L).

/* -------------------------------------------------------------------
 Question 3

 xdiff(+L1, +L2, -L)

 Given lists L1 and L2 of atoms, L is a list that contains the
 elements that are contained in L1 but not L2.
------------------------------------------------------------------- */

xdiff(List1, List2, List3) :- subtract(List1, List2, Sub), xunique(Sub, List3).

/* -------------------------------------------------------------------
 Question 4

 removeLast(+L, ?L1, ?Last)

 Given a non-empty list L, L1 is a list that is the result of removing
 the last element of list L, and Last is the last element of list L.
------------------------------------------------------------------- */

removeLast([Item], [], Item).
removeLast([Item|End1], [Item|End2], Item2) :- removeLast(End1, End2, Item2).

/* -------------------------------------------------------------------
 Question 5.1

 allConnected(+L)

 True if all elements in the list L are nodes and there is an edge
 between any two nodes.

 notAllConnected(+L)

 True if not all elements in the list L are nodes or there is not an
 edge between two different nodes in the list L.
------------------------------------------------------------------- */

allConnected([_]).
allConnected([A|L]) :- connect(A, L), allConnected(L).

notAllConnected([A|L]) :- allConnected(L), connect(A, L) -> fail; true.

/* -------------------------------------------------------------------
 connect(+A, +L)

 True if A and the elements of L are nodes, and there is an edge
 between A and each element in L.
------------------------------------------------------------------- */

connect(_, []).
connect(A, [B|L]) :- edge(A,B), connect(A, L).
connect(A, [B|L]) :- edge(B,A), connect(A, L).

/* -------------------------------------------------------------------
 Question 5.2

 maxclique(+N, -L)

 Given an integer N, L is a list of maximal cliques of size N.
------------------------------------------------------------------- */

maxclique(N, L) :- findall(X, maxcliquecond1(N, X) , L).

% cond1 ensures that the lists that are found are maximal cliques
maxcliquecond1(N1, L1) :-
	length(L1, N1),
	clique(L1),
	N2 is N1 + 1,
	findall(X, maxcliquecond2(N2, L1, X), []).

% cond2 ensures that there are no larger cliques that contain clique L1
maxcliquecond2(N, L1, L2) :- length(L2, N), clique(L2), xsubset(L1, L2).

/* -------------------------------------------------------------------
 clique(+L)

 L is a list of nodes such that there is an edge between any two nodes.

 xsubset(+L1, +L2)

 True if all the elements in list L1 are contained in list L2.

 xappend(+L1, +L2, ?L3)

 List L3 is the result of appending the elements of list L1 onto
 list L2.
------------------------------------------------------------------- */

clique(L) :- findall(X,node(X),Nodes), xsubset(L,Nodes), allConnected(L).

xsubset([], _).
xsubset([X|Xs], Set) :- xappend(_, [X|Set1], Set), xsubset(Xs, Set1).

xappend([], L, L).
xappend([H|T], L, [H|R]) :- xappend(T, L, R).
