% Name: William Hodgson
% Id: 1413104
% Course: Cmput 325
% Section: B1
% Assignment #4

?- use_module(library(clpfd)).

/* ---------------------------------------------------------
 Question 1

 fourSquares(+N, [-S1, -S2, -S3, -S4])

 Given an integer N greater than 0, S1, S2, S3, and S4 are 4
 non-negative integers such that S1*S1 + S2*S2 + S3*S3 + S4*S4.
 S1, S2, S3, and S4 are ordered such that S1 <= S2 <= S3 <= S4.
 
 Examples:
	20 = 0*0 + 0*0 + 2*2 + 4*4
	20 = 1*1 + 1*1 + 3*3 + 3*3
	9  = 0*0 + 0*0 + 0*0 + 3*3
	9  = 0*0 + 1*1 + 2*2 + 2*2
	87 = 1*1 + 1*1 + 2*2 + 9*9
	87 = 1*1 + 1*1 + 6*6 + 7*7
	87 = 1*1 + 5*5 + 5*5 + 6*6
	87 = 2*2 + 3*3 + 5*5 + 7*7

--------------------------------------------------------- */

fourSquares(N, [S1, S2, S3, S4]) :-
	[S1, S2, S3, S4] ins 0..sup,
	S1#=<S2,
	S2#=<S3,
	S3#=<S4,
	N #= S1*S1+S2*S2+S3*S3+S4*S4,
	label([S1, S2, S3, S4]).

/* ---------------------------------------------------------
 Question 2

 disarm(+Adivisions, +Bdivisions, -Solution)
 
 Given two lists of numbers Adivisions and Bdivisions that
 refer to the military strengths of two different countries,
 Solution is a list of monthly dismantlements where each monthl
 one country dismantles one division, and the other country
 dismantles two divisions. The solution is restricted in that
 each month the total military strength of the two countries
 must remain equal, and each months dismantlements must be
 less than or equal to the next months dismantlements.
 
 Examples:
	Adivisions: 1,1
	Bdivisions: 1,1
	Solution: false
	
	Adivisions: 1,3,3,4,6,10,12
	Bdivisions: 3,4,7,9,16
	Solution: [[[1,3],[4]],[[3,4],[7]],[[12],[3,9]],[[6,10],[16]]]
	
	Adivisions: 1,2,3,3,8,5,5
	Bdivisions: 3,6,4,4,10
	Solution: [[[1, 2], [3]], [[3, 3], [6]], [[8], [4, 4]], [[5, 5], [10]]]
--------------------------------------------------------- */

disarm([],[],[]).
disarm(Adivisions, Bdivisions, Solution) :-
	length(Adivisions, An),
	length(Bdivisions, Bn),
	% Create a variable for each number in the divisions
	length(Avarindexes, An),
	length(Bvarindexes, Bn),
	% These variables index into Adivisions and Bdivisions
	% and since they are unique and there is the same number
	% of variables as divisions, every division will be
	% assigned to a variable.
	Avarindexes ins 1..An,
	Bvarindexes ins 1..Bn,
	all_different(Avarindexes),
	all_different(Bvarindexes),
	disarmHelper(Avarindexes,Bvarindexes,Adivisions,Bdivisions,Sol),
	solutionLabel(Sol),
	% We have found a solution and labeled it,
	% Last thing is to convert the indexes to divisions.
	getElementsFromSol(Sol,Adivisions,Bdivisions,Solution).

disarmHelper([A1,A2],[B1],Adivisions,Bdivisions,[[[A1,A2],[B1]]]) :-
	element(A1,Adivisions,A1item),
	element(A2,Adivisions,A2item),
	element(B1,Bdivisions,B1item),
	A1item + A2item #= B1item.
disarmHelper([A1],[B1,B2],Adivisions,Bdivisions,[[[A1],[B1,B2]]]) :-
	element(A1,Adivisions,A1item),
	element(B1,Bdivisions,B1item),
	element(B2,Bdivisions,B2item),
	A1item #= B1item + B2item.
disarmHelper([A1, A2|Arest], [B1|Brest], Adivisions, Bdivisions, [[[A1,A2],[B1]]|Solution]) :-
	element(A1,Adivisions,A1item),
	element(A2,Adivisions,A2item),
	element(B1,Bdivisions,B1item),
	A1item + A2item #= B1item,
	B1item #=< Sum,
	sumLastDisarm(Solution, Sumindex, Adivisions, Bdivisions, Divisions),
	element(Sumindex, Divisions, Sum),
	disarmHelper(Arest,Brest,Adivisions,Bdivisions,Solution).
disarmHelper([A1|Arest], [B1,B2|Brest], Adivisions, Bdivisions, [[[A1],[B1,B2]]|Solution]) :-
	element(A1,Adivisions,A1item),
	element(B1,Bdivisions,B1item),
	element(B2,Bdivisions,B2item),
	A1item #= B1item + B2item,
	A1item #=< Sum,
	sumLastDisarm(Solution, Sumindex, Adivisions, Bdivisions, Divisions),
	element(Sumindex, Divisions, Sum),
	disarmHelper(Arest,Brest,Adivisions,Bdivisions,Solution).

/* ---------------------------------------------------------
 getElementsFromSol(+Solution, +Adivisions, +Bdivisions, -Elements)

 Given a Solution, which is a list of indexes of the form:
 [[A1,A2],[A3,A4],...] where each Ai is a list with either
 a single index or 2 indexes, and the divisions those indexes
 refer to, Elements is a list where all the indexes in Solution
 are replaced by the elements in the division lists they refer to.
--------------------------------------------------------- */

getElementsFromSol([[A,B]],Adivisions,Bdivisions,[[Item1,Item2]]) :-
	getElements(A,Adivisions,Item1),
	getElements(B,Bdivisions,Item2).
getElementsFromSol([[A,B]|Rest],Adivisions,Bdivisions,[[Item1,Item2]|Sol]) :-
	getElements(A,Adivisions,Item1),
	getElements(B,Bdivisions,Item2),
	getElementsFromSol(Rest,Adivisions,Bdivisions,Sol).

/* ---------------------------------------------------------
 getElements(+Indexes, +Divisions, -Elements)

 Given a list of indexes Indexes into the list Divisions,
 Elements is a list of the elements of Divisions at each
 of the indexes in Indexes.
--------------------------------------------------------- */

getElements([Index], Divisions, [Elem]) :- element(Index, Divisions, Elem).
getElements([Index|Rest], Divisions, [Elem|Elems]) :- element(Index, Divisions, Elem), getElements(Rest, Divisions, Elems).

/* ---------------------------------------------------------
 solutionLabel(+Solution)

 solutionLabel takes a solution to the disarm problem and
 recursively labels each element.
--------------------------------------------------------- */

solutionLabel([[[A1item, A2item],[B1item]]]) :- label([A1item, A2item, B1item]).
solutionLabel([[[A1item],[B1item, B2item]]]) :- label([A1item, B1item, B2item]).
solutionLabel([[[A1item, A2item],[B1item]]|X]) :- label([A1item, A2item, B1item]), solutionLabel(X).
solutionLabel([[[A1item],[B1item, B2item]]|X]) :- label([A1item, B1item, B2item]), solutionLabel(X).

/* ---------------------------------------------------------
 sumLastDisarm(+Disarms, -Sum, +Adivisions, +Bdivisions, -Divisions)

 Given a list of disarms, and the divisions those disarms came from,
 Sum is an index into the list of divisions Divisions that refers
 to the amount of the last disarm.
--------------------------------------------------------- */

sumLastDisarm([[[_,_], [X]]|_], X, _, Bdivision, Bdivision).
sumLastDisarm([[[X], [_,_]]|_], X, Adivision, _, Adivision).
