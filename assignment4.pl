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
--------------------------------------------------------- */

disarm(Adivisions, Bdivisions, Solution) :-
						  length(Adivisions, An),
						  length(Bdivisions, Bn),
						  length(Avarindexes, An),
						  length(Bvarindexes, Bn),
						  Avarindexes ins 1..An,
						  Bvarindexes ins 1..Bn,
						  all_different(Avarindexes),
						  all_different(Bvarindexes),
						  disarmHelper1(Avarindexes,Bvarindexes,Adivisions,Bdivisions,Sol),
						  solutionLabel(Sol),
						  getElementsFromSol(Sol,Adivisions,Bdivisions,Solution).

disarmHelper1([A1,A2],[B1],Adivisions,Bdivisions,[[[A1,A2],[B1]]]) :-
						element(A1,Adivisions,A1item),
						element(A2,Adivisions,A2item),
						element(B1,Bdivisions,B1item),
						A1item + A2item #= B1item.
disarmHelper1([A1],[B1,B2],Adivisions,Bdivisions,[[[A1],[B1,B2]]]) :-
                        element(A1,Adivisions,A1item),
                        element(B1,Bdivisions,B1item),
                        element(B2,Bdivisions,B2item),
						A1item #= B1item + B2item.
disarmHelper1([A1, A2|Arest], [B1|Brest], Adivisions, Bdivisions, [[[A1,A2],[B1]]|Solution]) :-
						element(A1,Adivisions,A1item),
                        element(A2,Adivisions,A2item),
                        element(B1,Bdivisions,B1item),
						A1item + A2item #= B1item,
						B1item #=< Sum,
						sumLastDisarm(Solution, Sumindex, Adivisions, Bdivisions, Divisions),
						element(Sumindex, Divisions, Sum),
						disarmHelper1(Arest,Brest,Adivisions,Bdivisions,Solution).
disarmHelper1([A1|Arest], [B1,B2|Brest], Adivisions, Bdivisions, [[[A1],[B1,B2]]|Solution]) :-
                        element(A1,Adivisions,A1item),
                        element(B1,Bdivisions,B1item),
                        element(B2,Bdivisions,B2item),
						A1item #= B1item + B2item,
						A1item #=< Sum,
                        sumLastDisarm(Solution, Sumindex, Adivisions, Bdivisions, Divisions),
						element(Sumindex, Divisions, Sum),
                        disarmHelper1(Arest,Brest,Adivisions,Bdivisions,Solution).

/* ---------------------------------------------------------
 getElementsFromSol(+Solution, +Adivisions, +Bdivisions, +Elements)

--------------------------------------------------------- */

getElementsFromSol([[A,B]],Adivisions,Bdivisions,[[Item1,Item2]]) :- getElements(A,Adivisions,Item1),
																  getElements(B,Bdivisions,Item2).
getElementsFromSol([[A,B]|Rest],Adivisions,Bdivisions,[[Item1,Item2]|Sol]) :- getElements(A,Adivisions,Item1),
																		   getElements(B,Bdivisions,Item2),
																		   getElementsFromSol(Rest,Adivisions,Bdivisions,Sol).

/* ---------------------------------------------------------
 getElements(+Indexes, +Divsions, -Elements)

--------------------------------------------------------- */

getElements([Index], Divisions, [Elem]) :- element(Index, Divisions, Elem).
getElements([Index|Rest], Divisions, [Elem|Elems]) :- element(Index, Divisions, Elem), getElements(Rest, Divisions, Elems).

/* ---------------------------------------------------------
 solutionLabel(+Solution)

--------------------------------------------------------- */

solutionLabel([[[A1item, A2item],[B1item]]]) :- label([A1item, A2item, B1item]).
solutionLabel([[[A1item],[B1item, B2item]]]) :- label([A1item, B1item, B2item]).
solutionLabel([[[A1item, A2item],[B1item]]|X]) :- label([A1item, A2item, B1item]), solutionLabel(X).
solutionLabel([[[A1item],[B1item, B2item]]|X]) :- label([A1item, B1item, B2item]), solutionLabel(X).

/* ---------------------------------------------------------
 sumLastDisarm(+Disarms, +Sum, +Adivisions, +Bdivisions, -Divisions)

--------------------------------------------------------- */

sumLastDisarm([[[_,_], [X]]|_], X, _, Bdivision, Bdivision).
sumLastDisarm([[[X], [_,_]]|_], X, Adivision, _, Adivision).
