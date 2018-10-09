%%% -- Mode: Prolog; --
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- use_module(library(random)).

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).

builtin(lmax(_,_)).
builtin(lmin(_,_)).
builtin(max_list(_,_)).
builtin(length(_,_)).
builtin(listavg(_,_,_)).

% Finds a element occuring the most number of times in the list.
lmax(L, M) :- lmax1(L, [], [], M).
lmax1([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax1(MMax, [], [], Max).
lmax1([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax1(T, Seen, [H|MMax], Max); lmax1(T, [H|Seen], MMax, Max)).
listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
%avg(X, P) ~ val(Avg) := findall_forward(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).
lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, Min) :- lmin(MMin, [], [], Min).
leftover([H|Seen], MMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, Min); Min=[H], !.
%minMod(X, P) ~ val(Min) := findall_forward(X,P,L), lmin(L, Min1), oneElementOfList(Min1,Min).

% Randomly picks one element from the list.
pickOneElement(List) ~ uniform(List).

%Aggregation Functions
maxMod(L) ~ val(Max) := lmax(L, Max1), pickOneElement(Max1) ~= Max.
max(L) ~ val(Max) := max_list(L, Max).
avg(L) ~ val(Avg) := length(L,Cnt), listavg(L, Cnt, Avg).
minMod(L) ~ val(Min) := lmin(L, Min1), pickOneElement(Min1) ~= Min.

% Facts
%friend(bob, ann) := true.
marks(ann, bio) ~ finite([0.6:low, 0.4:high]).
marks(ann, chem) ~ finite([0.3:low, 0.7:high]).
marks(ann, phy) ~ finite([0.1:low, 0.9:high]).
marks(ann, maths) ~ finite([0.2:low, 0.8:high]).

% Rule
grade(S) ~ finite([0.1:low, 0.9:high]) := findall_forward(M, marks(S,C) ~= M, L), write(L), writeln(' first '), maxMod(L) ~= high, findall_forward(M, marks(S,C) ~= M, L), write(L), writeln(' second '), minMod(L) ~= low.
rating(S) ~ gaussian(90, 0.1) := grade(S) ~= low. 
rating(S) ~ gaussian(120, 0.1) := grade(S) ~= high.

% Find probabilities
testRating(N) :-
query([],[],(rating(ann) ~= R, R > 120),N,P),
write('probability: '),writeln(P).

testGrade(N) :- init,
query([],[],(grade(ann) ~= high),N,P),
write('probability: '),writeln(P).
