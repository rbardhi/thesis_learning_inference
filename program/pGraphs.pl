%%% -*- Mode: Prolog; -*-
:- use_module('../../DCRuleLearning/DC/dcpf.pl').
:- use_module('../../DCRuleLearning/DC/random/sampling.pl').
:- use_module('../../DCRuleLearning/DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- set_debug(true).
:- set_inference(backward(lazy)).
:- initialization(init).


edge(1,2) ~ finite([0.6:true, 0.4:false]).
edge(1,3) ~ finite([0.1:true, 0.9:false]).
edge(2,5) ~ finite([0.4:true, 0.6:false]).
edge(2,6) ~ finite([0.3:true, 0.7:false]).
edge(3,4) ~ finite([0.3:true, 0.7:false]).
edge(4,5) ~ finite([0.8:true, 0.2:false]).
edge(5,6) ~ finite([0.2:true, 0.8:false]).

path(X,Y) := edge(X,Y) ~= true.
path(X,Y) := edge(X,Z) ~= true, Y \== Z, path(Z,Y).

complete(X,Y) ~ finite([0.2:true, 0.8:false]) := path(X,Y). 

test(N) :-
	init,
	query([],[],(path(1,5)),N,P),
	%query([],[],(satScore(ann)~=X, X<50),N,P),
	write('probability: '),writeln(P).

test1(N) :-
	init,
	query([],[],(path(1,6)),N,P),
	%query([],[],(satScore(ann)~=X, X<50),N,P),
	write('probability: '),writeln(P).

test2(N) :-
	init,
	query([],[],(complete(1,6)~=true),N,P),
	%query([],[],(satScore(ann)~=X, X<50),N,P),
	write('probability: '),writeln(P).
