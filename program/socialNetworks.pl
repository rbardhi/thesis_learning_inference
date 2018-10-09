%%% -*- Mode: Prolog; -*-
:- use_module('../../DCRuleLearning/DC/dcpf.pl').
:- use_module('../../DCRuleLearning/DC/random/sampling.pl').
:- use_module('../../DCRuleLearning/DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- set_debug(true).
:- set_inference(backward(lazy)).
:- initialization(init).

%stress(X) ~ finite([0.3:true, 0.7:false]) := person(X).
%influences(X,Y) ~ finite([0.2:true, 0.8:false]) := person(X), person(Y).

%smokes(X) := stress(X)~=true.
%smokes(X) := friend(X,Y), influences(Y,X)~=true, smokes(Y).

%asthma(X) ~ finite([0.4:true, 0.6:false]) := smokes(X).


rule1 ~ finite([0.3:true, 0.7:false]).
rule2 ~ finite([0.2:true, 0.8:false]). 
rule3 ~ finite([0.4:true, 0.6:false]).

stress(X) := person(X), rule1 ~= true.

influences(X,Y) := person(X), person(Y), rule2 ~= true.

smokes(X) := stress(X).
smokes(X) := friend(X,Y), influences(Y,X), smokes(Y).
 
asthma(X) := smokes(X), rule3 ~= true.


person(1) := true.
person(2) := true.
person(3) := true.
person(4) := true.

friend(1,2) := true.
friend(2,1) := true.
friend(2,4) := true.
friend(3,2) := true.
friend(4,2) := true.





test1(N) :-
	init,
	query([],[],(smokes(1)),N,P),
	write('probability: '),writeln(P).

test2(N) :-
	init,
	query([],[],(asthama(1)),N,P),
	write('probability: '),writeln(P).





