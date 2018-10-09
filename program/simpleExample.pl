%%% -- Mode: Prolog; --
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).


friend(X,Y) ~ finite([0.9:true, 0.1:false]) := true.
smokes(X) ~ finite([0.2:true, 0.8:false]) := friend(X,Y)~=true.
smokes(X) ~ finite([0.4:true, 0.6:false]) := friend(X,Y)~=false.

test(N) :-
	init,
	query([],[],(smokes(ann)~=true),N,P),
	write('probability: '),writeln(P).


