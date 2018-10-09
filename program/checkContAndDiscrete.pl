%%% -- Mode: Prolog; --
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).


%% stress(X) ~ finite([0.1:true, 0.9:false]) := true.
rating(X) ~ gaussian(5,1) := stress(X)~=true.
rating(X) ~ gaussian(10,1) := stress(X)~=false.


test(N) :-
	init,
	query([rating(ann)~=7.5],[],(stress(ann)~=true),N,P),
	write('probability: '),writeln(P).


