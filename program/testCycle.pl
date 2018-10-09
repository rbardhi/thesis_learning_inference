%%% -*- Mode: Prolog; -*-
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- initialization(init).


smokes ~ finite([0.1:true, 0.9:false]) := stress~=true.
smokes ~ finite([0.3:true, 0.7:false]) := stress~=false.

stress ~ finite([0.5:true, 0.5:false]).
stress ~ finite([0.8:true, 0.2:false]) := smokes~=true.
stress ~ finite([0.3:true, 0.7:false]) := smokes~=false.
%stress ~ finite([0.5:true, 0.5:false]).


test(N) :-
        init,
        query([stress~=true],[],(smokes~=true),N,P),
        write('probability: '),writeln(P).

