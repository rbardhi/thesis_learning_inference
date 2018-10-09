from pyswip import Prolog

prolog = Prolog()
prolog.consult('../data/data.pl')
result = prolog.query('current_posZ(sample100, X, Y)')
for element in result:
    print element['X'], element['Y']
    pass

prolog.dynamic('current_posX/3')
prolog.dynamic('current_posY/3')
prolog.dynamic('current_posZ/3')
prolog.dynamic('next_posX/3')
prolog.dynamic('next_posY/3')
prolog.dynamic('next_posZ/3')
prolog.dynamic('move_behind_of/3')
prolog.dynamic('move_infront_of/3')
prolog.dynamic('move_left_of/3')
prolog.dynamic('move_right_of/3')

prolog.retractall('current_posX(_,_,_)')
prolog.retractall('current_posY(_,_,_)')
prolog.retractall('current_posZ(_,_,_)')
prolog.retractall('next_posX(_,_,_)')
prolog.retractall('next_posY(_,_,_)')
prolog.retractall('next_posZ(_,_,_)')
prolog.retractall('move_behind_of(_,_,_)')
prolog.retractall('move_infront_of(_,_,_)')
prolog.retractall('move_left_of(_,_,_)')
prolog.retractall('move_right_of(_,_,_)')

prolog.consult('../data/data_large.pl')

result = prolog.query('current_posZ(sample100, X, Y)')
for element in result:
    print element['X'], element['Y']
    pass

# prolog.consult('../data/data_large.pl')
# result = prolog.query('current_posZ(sample100, X, Y)')
# for element in result:
#     print element['X'], element['Y']
#     pass


