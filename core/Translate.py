'''
Created on Feb 21, 2018

@author: nitesh
'''

def translate_rule(rule):
    head, body = rule.split(':-')
    head, distrib = head.split('~')
    distrib,append_body = translate_distrib(distrib)
    return head+' ~ '+distrib+' := '+body.rstrip().rstrip('.')+append_body+'.'

def split_distrib(distrib):
    name, args = distrib.split('(')
    arglist = list()
    inarg = False
    for c in args:
        if c == '[':
            inarg = True
            arglist.append([''])
        elif c == ']':
            inarg = False
        else:
            if inarg:
                if c == ',':
                    arglist[-1].append('')
                else:
                    arglist[-1][-1] += c
    return name,arglist

def translate_distrib(distrib):
    distrib = distrib.strip()
    name,args = split_distrib(distrib)
    print args
    if name == 'Finite':
        probs = ['{}:{}'.format(args[1][i],args[0][i]) for i in range(len(args[0]))]
        return 'finite([{}])'.format(','.join(probs)),''
    elif name =='LogisticRegression':
        return 'finite([P:{},P2:{}])'.format(args[1][0],args[1][1]),', logreg({},{},P), P2 is 1.0-P'.format([float(x) for x in args[2]],'[{}]'.format(', '.join([x.strip() for x in args[0]])))
    return '',''

if __name__ == '__main__':
    rule = 'grade(S,C,X4) ~ Finite([high, mid], [0.7682926829268293, 0.23170731707317074]) :- difficulty(C, X1_M), X1_M == easy, teaches(P_M, C).'
    rule1 = 'molecule_mutagenic(Molecule_id,Mutagenic) ~ LogisticRegression([X_T_29, X_T_45, Lumo_M, Logp_M], [no, yes], [-0.29842607367884438, 0.42354004647336491, -0.31296410363292082, 0.18695496292973357, 9.2538202541739221]) :- molecule_ind1(Molecule_id, Ind1_M), Ind1_M == 0, max(Type_M, (atom_molecule_id(A_id_M,Molecule_id), atom_type(A_id_M, Type_M)), X_T_29), min(Type_M_1, (atom_molecule_id(A_id_M_1,Molecule_id), atom_type(A_id_M_1, Type_M_1)), X_T_45), molecule_lumo(Molecule_id, Lumo_M), molecule_logp(Molecule_id, Logp_M).'
    print translate_rule(rule1)
    
    
    
    