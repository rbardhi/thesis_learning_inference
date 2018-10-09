import networkx as nx

from networkx.algorithms import in_degree_centrality


ensembleOfDLTFile = '../data/EnsembleOfDLTs.pl'
dependencyGraphOfPredicates = '../data/dependencyGraphOfPredicates.pl'
f = open(ensembleOfDLTFile, 'r')
data = f.read()
f.close()

listOfPredicates = ['hasLoan', 'clientDistrict', 'hasAccount', 'clientLoan', 'loanAmount', 'loanStatus','monthlyPayments', 'gender', 'age', 'freq', 'avgSalary', 'ratUrbInhab', 'avgNrWith', 'avgSumOfInc', 'avgSumOfW', 'stdMonthInc', 'stdMonthW']

data = data.split('\n')
G = nx.DiGraph()
G.add_nodes_from(listOfPredicates)


listOfEdges = []
for item in data:
    if item == '':
        continue
    item = item.split(',')
    eFirstStr = ''
    eFirst = True
    for ele in item:
        for e in listOfPredicates:
            if e in ele and eFirst:
                eFirstStr = e
                eFirst = False
            elif e in ele and not eFirst:
                listOfEdges.append(eFirstStr + ' -> ' + e + '\n')
            else:
                pass
f = open(dependencyGraphOfPredicates, 'w')
setOfEdges = sorted(set(listOfEdges), key=lambda x: listOfEdges.index(x))
f.write("digraph G {\n")
for ele in setOfEdges:
    eleTemp = ele.replace(' ', '')
    eleTemp = eleTemp.replace('\n', '')
    eleTemp = eleTemp.split('->')
    G.add_edge(eleTemp[0], eleTemp[1])
    f.write(ele)
f.write("}")
f.close()

print G.nodes
print G.edges
print G.graph
print in_degree_centrality(G)
print nx.pagerank(G, alpha=0.9)
    
