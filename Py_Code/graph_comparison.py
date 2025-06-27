## Libraries 

import numpy as np
import utils 
import pandas as pd 
from sklearn.impute import * 
import causallearn
import networkx as nx

from causallearn.search.ConstraintBased.PC import pc
from itertools import combinations


# Helper Function
def cg_fit(dataset):
    # temp = pc(dataset, 0.05, "fisherz")
    temp = pc(dataset, 0.05, "kci")
    temp.to_nx_graph()
    print(temp.G.graph)
    return(temp)


##  1) Import Data 
ground_truth = np.loadtxt("/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/test2.csv", delimiter = ",")


## Induce Missingness 
# mcar_data = utils.v_induce_missing_data(ground_truth, 0.2)

## 2 ) Impute Data/ Load Imputed Data 
imputer = utils.BatchImputation(folder_name="Experiment1")

datasets = imputer.load("Experiment1")

datasets["ground_truth"] = ground_truth




## 3) Create Grraphs 
graph_dict = {}

for (name, dataset) in datasets.items():
    temp = cg_fit(dataset)
    graph_dict.update({name : temp})


## 4) Fit Graphs
gc = utils.GraphComparison(graph_dict, {0:"A", 1:"B",2:"C", 3:"D"})
# 
gc.fit()

print(gc.composite_adj_mtrx)
gc.display_composite("exp1_test_kci")


# fit method 
# pc(data, 0.05, "fisherz")

###### To FiX 
# dataset_list = [ground_truth, complete_case, mean_imp, mice_data]
# for i in range(0, len(dataset_list)):
#     print(f"Dataset Number {i}")
#     data = dataset_list[i] 
#     temp = pc(data, 0.05, "fisherz")
#     temp.to_nx_graph()
#     graph_list.append(temp)
#     print(temp.G.graph)





# graph_dict = {"ground_truth": cg_fit(ground_truth), "complete_case": cg_fit(complete_case), 
#               "mean_imp": cg_fit(mean_imp), "mice_imp": cg_fit(mice_data), "mtrx_completion": cg_fit(soft_imp_data)}

# no ground truth
# graph_dict = {"complete_case": cg_fit(complete_case), 
#               "mean_imp": cg_fit(mean_imp), "mice_imp": cg_fit(mice_data), "mtrx_completion": cg_fit(soft_imp_data)}
# for(n1, g1) ,(n2, g2) in combinations(graph_dict.items(), 2): 
#     print(f"Comparing {n1} and {n2}")
#     print("Shared Edges:")
#     print(utils.shared_edges(g1.G.graph, g2.G.graph))
#     print("Graph Edit Dist:")
#     print(nx.graph_edit_distance(g1.nx_graph, g2.nx_graph))
