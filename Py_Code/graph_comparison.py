## Libraries 

import numpy as np
import utils 
import pandas as pd 
from sklearn.impute import * 
import causallearn
import networkx as nx
import statsmodels.api as sm
from causallearn.search.ConstraintBased.PC import pc
from itertools import combinations
from fancyimpute import SoftImpute




##  1) Import Data 
ground_truth = np.loadtxt("/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/test2.csv", delimiter = ",")


## Induce Missingness 
mcar_data = utils.v_induce_missing_data(ground_truth, 0.2)


## 2) Complete Case 
complete_case =  pd.DataFrame(mcar_data,columns=["A","B","C","D"])
complete_case.dropna(axis=0, how="any", inplace=True)
complete_case = complete_case.to_numpy()

### 3)  Mean Imputation 
imp = SimpleImputer(strategy="mean")
mean_imp = imp.fit_transform(mcar_data)

## 4) MICE 
mice_data = pd.DataFrame(mcar_data, columns=["A","B","C","D"])
mice_imp = sm.MICEData(mice_data)
mice_imp.update_all()
mice_data = mice_imp.data
print(type(mice_data))
mice_data = mice_data.to_numpy()
print(type(mice_data))

## 5) Matrix Completion 
soft_imputer = SoftImpute(verbose=False)
soft_imp_data = soft_imputer.fit_transform(mcar_data)




### Create graphs 
# pc(data, 0.05, "fisherz")
# dataset_list = [ground_truth, complete_case, mean_imp, mice_data]
# for i in range(0, len(dataset_list)):
#     print(f"Dataset Number {i}")
#     data = dataset_list[i] 
#     temp = pc(data, 0.05, "fisherz")
#     temp.to_nx_graph()
#     graph_list.append(temp)
#     print(temp.G.graph)

## Helper Function
def cg_fit(dataset):
    temp = pc(dataset, 0.05, "fisherz")
    temp.to_nx_graph()
    print(temp.G.graph)
    return(temp)

# graph_dict = {"ground_truth": cg_fit(ground_truth), "complete_case": cg_fit(complete_case), 
#               "mean_imp": cg_fit(mean_imp), "mice_imp": cg_fit(mice_data), "mtrx_completion": cg_fit(soft_imp_data)}

# no ground truth
graph_dict = {"complete_case": cg_fit(complete_case), 
              "mean_imp": cg_fit(mean_imp), "mice_imp": cg_fit(mice_data), "mtrx_completion": cg_fit(soft_imp_data)}



# for(n1, g1) ,(n2, g2) in combinations(graph_dict.items(), 2): 
#     print(f"Comparing {n1} and {n2}")
#     print("Shared Edges:")
#     print(utils.shared_edges(g1.G.graph, g2.G.graph))
#     print("Graph Edit Dist:")
#     print(nx.graph_edit_distance(g1.nx_graph, g2.nx_graph))

gc = utils.GraphComparison(graph_dict, {0:"A", 1:"B",2:"C", 3:"D"})
# 
gc.fit()

print(gc.composite_adj_mtrx)
gc.display_composite()
