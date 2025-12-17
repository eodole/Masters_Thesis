import numpy as np 
import utils
import pandas as pd 
from sklearn.impute import * 
import causallearn
import networkx as nx
import matplotlib.pyplot as plt

from causallearn.search.ConstraintBased.PC import pc
from itertools import combinations

# Helper Function
def cg_fit(dataset):
    # temp = pc(dataset, 0.05, "fisherz")
    temp = pc(dataset, 0.05, "fisherz")
    temp.to_nx_graph()
    # temp.draw_pydot_graph(labels=["A","B","C","D"])
    pydot_graph = temp.draw_pydot_graph(labels=["A","B","C","D"])
    # plt.savefig(f'/Users/eodole/Desktop/Masters_Thesis/R_Code/stable_imputation_cd/{file_name}.png',dpi=300, bbox_inches='tight')
    # plt.close()
    return(temp)

# import data 
# ground_truth = np.loadtxt("/Users/eodole/Desktop/Masters_Thesis/R_Code/stable_imputation_cd/full_data.csv", delimiter = ",")

high_missing = pd.read_csv("/Users/eodole/Desktop/Masters_Thesis/R_Code/stable_imputation_cd/high_missing_data.csv")
# low_missing = low_missing.replace('', np.nan)
high_missing = high_missing.to_numpy()

imputer = utils.BatchImputation(high_missing, folder_name="R_code/stable_imputation_cd/high_missing_imp_data", save=True)
datasets = imputer.impute()

# datasets = {"ground_truth" : ground_truth}
# datasets["ground_truth"] = ground_truth

## 3) Create Graphs& vis
graph_dict = {}

for (name, dataset) in datasets.items():
    print(name)
    temp = cg_fit(dataset)
    #graph_dict.update({name : temp})
    #temp.draw_nx_graph()

