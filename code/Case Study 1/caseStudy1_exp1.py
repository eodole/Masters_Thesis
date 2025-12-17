#Libraries 
import utils 
import numpy as np 
import pandas as pd 
import networkx as nx 
import matplotlib.pyplot as plt
from causallearn.search.ConstraintBased.PC import pc
from causallearn.search.ConstraintBased.FCI import fci 
from causallearn.search.ScoreBased.GES import ges
from causallearn.utils.GraphUtils import GraphUtils


figure_path = "figures/pres1"
gen_data_path = "Py_Code/gen_data/pres1"




## Initial Set Up 
# ground_truth = utils.generate_full_data(1000,True, "five_percent_missing.csv")

# ## Induce Missingness 

# pd.DataFrame(missing_data).to_csv(f"{gen_data_path}/five_percent_missing.csv")

# imp  = utils.BatchImputation(missing_data, True, "Py_Code/gen_data/pres1")

# imp_dict = imp.impute()


## Load Data 

ground_truth = np.loadtxt(f"/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/presentation1.2.csv", delimiter=",")
# print(ground_truth.shape)

imp = utils.BatchImputation(folder_name=gen_data_path)
imp_dict = imp.load(gen_data_path)
# # print(imp_dict.keys())


# Draw Ground Truth Graph
H = nx.DiGraph()
H.add_weighted_edges_from([("A", "B", 2), ("C", "B", 3), ("C", "D", -1), ("B", "D", 5)])




for (name, data) in imp_dict.items(): 

    # fit with 3 algos 
    G = pc(data)
    G.to_nx_graph()

    # record the graph 
    pdy = GraphUtils.to_pydot(G.G, labels = ["A", "B", "C", "D"])
    pdy.write_png(f'{figure_path}/pc_{name}.png')


    # compare to ground truth H via GED 
    print(f"Graph Edit Distance for  PC Algo and {name} Data: ")
    print(nx.graph_edit_distance(H, G.nx_graph))


    # 2. fci 
    g, edges = fci(data)
    g = g.to_nx_graph

    # record graph
    pdy = GraphUtils.to_pydot(g, labels = ["A", "B", "C", "D"])
    pdy.write_png(f'{figure_path}/fci_{name}.png')
    print(f"Graph Edit Distance for  PC Algo and {name} Data: ")
    print(nx.graph_edit_distance(H, g.nx_graph))
        # change name 

    # 3. GES 
    Record = ges(data)
    pdy = GraphUtils.to_pydot(Record["G"], labels = ["A", "B", "C", "D"])
    pdy.write_png(f'{figure_path}/ges_{name}.png')
    print(f"Graph Edit Distance for  ges Algo and {name} Data: ")
    print(nx.graph_edit_distance(H, Record["G"].nx_graph))





