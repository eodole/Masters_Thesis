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


figure_path = "/Users/eodole/Desktop/Masters_Thesis/figures/presentation1"
gen_data_path = "/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/pres1"




## Initial Set Up 
ground_truth = utils.generate_full_data(1000,True, "presentation1.2")

missing_data = utils.induce_missing_data(ground_truth, 0.05)
pd.DataFrame(missing_data).to_csv(f"{gen_data_path}/five_percent_missing.csv")


imp  = utils.BatchImputation(missing_data, True, "Py_Code/gen_data/pres1")

imp_dict = imp.impute()


## Experiment with new Ground Truth 
# ground_truth = utils.generate_full_data(1000,True, "test_pres1")

## Load Data 

# ground_truth = np.loadtxt(f"/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/presentation1.2.csv", delimiter=",")
# print(ground_truth.shape)

# imp = utils.BatchImputation(folder_name="Py_Code/gen_data/pres1")
# imp_dict = imp.load("Py_Code/gen_data/pres1")
# # print(imp_dict.keys())


# Draw Ground Truth Graph
H = nx.DiGraph()
H.add_weighted_edges_from([("A", "B", 2), ("C", "B", 3), ("C", "D", -1), ("B", "D", 5)])

# def draw_ground_truth():
#     pos = nx.planar_layout(H)
#     nx.draw_networkx_nodes(H, pos, 
#                         node_size=700,
#                             node_color="white",
#                             edgecolors='black',       # Outline color
#                             linewidths=1.5
#                             )

#     nx.draw_networkx_labels(H, pos )

#     edge_labels = nx.get_edge_attributes(H, "weight")
#     nx.draw_networkx_edges(
#                 H, pos,
#                 arrowstyle='->',
#                 arrowsize=30,
#                 # connectionstyle='arc3,rad=0.2',
#                 edge_color="red"
#             )
#     nx.draw_networkx_edge_labels(
#         H, pos, edge_labels
#     )

#     ax = plt.gca()
#     ax.margins(0.08)
#     plt.axis("off")
#     plt.tight_layout()
# plt.show()

# plt.savefig(f"{figure_path}/ground_truth.png")


## I can just use this to draw the ground truth on top lol 
# draw_ground_truth()

# plt.show()



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

    # record graph
    pdy = GraphUtils.to_pydot(g, labels = ["A", "B", "C", "D"])
    pdy.write_png(f'{figure_path}/fci_{name}.png')
        # change name 

    # 3. GES 
    Record = ges(data)
    pdy = GraphUtils.to_pydot(Record["G"], labels = ["A", "B", "C", "D"])
    pdy.write_png(f'{figure_path}/ges_{name}.png')





