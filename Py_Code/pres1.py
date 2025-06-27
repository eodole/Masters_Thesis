#Libraries 
import utils 
import numpy as np 
import pandas as pd 
import networkx as nx 
import matplotlib.pyplot as plt



figure_path = "/Users/eodole/Desktop/Masters_Thesis/figures/presentation1"
gen_data_path = "/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/pres1"




## Initial Set Up 
ground_truth = utils.generate_full_data(1000,True, "presentation1.2")

missing_data = utils.induce_missing_data(ground_truth, 0.5)
pd.DataFrame(missing_data).to_csv(f"{gen_data_path}/five_percent_missing.csv")


imp  = utils.BatchImputation(missing_data, True, "Py_Code/gen_data/pres1")

imp.impute()


## Experiment with new Ground Truth 
# ground_truth = utils.generate_full_data(1000,True, "test_pres1")

## Load Data 

# ground_truth = np.loadtxt(f"/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/presentation1.csv", delimiter=",")
# print(ground_truth.shape)

# imp = utils.BatchImputation(folder_name="Py_Code/gen_data/pres1")
# imp_dict = imp.load("Py_Code/gen_data/pres1")
# # print(imp_dict.keys())


## Draw Ground Truth Graph
H = nx.DiGraph()
H.add_weighted_edges_from([("A", "B", 2), ("A", "C",3), ("C", "D", -1)])

def draw_ground_truth():
    pos = nx.planar_layout(H)
    nx.draw_networkx_nodes(H, pos, 
                        node_size=700,
                            node_color="white",
                            edgecolors='black',       # Outline color
                            linewidths=1.5
                            )

    nx.draw_networkx_labels(H, pos )

    edge_labels = nx.get_edge_attributes(H, "weight")
    nx.draw_networkx_edges(
                H, pos,
                arrowstyle='->',
                arrowsize=30,
                # connectionstyle='arc3,rad=0.2',
                edge_color="red"
            )
    nx.draw_networkx_edge_labels(
        H, pos, edge_labels
    )

    ax = plt.gca()
    ax.margins(0.08)
    plt.axis("off")
    plt.tight_layout()
# plt.show()

# plt.savefig(f"{figure_path}/ground_truth.png")


## I can just use this to draw the ground truth on top lol 
# draw_ground_truth()

# plt.show()

## Test PC Algo 
from causallearn.search.ConstraintBased.PC import pc
G = pc(ground_truth)


from causallearn.utils.GraphUtils import GraphUtils

pdy = GraphUtils.to_pydot(G.G, labels = ["A", "B", "C", "D"])
pdy.write_png(f'{figure_path}/simple_test_pc.png')

### Test FCI Algo 

from causallearn.search.ConstraintBased.FCI import fci 

g, edges = fci(ground_truth)

from causallearn.utils.GraphUtils import GraphUtils

pdy = GraphUtils.to_pydot(g, labels = ["A", "B", "C", "D"])
pdy.write_png(f'{figure_path}/simple_test_fci.png')


### Testing GES Algo 
from causallearn.search.ScoreBased.GES import ges

# default parameters
Record = ges(ground_truth)


from causallearn.utils.GraphUtils import GraphUtils

pdy = GraphUtils.to_pydot(Record["G"], labels = ["A", "B", "C", "D"])
pdy.write_png(f'{figure_path}/simple_test_ges.png')

