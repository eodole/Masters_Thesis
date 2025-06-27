import statsmodels.api as sm

import pandas

from patsy import dmatrices

import numpy as np

import utils 
import pandas as pd

import itertools
from fancyimpute import SoftImpute
# x = np.random.standard_normal((1000, 2))
# x.flat[np.random.sample(2000) < 0.1] = np.nan

# print(x)
# print(x.shape)

# def model_args_fn(x):
#  # Return endog, exog from x
#     return x[:, 0], x[:, 1:]

# imp = sm.BayesGaussMI(x)

# print(imp)

# mi = sm.MI(imp, sm.OLS, model_args_fn)

# print(mi)

# print(mi.fit())

# ground_truth = np.loadtxt("/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/test2.csv", delimiter = ",")
# mcar_data = utils.v_induce_missing_data(ground_truth, 0.2)
# mcar_data = pd.DataFrame(mcar_data, columns=["A","B","C","D"])

# imp_data = sm.MICEData(mcar_data)
# imp_data.update_all()

# print(imp_data.data)

# l = {"a":1,"b":2,"c":3,"d":2}

# for (k1, v1), (k2,v2) in itertools.combinations(l.items(), 2):
#     print(f"comparing {k1} and {k2}")
#     print(v1 == v2)
    
# print(l.values())

# ##  1) Import Data 
# ground_truth = np.loadtxt("/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/test2.csv", delimiter = ",")

# ## Induce Missingness 
# X = utils.v_induce_missing_data(ground_truth, 0.2)


# soft_imputer = SoftImpute()
# # X, mask = soft_imputer.prepare_input_data(X)
# # soft_imp_data = soft_imputer.solve(X, mask)
# soft_imputer.fit_transform(X)


import pydot
from IPython.display import Image, display

# Create a directed graph in pydot
dot = pydot.Dot(graph_type='digraph')

# Define the edges with weights
edges = [("A", "B", 2), ("A", "C", 3), ("C", "D", -1)]

# Add edges and nodes with attributes
for src, dst, weight in edges:
    edge = pydot.Edge(src, dst, label=str(weight), color='red', fontsize='10')
    dot.add_edge(edge)

    # Ensure all nodes are styled
    for node_name in (src, dst):
        if not dot.get_node(node_name):
            node = pydot.Node(node_name, style="filled", fillcolor="white", color="black", shape="circle", fontsize='12')
            dot.add_node(node)

# Render and display the graph as an image
dot.write_png('graph.png')
display(Image(filename='graph.png'))
