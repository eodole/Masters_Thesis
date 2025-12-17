
import networkx as nx
import numpy as np 
import random
import utils
import matplotlib.pyplot as plt 

G = utils.RandomGraph(15, 0.2)

nx.draw(G.G,with_labels=True)
plt.savefig("experiment4_small.png")

data = G.gen_data(10000)

np.savetxt("/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/random_er_dag/experiment4_small_data.csv", data, delimiter=",")
np.savetxt("/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/random_er_dag/experiment4_small_true_adj.csv", G.get_adj(), delimiter=",")
