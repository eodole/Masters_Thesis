import numpy as np 
from itertools import combinations
import networkx as nx
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.impute import * 
import statsmodels.api as sm
from fancyimpute import SoftImpute



# generator functions 

def f_b(a): 
    # b = a + a**2 + np.random.normal(0,1,1) test_csv1 
    b = 2*a + np.random.normal(0,1,1)
    return b


def f_c(a): 
    # c = a**3 + np.random.normal(0,1,1)  test_csv1
    c = 3*a + 1 + np.random.normal(0,1,1)
    return c


def f_d(c): 
    # d = c**2 + 1 + np.random.normal(0,1,1) test_1 csv
    d = -1*c + np.random.normal(0,1,1)
    return d 

# vectorize the generators 
vf_b = np.vectorize(f_b) 
vf_c = np.vectorize(f_c)
vf_d = np.vectorize(f_d)


# generate the full dataset 
def generate_full_data(batch, save_data = False, file_name = None): 
    ''' 
    batch: size that the dataset should be 
    save_data: boolean on if the data should be saved 
    file_name: str to save the file name as 
    '''

    A = np.random.normal(1,1, batch)
    B = vf_b(A)
    C = vf_c(A)
    D =vf_d(C)
    data = np.transpose(np.array([A,B,C,D]))
    if save_data: 
        np.savetxt(f"./gen_data/{file_name}.csv", data, delimiter=",")
    return data 

# function to induce MCAR data with simple uniform distb
def induce_missing_data(cell, prob): 
    '''
    cell: single cell array dataset to induce missingness on 
    prob: float [0,1] for prob that any one cell is missing 
    '''
    if np.random.uniform() <= prob: 
        return None 
    else: 
        return cell 

v_induce_missing_data = np.vectorize(induce_missing_data, excluded = {2, "prob"})




class GraphComparison:
    def __init__(self, graph_dict, node_labels):
        self.graph_dict = graph_dict
        self.composite_adj_mtrx = "" 
        self.distances = []
        self.order = []
        self.node_labels = node_labels
    

    def shared_edges(self, G1_adj, G2_adj): 
    # maybe modify to just take in the causalgraph obj?
        match_array = G1_adj == G2_adj
        new_adj = np.multiply(G2_adj, match_array)
        return new_adj

    def fit(self, verbose = False):
        # make pairwise comparisons 
        for(n1, g1) ,(n2, g2) in combinations(self.graph_dict.items(), 2): 

            if verbose:
                print(n1)
                print(g1)
                print(n2)
                print(g2)

            self.order.append((n1,n2))

            if verbose: 
                print(f"Comparing {n1} and {n2}")
                print(g1.G.graph)
                print(g2.G.graph)

            se = self.shared_edges(g1.G.graph, g2.G.graph)

            if isinstance(self.composite_adj_mtrx,str):
                self.composite_adj_mtrx = se
            else: 
                self.composite_adj_mtrx += se

            if verbose:
                print("Shared Edges:")   
                print(se)

            dist = nx.graph_edit_distance(g1.nx_graph, g2.nx_graph)
            self.distances.append(dist)
            if verbose:
                print("Graph Edit Dist:")
                print(dist)
            
            print("Average Graph Editing Distance:")
            print()

    def display_composite(self): 
        G = nx.from_numpy_array(self.composite_adj_mtrx, create_using=nx.DiGraph, )
        

        # Extract edge weights
        edges = G.edges(data=True)
        weights = [d['weight'] for (_, _, d) in edges]

        # Node positions (spring layout)
        # pos = nx.spring_layout(G)
        pos = nx.planar_layout(G)
       
        # Draw nodes and labels
        nx.draw_networkx_nodes(G, pos, 
                            node_size=700,
                            node_color="white",
                            edgecolors='black',       # Outline color
                            linewidths=1.5
                            )
        nx.draw_networkx_labels(G, pos, labels=self.node_labels)

        # Draw edges with arrowstyle and width based on weights
        nx.draw_networkx_edges(
            G, pos,
            edgelist=edges,
            arrowstyle='->',
            arrowsize=30,
            width=weights,
            connectionstyle='arc3,rad=0.2',
        )

        # Optional: draw edge lab
        #nx.draw(G, with_labels = True)


        plt.savefig("composite_wo_gt.png")


class BatchImputation:
    def __init__(self, ma_dataset = "", save = False, folder_name = ""):
        self.data = ma_dataset # dataset with missing values 
        self.save = save
        self.folder_name = folder_name
        self.imp_dataset_dict = ""

    def impute(self):
        ## 1) Complete Case 
        complete_case =  pd.DataFrame(self.data,columns=["A","B","C","D"])
        complete_case.dropna(axis=0, how="any", inplace=True)
        complete_case = complete_case.to_numpy()
        self.imp_dataset_dict = {"complete_case" : complete_case}

        ### 2)  Mean Imputation 
        imp = SimpleImputer(strategy="mean")
        mean_imp_data = imp.fit_transform(self.data)
        self.imp_dataset_dict["mean_imp"] = mean_imp_data

        ## 3) MICE 
        mice_data = pd.DataFrame(self.data, columns=["A","B","C","D"])
        mice_imp = sm.MICEData(mice_data)
        mice_imp.update_all()
        mice_data = mice_imp.data
        # print(type(mice_data))
        mice_data = mice_data.to_numpy()
        # print(type(mice_data))
        self.imp_dataset_dict["mice_data"] = mice_data

        ## 4) Matrix Completion 
        soft_imputer = SoftImpute(verbose=False)
        soft_imp_data = soft_imputer.fit_transform(self.data)
        self.imp_dataset_dict["matrix_completion"] = soft_imp_data

        if self.save: 
            ## save the different datasets. 
            complete_case.tofile(f"/Users/edole/Users/eodole/Desktop/Masters_Thesis/{self.folder_name}/complete_case.csv", sep = ",")
            mean_imp_data.tofile(f"/Users/edole/Users/eodole/Desktop/Masters_Thesis/{self.folder_name}/mean_imp_data.csv", sep = ",")
            mice_data.tofile(f"/Users/edole/Users/eodole/Desktop/Masters_Thesis/{self.folder_name}/mice_data.csv", sep = ",")
            soft_imp_data.tofile(f"/Users/edole/Users/eodole/Desktop/Masters_Thesis/{self.folder_name}/mtrx_completion_data.csv", sep = ",")

        return self.imp_dataset_dict


    def load(self, folderpath):
        self.imp_dataset_dict = {"complete_case" : np.loadtxt(f"{folderpath}/complete_case.csv", delimiter = ",")}
        self.imp_dataset_dict["mean_imp_data"] = np.loadtxt(f"{folderpath}/mean_imp.csv", delimiter = ",")
        self.imp_dataset_dict["mice_data"] = np.loadtxt(f"{folderpath}/mice_data.csv", delimiter = ",")
        self.imp_dataset_dict["matrix_completion"] = np.loadtxt(f"{folderpath}/mtrx_completion_data.csv", delimiter = ",")         
