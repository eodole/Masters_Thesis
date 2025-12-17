import numpy as np 
from itertools import combinations
import networkx as nx
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.impute import * 
import statsmodels.api as sm
from fancyimpute import SoftImpute
import random



# generator functions previous iteration changed 27.6 

# def f_b(a): 
#     # b = a + a**2 + np.random.normal(0,1,1) test_csv1 
#     b = 2*a + np.random.normal(0,1,1)
#     return b


# def f_c(a): 
#     # c = a**3 + np.random.normal(0,1,1)  test_csv1
#     c = 3*a + 1 + np.random.normal(0,1,1)
#     return c


# def f_d(c): 
#     # d = c**2 + 1 + np.random.normal(0,1,1) test_1 csv
#     d = -1*c + np.random.normal(0,1,1)
#     return d 

# # vectorize the generators 
# vf_b = np.vectorize(f_b) 
# vf_c = np.vectorize(f_c)
# vf_d = np.vectorize(f_d)


# # generate the full dataset 
# def generate_full_data(batch, save_data = False, file_name = None): 
#     ''' 
#     batch: size that the dataset should be 

#     save_data: boolean on if the data should be saved 

#     file_name: str to save the file name as 

#     returns data as np array 
#     '''

#     A = np.random.normal(1,1, batch)
#     B = vf_b(A)
#     C = vf_c(A)
#     D =vf_d(C)
#     data = np.transpose(np.array([A,B,C,D]))
#     if save_data: 
#         np.savetxt(f"/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/{file_name}.csv", data, delimiter=",")
#     return data 


def f_b(a, c): 
    # b = a + a**2 + np.random.normal(0,1,1) test_csv1 
    b = 2*a + 3*c + np.random.normal(0,1,1)
    return b


# def f_c(a): 
#     # c = a**3 + np.random.normal(0,1,1)  test_csv1
#     c = 3*a + 1 + np.random.normal(0,1,1)
#     return c


def f_d(b, c): 
    # d = c**2 + 1 + np.random.normal(0,1,1) test_1 csv
    d = -1*c + 5*b + np.random.normal(0,1,1)
    return d 

# vectorize the generators 
vf_b = np.vectorize(f_b) 
# vf_c = np.vectorize(f_c)
vf_d = np.vectorize(f_d)


# generate the full dataset 
def generate_full_data(batch, save_data = False, file_name = None): 
    ''' 
    batch: size that the dataset should be 

    save_data: boolean on if the data should be saved 

    file_name: str to save the file name as 

    returns data as np array 
    '''
    A = np.random.normal(1,1, batch)
    C = np.random.normal(0,1, batch)
    B = vf_b(A, C)
    D =vf_d(B,C)

    data = np.transpose(np.array([A,B,C,D]))
    if save_data: 
        np.savetxt(f"/Users/eodole/Desktop/Masters_Thesis/Py_Code/gen_data/{file_name}.csv", data, delimiter=",")
    return data 

# function to induce MCAR data with simple uniform distb
def cellwise_induce_missing_data(cell, prob): 
    '''
    cell: single cell array dataset to induce missingness on 

    prob: float [0,1] for prob that any one cell is missing 

    NOTE Renamed this function on 27.6 previously 
    was induce_missing_data

    '''
    if np.random.uniform() <= prob: 
        return None 
    else: 
        return cell 

induce_missing_data = np.vectorize(cellwise_induce_missing_data, excluded = {1, "prob"})


# function to induce MNAR data with threshold method
def mnar_threshold_data_helper(cell, threshold): 
    ''' 
    cell: single cell in dataset to induce missingness on 
    
    threshold: number that cell value should be compared to, as a rule of thumb use 1 sd above mean 
    (why? arbitrary idk)
    
    '''
    if cell < threshold: 
        return None 
    else:
        return cell 
    
mnar_threshold_data = np.vectorize(mnar_threshold_data_helper, excluded={1, "threshold"})

# maybe for mar i need to look at each row, and have one of the rows affect the others
# for example for each row if A < threshold then B is removed or something 

def mar_threshold_data(row, threshold): 
    ''' 
    Helper function for 
    np.apply_along_axis(funct, arr, axis =1, kwargs)
    '''
    # first will affect the third 
    print(row)
    if row[0] < threshold: 
        row[3] = None 
    
    return row


    


class GraphComparison:
    def __init__(self, graph_dict, node_labels):
        self.graph_dict = graph_dict
        self.composite_adj_mtrx = "" 
        self.distances = []
        self.order = []
        self.node_labels = node_labels
    

    def shared_edges(self, G1_adj, G2_adj): 
        '''
        calculates the matching edges of two graphs using thier adj matrx 
        will return a new adj mtrx 
        '''
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

    def display_composite(self, figure_name): 
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


        plt.savefig(f"{figure_name}.png")





class BatchImputation:
    '''
        ma_dataset: numpy array containing missing data 

        save: option to save or not

        folder_name: do not include leading master thesis folders or a trailing / 
        '''
    def __init__(self, ma_dataset, save = False, folder_name = ""):
        self.data = ma_dataset # dataset with missing values 
        self.save = save
        self.folder_name = folder_name
        self.imp_dataset_dict = ""
        self.thesis_path = "/Users/eodole/Desktop/Masters_Thesis"

        

    def impute(self):
        '''
        Imputes the data using 4 different methods 
        1. Complete Case Analysis: delete all rows containing NAs
        2. Mean Imputation
        3. MICE (Bayesian)
        4. MICE 10 
        5. Matrix Completion 

        optionally can save the data if object self.save is set to true

        '''
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

        ## 4 ) MICE 10 
        mice_data10 = pd.DataFrame(self.data, columns=["A","B","C","D"])
        mice_imp10 = sm.MICEData(mice_data10)
        mice_imp10.update_all(10)
        mice_data10 = mice_imp10.data
        mice_data10 = mice_data10.to_numpy()
        self.imp_dataset_dict["mice_data10"] = mice_data10


        ## 5) Matrix Completion 
        soft_imputer = SoftImpute(verbose=False)
        soft_imp_data = soft_imputer.fit_transform(self.data)
        self.imp_dataset_dict["matrix_completion"] = soft_imp_data

        if self.save: 
            ## save the different datasets. 
            np.savetxt(f"{self.thesis_path}/{self.folder_name}/complete_case.csv", complete_case, delimiter = ",", newline="\n")
            
            np.savetxt(f"{self.thesis_path}/{self.folder_name}/mean_imp_data.csv", mean_imp_data, delimiter = ",", newline="\n")
            np.savetxt(f"{self.thesis_path}/{self.folder_name}/mice_data.csv", mice_data, delimiter=",",newline="\n" )
            np.savetxt(f"{self.thesis_path}/{self.folder_name}/mtrx_completion_data.csv", soft_imp_data, delimiter=",",newline="\n" )
            
            # mean_imp_data.tofile(f"{self.thesis_path}/{self.folder_name}/mean_imp_data.csv", sep = ",")
            # complete_case.tofile(f"{self.thesis_path}/{self.folder_name}/complete_case.csv", sep = ",")
            # mice_data.tofile(f"{self.thesis_path}/{self.folder_name}/mice_data.csv", sep = ",")
            # soft_imp_data.tofile(f"{self.thesis_path}/{self.folder_name}/mtrx_completion_data.csv", sep = ",")

        return self.imp_dataset_dict


    def load(self, foldername):
        self.imp_dataset_dict = {"complete_case" : np.loadtxt(f"{self.thesis_path}/{foldername}/complete_case.csv", delimiter = ",") }
        self.imp_dataset_dict["mean_imp_data"] = np.loadtxt(f"{self.thesis_path}/{foldername}/mean_imp_data.csv", delimiter = ",")
        self.imp_dataset_dict["mice_data"] = np.loadtxt(f"{self.thesis_path}/{foldername}/mice_data.csv", delimiter = ",")
        self.imp_dataset_dict["mice_data10"] = np.loadtxt(f"{self.thesis_path}/{foldername}/mice_data10.csv", delimiter = ",")
        self.imp_dataset_dict["matrix_completion"] = np.loadtxt(f"{self.thesis_path}/{foldername}/mtrx_completion_data.csv", delimiter = ",")         
        return self.imp_dataset_dict
    


class RandomGraph: 
    """
    Want to generate random data from a causal graph 
    """ 

    def __init__(self, d_feats, connection_prob):
        self.d = d_feats
        self.p = connection_prob

        self.G, self.labels  = self.random_er_dag(self.d, self.p)
        self.A, self.W = self.assign_weights(self.G)
        

    def random_er_dag(self, v, p):
        """
        Generate a random Erdos–Rényi DAG with n nodes and edge probability p.
        """
        G = nx.DiGraph()
        G.add_nodes_from(range(v))

        # Assign a random topological ordering of nodes
        nodes = list(range(v))
        random.shuffle(nodes)

        # Only allow edges from earlier to later in this ordering
        for i in range(v):
            for j in range(i + 1, v):
                if random.random() < p:
                    G.add_edge(nodes[i], nodes[j])

        return G, nodes
    
    def assign_weights(self, G):

        A =  nx.to_numpy_array(G)
    
        r_weights = np.random.rand(A.shape[0],A.shape[1])
        
        return(A, np.multiply(A,r_weights))
    
    def gen_data(self, n_samples):
        weights = self.W
        d_feats = self.d
        data = np.ones(shape=(n_samples, d_feats))
        noise = np.random.normal(size=(n_samples, d_feats))

        return np.add(noise, np.matmul(data, weights))

    def draw(self):
        nx.draw_networkx(self.G, with_labels=True, pos=nx.planar_layout(self.G))

    def get_adj(self):
        return self.A
    
