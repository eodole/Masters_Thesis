import numpy as np 

# generator functions 

def f_b(a): 
    b = a + a**2 + np.random.normal(0,1,1)
    return b


def f_c(a): 
    c = a**3 + np.random.normal(0,1,1) 
    return c


def f_d(c): 
    d = c**2 + 1 + np.random.normal(0,1,1)
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


def induce_missing_data(data, prob): 
    '''
    data: np array dataset to induce missingness on 
    prob: float [0,1] for prob that any one cell is missing 
    '''