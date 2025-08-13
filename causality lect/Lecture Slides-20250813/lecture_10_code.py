import numpy as np
import pandas as pd
from causallearn.search.ScoreBased.GES import ges
# For visualization
from causallearn.utils.GraphUtils import GraphUtils
import matplotlib.image as mpimg
import matplotlib.pyplot as plt
import io

def plot_graph(g):
    pyd = GraphUtils.to_pydot(g, labels=labels)
    tmp_png = pyd.create_png(f="png")
    fp = io.BytesIO(tmp_png)
    img = mpimg.imread(fp, format='png')
    plt.axis('off')
    plt.imshow(img)
    plt.show()

# Simulate the SCM data
np.random.seed(42)
N = 1000

e1 = np.random.normal(0, 1, N)
e2 = np.random.normal(0, 1, N)
e3 = np.random.normal(0, 1, N)
e4 = np.random.normal(0, 1, N)

X1 = e1
X2 = 0.5 * X1 + e2
X3 = 0.5 * X1 + e3
X4 = 1.5 * X2 + 0.7 * X3 + e4

data = np.vstack([X1, X2, X3, X4]).T
labels = ["X1", "X2", "X3", "X4"]
df = pd.DataFrame(data, columns=labels)
data = df.to_numpy()

Record = ges(data)

# Inspect output
print(Record)

# Print graphs from the forward step
# (the package does not output all graphs, but
#  we can reconstruct them by inspecting
#  'update1' and 'update2' of Record)
print("Forward Steps")
for g in Record['G_step1']:
    plot_graph(g)
    
print("Backward Steps")
for g in Record['G_step2']:
    plot_graph(g)

