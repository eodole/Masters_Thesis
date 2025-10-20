import statsmodels.api as sm
from fancyimpute import SoftImpute
import pandas as pd 

def py_mice(dataset, cols):
  dataset = pd.DataFrame(dataset, columns = cols)
  imp = sm.MICEData(dataset)
  imp.update_all()
  return(imp.data)

def py_softimp(dataset): 
  imp = SoftImpute(verbose=False)
  return(imp.fit_transform(dataset))

def py_mice2(dataset, cols):
  dataset = pd.DataFrame(dataset, columns = cols)
  imp = sm.MICEData(dataset)
  imp.update_all(10)
  return(imp.data)
