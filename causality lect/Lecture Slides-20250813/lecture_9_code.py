import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import SplineTransformer
import statsmodels.api as sm
from scipy.special import expit
# From: https://github.com/amber0309/HSIC/blob/master/HSIC.py
# File needs to be in the same folder for the scipt to run (as 'hsic.py')
from hsic import hsic_gam

# Generate data through non-linear Gaussian SCM
np.random.seed(42)
n = 2000
X = np.random.normal(0, 1, n)
N_Y = np.random.normal(0, 0.05, n)
Y = expit(X) + N_Y

# Fit cubic splines and return predictions, residuals and the model
def spline_fit(x, y, num_knots=4):
    x = np.asarray(x).reshape(-1, 1)
    spline_transformer = SplineTransformer(n_knots=num_knots)
    X_transformed = spline_transformer.fit_transform(x)
    model = sm.OLS(y, X_transformed).fit()
    y_hat = model.predict(X_transformed)
    residuals = y - y_hat
    
    return y_hat, residuals, model

# Fit splines for directions
fitted_Y_X, resid_Y_X, model_Y_X = spline_fit(X, Y, num_knots=4)
fitted_X_Y, resid_X_Y, model_X_Y = spline_fit(Y, X, num_knots=4)


# Apply HSIC independence test
hsic_Y_X, thr_Y_X = hsic_gam(X.reshape(-1,1), resid_Y_X.reshape(-1,1))
hsic_X_Y, thr_X_Y = hsic_gam(Y.reshape(-1,1), resid_X_Y.reshape(-1,1))

# If hsic score < threshold => independence holds for alpha = 0.05
print(f"Score X to Y: {hsic_Y_X} < {thr_Y_X} is {hsic_Y_X < thr_Y_X}")
print(f"Score Y to X: {hsic_X_Y} < {thr_X_Y} is {hsic_X_Y < thr_X_Y}")

# Practical approach to decide for direction (forced decision)
if hsic_Y_X < hsic_X_Y:
    print("X --> Y")
else:
    print("Y --> X")

# Plot results
X_sorted = np.argsort(X)
Y_sorted = np.argsort(Y)

# Plotting
fig, axes = plt.subplots(2, 2, figsize=(12, 8))

# Top-left: Y ~ X with fit
axes[0, 0].scatter(X, Y, alpha=0.3, label="Data")
axes[0, 0].plot(X[X_sorted], fitted_Y_X[X_sorted], color='red', label='Spline Fit')
axes[0, 0].set_title("$\\hat{{f}}_Y$ fitted with cubic splines")
axes[0, 0].set_xlabel("X")
axes[0, 0].set_ylabel("Y")
axes[0, 0].legend()

# Top-right: X ~ Y with fit
axes[0, 1].scatter(Y, X, alpha=0.3, label="Data")
axes[0, 1].plot(Y[Y_sorted], fitted_X_Y[Y_sorted], color='black', label='Spline Fit')
axes[0, 1].set_title("$\\hat{{f}}_X$ fitted with cubic splines")
axes[0, 1].set_xlabel("Y")
axes[0, 1].set_ylabel("X")
axes[0, 1].legend()

# Bottom-left: Residuals Y ~ X vs X
axes[1, 0].scatter(X, resid_Y_X, alpha=0.3)
axes[1, 0].set_title(f"Residuals ($Y - \\hat{{f}}_Y(X)$) vs X (HSIC indep. for $\\alpha = 0.05$)")
axes[1, 0].set_xlabel("X")
axes[1, 0].set_ylabel("Residuals ($Y - \\hat{{f}}_Y(X)$)")

# Bottom-right: Residuals X ~ Y vs Y
axes[1, 1].scatter(Y, resid_X_Y, alpha=0.3)
axes[1, 1].set_title(f"Residuals ($X - \\hat{{f}}_X(Y)$) vs Y (HSIC not indep. for $\\alpha = 0.05$)")
axes[1, 1].set_xlabel("Y")
axes[1, 1].set_ylabel("Residuals ($X - \\hat{{f}}_X(Y)$)")

plt.tight_layout()
plt.show()
