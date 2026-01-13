import numpy as np
from pyChemelt.utils.fitting import fit_exponential_robust

np.random.seed(0)
# true parameters
a_true = 10.0
c_true = 5.0
alpha_true = 0.5

x = np.linspace(0, 10, 50)
noise = np.random.normal(scale=0.1, size=x.shape)
y = a_true + c_true * np.exp(-alpha_true * x) + noise

print('data samples (first 5):', y[:5])

try:
    a_fit, c_fit, alpha_fit = fit_exponential_robust(x, y)
    print('Fitted parameters: a={:.4f}, c={:.4f}, alpha={:.4f}'.format(a_fit, c_fit, alpha_fit))
except Exception as e:
    print('Error during fitting:', repr(e))

# compute fitted curve and residual
try:
    y_fit = a_fit + c_fit * np.exp(-alpha_fit * x)
    rss = np.sum((y - y_fit) ** 2)
    print('RSS:', rss)
except Exception:
    pass

