import numpy as np
import matplotlib.pyplot as plt

# Parameters
T = 1
N = 500
h = T / N

# Time grid
t = np.linspace(0, T, N+1)

# Brownian increments
dW = np.sqrt(h) * np.random.randn(N)

# Brownian path
W = np.zeros(N+1)
W[1:] = np.cumsum(dW)

# Plot
plt.plot(t, W)
plt.title("One realization of Brownian motion")
plt.xlabel("t")
plt.ylabel("W(t)")
plt.grid()
plt.show()

M = 50  # number of paths

# Generate all increments at once
dW = np.sqrt(h) * np.random.randn(M, N)

# Cumulative sum along time axis
W = np.zeros((M, N+1))
W[:, 1:] = np.cumsum(dW, axis=1)

# Plot all paths
for i in range(M):
    plt.plot(t, W[i], linewidth=0.8)

plt.title("50 realizations of Brownian motion")
plt.xlabel("t")
plt.ylabel("W(t)")
plt.grid()
plt.show()


# Function to compute Ito integral
def ito_integral(W, dW):
    return np.sum(W[:, :-1] * dW, axis=1)

# Test for different N values
Ns = [100, 500, 1000]
M = 1000  # number of simulations

for N in Ns:
    h = T / N
    
    # Simulate Brownian motions
    dW = np.sqrt(h) * np.random.randn(M, N)
    W = np.zeros((M, N+1))
    W[:, 1:] = np.cumsum(dW, axis=1)
    
    # Numerical Ito integral
    I_num = ito_integral(W, dW)
    
    # Exact formula
    W_T = W[:, -1]
    I_exact = 0.5 * W_T**2 - 0.5 * T
    
    # Compare means
    error = np.mean(np.abs(I_num - I_exact))
    
    print(f"N = {N}, Mean error = {error:.5f}")