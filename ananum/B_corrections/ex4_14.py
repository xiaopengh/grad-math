import numpy as np
import matplotlib.pyplot as plt

# Parameters
theta = 1.0
mu = 10.0
sigma = 3.0
X0 = 0.0
T = 1.0

# =========================
# 1. Euler–Maruyama simulation
# =========================
N = 100
M = 100
h = T / N
t = np.linspace(0, T, N+1)

# Brownian increments
dW = np.sqrt(h) * np.random.randn(M, N)

# EM scheme (vectorized in M)
X = np.zeros((M, N+1))
X[:, 0] = X0

for n in range(N):
    X[:, n+1] = X[:, n] + theta*(mu - X[:, n])*h + sigma*dW[:, n]

# Plot
plt.figure()
plt.plot(t, X.T, linewidth=0.8)
plt.title("Euler–Maruyama (OU process)")
plt.xlabel("t")
plt.ylabel("X(t)")
plt.grid()
plt.show()

# =========================
# 2. Milstein scheme
# =========================
# For OU: Milstein = EM (since sigma is constant)
X_milstein = X.copy()

# =========================
# 3. Exact solution
# =========================
def exact_solution(dW, theta, mu, sigma, X0, T):
    M, N = dW.shape
    h = T / N
    t = np.linspace(0, T, N+1)

    # deterministic part
    X_det = X0*np.exp(-theta*T) + mu*(1 - np.exp(-theta*T))

    # stochastic integral (correct discretization)
    weights = np.exp(-theta*(T - t[:-1]))  # aligned with dW
    Ito_int = np.sum(weights * dW, axis=1)

    return X_det + sigma * Ito_int

# =========================
# 4. Strong convergence
# =========================
Ns = [50, 100, 200, 400]
M = 2000

strong_errors = []

for N in Ns:
    h = T / N
    
    dW = np.sqrt(h) * np.random.randn(M, N)
    
    # EM
    X = np.zeros((M, N+1))
    for n in range(N):
        X[:, n+1] = X[:, n] + theta*(mu - X[:, n])*h + sigma*dW[:, n]
    
    X_num = X[:, -1]
    X_exact = exact_solution(dW, theta, mu, sigma, X0, T)
    
    error = np.mean(np.abs(X_num - X_exact))
    strong_errors.append(error)
    
    print(f"N={N}, Strong error={error:.5f}")

# Plot strong convergence
plt.figure()
plt.loglog(Ns, strong_errors, 'o-')
plt.title("Strong convergence (EM)")
plt.xlabel("N")
plt.ylabel("Error")
plt.grid()
plt.show()

# =========================
# 5. Weak convergence
# =========================
def phi(x):
    return x**2

weak_errors = []

for N in Ns:
    h = T / N
    
    dW = np.sqrt(h) * np.random.randn(M, N)
    
    # EM
    X = np.zeros((M, N+1))
    for n in range(N):
        X[:, n+1] = X[:, n] + theta*(mu - X[:, n])*h + sigma*dW[:, n]
    
    X_num = X[:, -1]
    X_exact = exact_solution(dW, theta, mu, sigma, X0, T)
    
    error = abs(np.mean(phi(X_num)) - np.mean(phi(X_exact)))
    weak_errors.append(error)
    
    print(f"N={N}, Weak error={error:.5f}")

# Plot weak convergence
plt.figure()
plt.loglog(Ns, weak_errors, 'o-')
plt.title("Weak convergence (EM)")
plt.xlabel("N")
plt.ylabel("Error")
plt.grid()
plt.show()