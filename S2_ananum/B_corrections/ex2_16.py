import numpy as np
import matplotlib.pyplot as plt

# ============================================================
# Problem setup
# x' = 2 sqrt(|x|)
# x(0) = 0
# ============================================================

def f(x):
    return 2 * np.sqrt(np.abs(x))

# Time parameters
T = 2.0
h = 0.01
N = int(T / h)
t = np.linspace(0, T, N+1)

# ============================================================
# 1) Explicit Euler (EE)
# ============================================================

def explicit_euler(x0):
    x = np.zeros(N+1)
    x[0] = x0
    for n in range(N):
        x[n+1] = x[n] + h * f(x[n])
    return x

# ============================================================
# 2) Implicit Euler (EI)
# x_{n+1} = x_n + 2h sqrt(x_{n+1})
# Solve analytically:
# sqrt(x_{n+1}) = h + sqrt(h^2 + x_n)
# ============================================================

def implicit_euler(x0):
    x = np.zeros(N+1)
    x[0] = x0
    for n in range(N):
        y = h + np.sqrt(h**2 + x[n])
        x[n+1] = y**2
    return x

# ============================================================
# 3) Simulations
# ============================================================

# Case A: exact zero initial condition
x_EE_zero = explicit_euler(0.0)
x_EI_zero = implicit_euler(0.0)

# Case B: small perturbation (finite precision simulation)
eps = 1e-14  # Small perturbation to simulate finite precision
x_EE_perturbed = explicit_euler(eps)

# Exact solution t^2
x_exact = t**2

# ============================================================
# Plot results
# ============================================================

plt.figure(figsize=(10,6))

plt.plot(t, x_exact, 'k--', label="Exact solution $t^2$")
plt.plot(t, x_EI_zero, label="Implicit Euler (x0 = 0)")
plt.plot(t, x_EE_zero, label="Explicit Euler (x0 = 0)")
plt.plot(t, x_EE_perturbed, label="Explicit Euler (x0 = 1e-14)")

plt.xlabel("t")
plt.ylabel("x(t)")
plt.title("Exercise 2.16 — Numerical precision effects")
plt.legend()
plt.grid(True)
plt.show()
