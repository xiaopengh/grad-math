import numpy as np
import matplotlib.pyplot as plt

# Parameters
lambda_ = 1j
T = 100*2*np.pi
h = 2*np.pi/100
N = int(T/h)

# Time array
t = np.linspace(0, T, N+1)

# Exact solution
x_exact = np.exp(lambda_ * t)

# ================================
# Explicit Euler (EE)
# ================================
x_EE = np.zeros(N+1, dtype=complex)
x_EE[0] = 1.0
for n in range(N):
    x_EE[n+1] = x_EE[n] + h*lambda_*x_EE[n]

# ================================
# Implicit Euler (EI)
# x_{n+1} = x_n / (1 - h*lambda)
# ================================
x_EI = np.zeros(N+1, dtype=complex)
x_EI[0] = 1.0
for n in range(N):
    x_EI[n+1] = x_EI[n] / (1 - h*lambda_)

# ================================
# Plot 1: EE real part
# ================================
plt.figure(figsize=(8,4))
plt.plot(t[0:1000], x_exact.real[0:1000], 'k--', label="Exact Re(x)")
plt.plot(t[0:1000], x_EE.real[0:1000], 'r', label="EE Re(x)")
plt.xlabel("t")
plt.ylabel("Re(x)")
plt.title("Explicit Euler: Real part")
plt.legend()
plt.grid(True)

# ================================
# Plot 2: EE imaginary part
# ================================
plt.figure(figsize=(8,4))
plt.plot(t[0:1000], x_exact.imag[0:1000], 'k--', label="Exact Im(x)")
plt.plot(t[0:1000], x_EE.imag[0:1000], 'b', label="EE Im(x)")
plt.xlabel("t")
plt.ylabel("Im(x)")
plt.title("Explicit Euler: Imaginary part")
plt.legend()
plt.grid(True)

# ================================
# Plot 3: EI real part
# ================================
plt.figure(figsize=(8,4))
plt.plot(t[0:1000], x_exact.real[0:1000], 'k--', label="Exact Re(x)")
plt.plot(t[0:1000], x_EI.real[0:1000], 'r', label="EI Re(x)")
plt.xlabel("t")
plt.ylabel("Re(x)")
plt.title("Implicit Euler: Real part")
plt.legend()
plt.grid(True)

# ================================
# Plot 4: EI imaginary part
# ================================
plt.figure(figsize=(8,4))
plt.plot(t[0:1000], x_exact.imag[0:1000], 'k--', label="Exact Im(x)")
plt.plot(t[0:1000], x_EI.imag[0:1000], 'b', label="EI Im(x)")
plt.xlabel("t")
plt.ylabel("Im(x)")
plt.title("Implicit Euler: Imaginary part")
plt.legend()
plt.grid(True)

plt.show()
