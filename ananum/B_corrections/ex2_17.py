import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

# ============================================================
# PARAMETERS
# ============================================================

r = 0.5
a = 0.33

T = 150.0
N = 150
h = T / N

# Initial data (convert to proportions)
S0 = 1e6
I0 = 10.0
R0 = 0.0

Pop = S0 + I0 + R0
S0 /= Pop
I0 /= Pop
R0 /= Pop

y0 = np.array([S0, I0, R0])

# ============================================================
# SIR MODEL
# ============================================================

def f(y, r, a):
    S, I, R = y
    return np.array([
        -r * S * I,
        r * S * I - a * I,
        a * I
    ])

def f_odeint(y, t, r, a):
    S, I, R = y
    return [
        -r * S * I,
        r * S * I - a * I,
        a * I
    ]

# ============================================================
# NUMERICAL SCHEMES
# ============================================================

def explicit_euler(y0, h, T, r, a):
    N = int(T/h)
    y = np.zeros((N+1, 3))
    y[0] = y0
    for n in range(N):
        y[n+1] = y[n] + h * f(y[n], r, a)
    return y

def heun(y0, h, T, r, a):
    N = int(T/h)
    y = np.zeros((N+1, 3))
    y[0] = y0
    for n in range(N):
        k1 = f(y[n], r, a)
        k2 = f(y[n] + h*k1, r, a)
        y[n+1] = y[n] + h*(k1 + k2)/2
    return y

def rk4(y0, h, T, r, a):
    N = int(T/h)
    y = np.zeros((N+1, 3))
    y[0] = y0
    for n in range(N):
        k1 = f(y[n], r, a)
        k2 = f(y[n] + h*k1/2, r, a)
        k3 = f(y[n] + h*k2/2, r, a)
        k4 = f(y[n] + h*k3, r, a)
        y[n+1] = y[n] + h*(k1 + 2*k2 + 2*k3 + k4)/6
    return y

# ============================================================
# 1) Reference solution using VERY SMALL STEP RK4
# ============================================================

t_ref = np.linspace(0, T, 5000)
y_ref = odeint(f_odeint, y0, t_ref, args=(r, a))

plt.figure()
plt.plot(t_ref, y_ref[:,0], label="S")
plt.plot(t_ref, y_ref[:,1], label="I")
plt.plot(t_ref, y_ref[:,2], label="R")
plt.legend()
plt.title("Reference solution (odeint)")
plt.show()

# ============================================================
# 2) Order study
# ============================================================

T0 = 52
T1 = 60

# Get reference initial value at T0 from odeint
y_T0 = odeint(f_odeint, y0, [0, T0], args=(r, a))[-1]

# Exact value at T1 from odeint
y_T1_exact = odeint(f_odeint, y_T0, [T0, T1], args=(r, a))[-1]
S_exact = y_T1_exact[0]

h_values = [4, 2, 1, 0.5, 0.1, 0.05, 0.01]

errors_EE = []
errors_Heun = []
errors_RK4 = []

for h in h_values:

    y_EE = explicit_euler(y_T0, h, T1-T0, r, a)
    y_Heun = heun(y_T0, h, T1-T0, r, a)
    y_RK4 = rk4(y_T0, h, T1-T0, r, a)

    errors_EE.append(abs(y_EE[-1,0] - S_exact))
    errors_Heun.append(abs(y_Heun[-1,0] - S_exact))
    errors_RK4.append(abs(y_RK4[-1,0] - S_exact))

# Log-log plot
plt.figure()
plt.loglog(h_values, errors_EE, 'o-', label="Explicit Euler")
plt.loglog(h_values, errors_Heun, 'o-', label="Heun")
plt.loglog(h_values, errors_RK4, 'o-', label="RK4")
plt.xlabel("h")
plt.ylabel("Error on S(T)")
plt.legend()
plt.title("Order study (NumPy only)")
#plt.gca().invert_xaxis()
plt.show()

# ============================================================
# 3) Control policy impact
# ============================================================

r_control = 0.4
a_control = 0.3

t_control = np.linspace(0, T, 5000)
y_control = odeint(f_odeint, y0, t_control, args=(r_control, a_control))

plt.figure()
plt.plot(t_ref, y_ref[:,1], label="I (no control)")
plt.plot(t_control, y_control[:,1], label="I (control)")
plt.legend()
plt.title("Impact of control policy")
plt.show()

plt.figure()
plt.plot(t_control, y_control[:,0], label="S")
plt.plot(t_control, y_control[:,1], label="I")
plt.plot(t_control, y_control[:,2], label="R")
plt.legend()
plt.title("Impact of control policy Solution")
plt.show()
