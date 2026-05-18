import numpy as np
import matplotlib.pyplot as plt

T = 10 # Final time
h = 0.01 # Step size
N = int(T/h) # Number of steps
lambda_val = 0. 
x_0 = 0. # Initial condition

def f(t,x):
    return 2 * np.sqrt(np.abs(x))

def euler_explicit(T, N, lambda_val, x_0):
    t = np.linspace(0, T, N+1)
    x = np.zeros(N+1)
    x[0] = x_0
    for n in range(N):
        x[n+1] = x[n] + h * f(t[n], x[n])
    return t, x

# def euler_implicit(T, N, lambda_val, x_0):
#     t = np.linspace(0, T, N+1)
#     x = np.zeros(N+1)
#     x[0] = x_0
#     for n in range(N):
#         # Implicit method requires solving for x[n+1]
#         # Here we use a simple fixed-point iteration to find x[n+1]
#         x_next = x[n]  # Initial guess
#         for _ in range(10):  # Iterate to find a better approximation
#             x_next = x[n] + h * f(t[n+1], x_next)
#         x[n+1] = x_next
#     return t, x

def euler_implicit(T, N, lambda_val, x_0):
    t = np.linspace(0, T, N+1)
    x = np.zeros(N+1)
    x[0] = x_0
    for n in range(N):
        # Solving directly for x[n+1] using the implicit method relation
        x[n+1] = (h + np.sqrt(h**2 + x[n]))**2
    return t, x

t_explicit, x_explicit = euler_explicit(T, N, lambda_val, x_0)
t_implicit, x_implicit = euler_implicit(T, N, lambda_val, x_0)
x_exact = np.where(t_explicit >= lambda_val, (t_explicit - lambda_val)**2, 0)

plt.figure(figsize=(12, 6))
plt.plot(t_explicit, x_explicit, label='Explicit Euler', color='blue')
plt.plot(t_implicit, x_implicit, label='Implicit Euler', color='red')
plt.plot(t_explicit, x_exact, label='Exact Solution', color='green', linestyle='--')
plt.title('Explicit vs Implicit Euler Methods')
plt.xlabel('Time')
plt.ylabel('x(t)')
plt.legend()
plt.grid()
plt.show()

# The explicit Euler method simulates the trivial solution x(t) = 0, while the implicit Euler method captures the non-trivial solution x(t) = (t - lambda)^2 for t >= lambda. This illustrates the importance of choosing the appropriate numerical method for solving ODEs, especially when dealing with non-linear equations.
