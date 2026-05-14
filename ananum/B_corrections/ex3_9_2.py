import numpy as np
import matplotlib.pyplot as plt

# =========================
# Parameters for plotting
# =========================
T = 150  # Total time
N = 150  # Number of time steps
h = T / N  # Time step size
S0 = 1e6  # Initial susceptible population
I0 = 10   # Initial infected population
R0 = 0    # Initial recovered population
N_total = S0 + I0 + R0  # Total population
c0 = 1e-2  # Cost penalty coefficient
gamma = 1/3

# Initial control: beta = 0.5 / N_total (original setup)
beta = (0.5 / N_total) * np.ones(N+1)

def f(x, beta):
    S, I, R = x
    return np.array([
        -beta * S * I,
        beta * S * I - gamma * I,
        gamma * I
    ])

def jacobian_f(x, beta):
    S, I, R = x
    return np.array([
        [-beta*I, -beta*S, 0],
        [beta*I, beta*S - gamma, 0],
        [0, gamma, 0]
    ])

# =========================
# Implicit Euler step
# =========================
def implicit_step(xn, beta_n):
    x_next = xn.copy()
    for _ in range(20):
        F = x_next - xn - h*f(x_next, beta_n)
        J = np.eye(3) - h*jacobian_f(x_next, beta_n)
        delta = np.linalg.solve(J, -F)
        x_next += delta
        if np.linalg.norm(delta) < 1e-10:
            break
    return x_next


# =========================
# Cost function gradient for 1/beta penalty
# =========================
def compute_gradient_1overbeta(x, beta, lambda_):
    grad = np.zeros(N+1)
    eps = 1e-12  # Avoid divide by zero
    for n in range(N):
        S, I, R = x[n+1]
        df_dbeta = np.array([-S * I, S * I, 0])
        J = jacobian_f(x[n+1], beta[n])
        A = np.eye(3) - h * J
        temp = np.linalg.solve(A, h * df_dbeta)
        grad[n] = lambda_[n+1].dot(temp)
        grad[n] += h * (-c0 / max(beta[n]**2, eps))  # 1/beta penalty
    return grad

# =========================
# Forward and adjoint functions (same as before)
# =========================
def forward(beta):
    x = np.zeros((N+1, 3))
    x[0] = [S0, I0, R0]
    for n in range(N):
        x[n+1] = implicit_step(x[n], beta[n])
    return x

def adjoint(x, beta):
    lambda_ = np.zeros((N+1, 3))
    lambda_[N] = np.array([-1, 0, 0])  # Final state
    for n in reversed(range(N)):
        J = jacobian_f(x[n+1], beta[n])
        A = np.eye(3) - h * J
        lambda_[n] = np.linalg.solve(A.T, lambda_[n+1])
    return lambda_

# =========================
# Gradient Descent Update
# =========================
def gradient_descent_update(beta, grad, alpha=1e-3, beta_min=1e-6, beta_max=1.0):
    for n in range(N):
        beta[n] = beta[n] - alpha * grad[n]
        beta[n] = max(beta_min, min(beta_max, beta[n]))
    return beta

# =========================
# Run optimization with the original 1/beta penalty
# =========================
beta_history = []
grad_history = []

for iteration in range(100):
    x = forward(beta)
    lambda_ = adjoint(x, beta)
    grad = compute_gradient_1overbeta(x, beta, lambda_)
    beta = gradient_descent_update(beta, grad, alpha=1e-3)
    
    beta_history.append(beta.copy())
    grad_history.append(grad.copy())

beta_history = np.array(beta_history)
grad_history = np.array(grad_history)

# =========================
# Plot the results
# =========================
# Plot: Gradient and Beta over time
plt.figure(figsize=(12, 6))

# Plot gradient
plt.subplot(2, 1, 1)
plt.plot(np.linspace(0, T, N+1), grad_history[-1], label="Gradient wrt Beta")
plt.title('Gradient wrt Beta over Time (with 1/β penalty)')
plt.xlabel('Time (t)')
plt.ylabel('Gradient')

# Plot Beta
plt.subplot(2, 1, 2)
plt.plot(np.linspace(0, T, N+1), beta_history[-1], label="Beta (control)")
plt.title('Optimal Beta over Time (with 1/β penalty)')
plt.xlabel('Time (t)')
plt.ylabel('Beta')

plt.tight_layout()
plt.show()