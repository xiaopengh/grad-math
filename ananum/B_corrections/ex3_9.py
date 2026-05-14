import numpy as np
import matplotlib.pyplot as plt

# =========================
# Parameters
# =========================
T = 150
N = 150
h = T / N

# Initial conditions (normalized)
S0 = 1e6
I0 = 10
R0 = 0
N_total = S0 + I0 + R0

S0 /= N_total
I0 /= N_total
R0 /= N_total

gamma = 1/3
c0 = 1e-2

# Control initialization
beta = 0.5 * np.ones(N+1)

beta_min = 1e-3
beta_max = 1.0

# =========================
# SIR model
# =========================
def f(x, beta, gamma=gamma):
    S, I, R = x
    return np.array([
        -beta * S * I,
        beta * S * I - gamma * I,
        gamma * I
    ])

def jacobian_f(x, beta):
    S, I, R = x
    return np.array([
        [-beta * I, -beta * S, 0],
        [beta * I, beta * S - gamma, 0],
        [0, gamma, 0]
    ])

# =========================
# Implicit Euler
# =========================
def implicit_step(xn, beta_n):
    x_next = xn.copy()
    for _ in range(50):
        F = x_next - xn - h * f(x_next, beta_n)
        J = np.eye(3) - h * jacobian_f(x_next, beta_n)
        delta = np.linalg.solve(J, -F)
        x_next += delta
        if np.linalg.norm(delta) < 1e-10:
            break
    return x_next

# =========================
# Forward simulation
# =========================
def forward(beta):
    x = np.zeros((N+1, 3))
    x[0] = [S0, I0, R0]
    for n in range(N):
        x[n+1] = implicit_step(x[n], beta[n])
    return x

# =========================
# Cost function
# =========================
def cost(x, beta):
    term1 = S0 - x[-1, 0]
    term2 = h * np.sum(c0 / beta[:-1])
    return term1 + term2

# =========================
# Adjoint
# =========================
def adjoint(x, beta):
    lambda_ = np.zeros((N+1, 3))
    lambda_[N] = np.array([-1, 0, 0])

    for n in reversed(range(N)):
        J = jacobian_f(x[n+1], beta[n])
        A = np.eye(3) - h * J
        lambda_[n] = np.linalg.solve(A.T, lambda_[n+1])

    return lambda_

# =========================
# Gradient
# =========================
def compute_gradient(x, beta, lambda_):
    grad = np.zeros(N+1)

    for n in range(N):
        S, I, R = x[n+1]

        df_dbeta = np.array([
            -S * I,
            S * I,
            0
        ])

        J = jacobian_f(x[n+1], beta[n])
        A = np.eye(3) - h * J

        temp = np.linalg.solve(A, h * df_dbeta)

        grad[n] = lambda_[n+1].dot(temp)

        # Cost derivative: d/dβ (c0/β) = -c0/β^2
        grad[n] += -h * c0 / (beta[n]**2)

    return grad

# =========================
# Gradient descent
# =========================
def gradient_descent(beta, n_iter=1000, alpha=5e-3):
    beta_hist = []

    for k in range(n_iter):
        x = forward(beta)
        Jval = cost(x, beta)

        lambda_ = adjoint(x, beta)
        grad = compute_gradient(x, beta, lambda_)

        # Update
        beta = beta - alpha * grad

        # Projection (important!)
        beta = np.clip(beta, beta_min, beta_max)

        beta_hist.append(beta.copy())

        if k % 100 == 0:
            print(f"Iter {k} | Cost = {Jval:.6f} | Grad norm = {np.linalg.norm(grad):.4f}")

    return beta, np.array(beta_hist), x

# =========================
# Run optimization
# =========================
beta_opt, beta_history, x = gradient_descent(beta, n_iter=5000)

# =========================
# Plots
# =========================
t = np.linspace(0, T, N+1)

# SIR dynamics
plt.figure()
plt.plot(t, x[:, 0], label="S")
plt.plot(t, x[:, 1], label="I")
plt.plot(t, x[:, 2], label="R")
plt.legend()
plt.title("SIR dynamics (optimal control)")
plt.xlabel("t")
plt.grid()
plt.show()

# Control
plt.figure()
plt.plot(t[:-1], beta_opt[:-1])
plt.title("Optimal control beta(t)")
plt.xlabel("t")
plt.grid()
plt.show()

# Beta evolution
plt.figure()
plt.imshow(beta_history.T, aspect='auto', origin='lower',
           extent=[0, len(beta_history), 0, T])
plt.colorbar(label='beta(t)')
plt.xlabel('Iteration')
plt.ylabel('Time')
plt.title('Beta evolution')
plt.show()