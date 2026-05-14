import numpy as np
import matplotlib.pyplot as plt

# =========================
# Parameters
# =========================
T = 1.0
N = 255
M = 100

S0 = 100
mu = 0.1
sigma = 0.25
r = 0.05
K = 110

t = np.linspace(0, T, N + 1)
h = T / N


# =========================
# Bootstrap confidence interval
# =========================
def bootstrap_mean_confidence_interval(data, num_iterations=10000, alpha=0.05):
    data = np.array(data)
    n = len(data)

    means = np.zeros(num_iterations)

    for i in range(num_iterations):
        sample = np.random.choice(data, size=n, replace=True)
        means[i] = np.mean(sample)

    means.sort()

    lower = means[int(num_iterations * (alpha / 2))]
    upper = means[int(num_iterations * (1 - alpha / 2))]

    return np.mean(means), (lower, upper)


# =========================
# 1. Exact simulation (Black–Scholes)
# =========================

# Brownian motion
dW = np.sqrt(h) * np.random.randn(M, N)
W = np.zeros((M, N + 1))
W[:, 1:] = np.cumsum(dW, axis=1)

# exact stock paths
S_exact = S0 * np.exp(
    (mu - 0.5 * sigma**2) * t[None, :] + sigma * W
)

# Plot some paths
plt.figure()
for i in range(20):
    plt.plot(t, S_exact[i], linewidth=0.8)

plt.title("Exact Black–Scholes paths")
plt.xlabel("t")
plt.ylabel("S_t")
plt.grid()
plt.show()


# =========================
# Payoff (Exact)
# =========================
payoff_exact = np.maximum(S_exact[:, -1] - K, 0)
discounted_exact = np.exp(-r * T) * payoff_exact

price_exact = np.mean(discounted_exact)
_, ci_exact = bootstrap_mean_confidence_interval(discounted_exact)

print("=== Exact simulation ===")
print("Price:", price_exact)
print("95% CI:", ci_exact)


# =========================
# 2. Euler–Maruyama scheme
# =========================

S_em = np.zeros((M, N + 1))
S_em[:, 0] = S0

dW_em = np.sqrt(h) * np.random.randn(M, N)

for n in range(N):
    S_em[:, n + 1] = (
        S_em[:, n]
        + mu * S_em[:, n] * h
        + sigma * S_em[:, n] * dW_em[:, n]
    )

# Plot EM paths
plt.figure()
for i in range(20):
    plt.plot(t, S_em[i], linewidth=0.8)

plt.title("Euler–Maruyama Black–Scholes paths")
plt.xlabel("t")
plt.ylabel("S_t")
plt.grid()
plt.show()


# =========================
# Payoff (EM)
# =========================
payoff_em = np.maximum(S_em[:, -1] - K, 0)
discounted_em = np.exp(-r * T) * payoff_em

price_em = np.mean(discounted_em)
_, ci_em = bootstrap_mean_confidence_interval(discounted_em)

print("\n=== Euler–Maruyama ===")
print("Price:", price_em)
print("95% CI:", ci_em)