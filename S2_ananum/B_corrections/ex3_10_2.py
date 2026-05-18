import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier

# 1. Generate data
np.random.seed(42)
N = 500
X = np.random.rand(N, 2)

# 2. Checkerboard labeling
n_squares = 4
y = ((np.floor(X[:,0]*n_squares) + np.floor(X[:,1]*n_squares)) % 2).astype(int)

# 3. Scale
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# 4. Split
X_train, X_test, y_train, y_test = train_test_split(
    X_scaled, y, test_size=0.2, random_state=42
)

# 5. Train NN
mlp = MLPClassifier(hidden_layer_sizes=(64, 64), max_iter=300, batch_size=16, random_state=42)
mlp.fit(X_train, y_train)

# 6. Accuracy
accuracy = mlp.score(X_test, y_test)
print(f"Test Accuracy: {accuracy:.4f}")

# 7. Grid for NN prediction
xx, yy = np.meshgrid(np.linspace(0,1,50), np.linspace(0,1,50))
grid = np.c_[xx.ravel(), yy.ravel()]
grid_scaled = scaler.transform(grid)
Z = mlp.predict(grid_scaled)

# 8. Plot
plt.figure(figsize=(6,6))

# NN predictions (blurred)
plt.scatter(grid[:,0], grid[:,1], c=Z, cmap='bwr', alpha=0.3, s=15)

# Original points (true labels)
plt.scatter(X[:,0], X[:,1], c=y, cmap='bwr', edgecolor='k', alpha=0.6)

# Real borders of the checkerboard
for i in range(1, n_squares):
    plt.axvline(i/n_squares, color='green', linewidth=1.5)  # vertical lines
    plt.axhline(i/n_squares, color='green', linewidth=1.5)  # horizontal lines

plt.title("NN vs Classification Function")
plt.xlabel("x1")
plt.ylabel("x2")
plt.show()