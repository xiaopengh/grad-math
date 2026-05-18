import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import accuracy_score

# 1 Generate a synthetic dataset
np.random.seed(42)
N = 500
X = np.random.rand(N, 2)  # points in [0,1] x [0,1]

# 2 Define a function for labeling
# Example: classify points inside a circle of radius 0.4 centered at (0.5, 0.5)
center = np.array([0.5, 0.5])
radius = 0.4
distances = np.linalg.norm(X - center, axis=1)
y = (distances < radius).astype(int)  # inside circle = class 1, outside = class 0

# 3 Scale features
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# 4 Split dataset
X_train, X_test, y_train, y_test = train_test_split(
    X_scaled, y, test_size=0.2, random_state=42
)

# 5 Train a small neural network
mlp = MLPClassifier(hidden_layer_sizes=(8, 8), max_iter=300, batch_size=16, random_state=42)
mlp.fit(X_train, y_train)

# 6 Evaluate
accuracy = mlp.score(X_test, y_test)
print(f"Test Accuracy: {accuracy:.4f}")

# 7 Plot real function vs NN prediction
# Lightweight NN plot
xx, yy = np.meshgrid(np.linspace(0, 1, 20), np.linspace(0, 1, 20))
grid = np.c_[xx.ravel(), yy.ravel()]
grid_scaled = scaler.transform(grid)
Z = mlp.predict(grid_scaled)

plt.figure(figsize=(5,5), dpi=80)

# Plot NN predictions as scattered colored points
plt.scatter(grid[:,0], grid[:,1], c=Z, cmap='bwr', alpha=0.3, s=15)

# Plot original points
plt.scatter(X[:,0], X[:,1], c=y, cmap='bwr', edgecolor='k', alpha=0.6)

# Plot the real circle boundary
theta = np.linspace(0, 2*np.pi, 100)
plt.plot(center[0] + radius*np.cos(theta), center[1] + radius*np.sin(theta),
         color='green', linewidth=2, label='Real boundary')

plt.xlabel('x')
plt.ylabel('y')
plt.title('NN classification vs real function (lightweight)')
plt.legend()
plt.show()