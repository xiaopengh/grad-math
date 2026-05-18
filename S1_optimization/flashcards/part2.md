# Unconstrained optimization: approximation and gradient descent

## cv-linear
```yaml
id: cv-linear
deck: optimization
tags: [optimization, convergence]
note_type: basic
```
Define for \( \{ x_k \}_{k \in \mathbb{N}} \) when it is said to converge linearly to \( x^* \) at rate \( \alpha \in (0,1) \).

\( \{ x_k \}_{k \in \mathbb{N}} \) converges linearly to \( x^* \) at rate \( \alpha \in (0,1) \) if there exists a constant \( C > 0 \) such that
\[ \| x_{k+1} - x^* \| \leq C \alpha^k \quad \text{for all } k \in \mathbb{N}. \]


## gradient-descent-init
```yaml
id: gradient-descent-init
deck: optimization
tags: [optimization, gradient-descent, taylors expansion]
note_type: basic
```
Let's say that we are start from an initial guess \( x_0 \) to minimize \( f \), and we consider \( x \) in the neighborhood of \( x_0 \), such that the distance doesn't exceed \( \| x - x_0 \| < d_0 \). Using Taylor's expansion, give the first order approximation of \( f(x) \) around \( x_0 \), and explain how to choose \( x \) to get a better guess.

Using Taylor's expansion, the first order approximation of \( f(x) \) around \( x_0 \) is given by
    \[ f(x) \approx f(x_0) + \nabla f(x_0)^T (x - x_0). \]
To get a better guess, we consider the minimization problem
    \[ \min_{\| x - x_0 \| < d_0} f(x) \approx \min_{\| x - x_0 \| < d_0} f(x_0) + \nabla f(x_0)^T (x - x_0). \]
In the case where the gradient **does not vanish** at \( x_0 \), this is equivalent to solving:
    \[\min_{\| x - x_0 \| < d_0} \langle \nabla f(x_0), x - x_0 \rangle.\]
This problem is solved by (considering the geometric interpretation of the inner product): 
    \[ x - x_0 = - d_0 \frac{\nabla f(x_0)}{\|\nabla f(x_0)\|} \]


## C1.1-def
```yaml
id: C1.1-def
deck: optimization
tags: [optimization, differentiability, lipschitz]
note_type: basic
```
Define what it means for a function \( f: \mathbb{R}^n \to \mathbb{R} \) to be of class \( \mathscr{C}^{1,1} \) on a set \( S \subseteq \mathbb{R}^n \).

1. \( f \) is in \( \mathscr{C}^1 \) on \( S \), i.e., \( f \) is differentiable on \( S \) and its gradient \( \nabla f \) is continuous on \( S \).
2. The gradient \( \nabla f \) is Lipschitz continuous on \( S \), i.e., there exists a constant \( L > 0 \) such that for all \( x, y \in S \),
   \[ \| \nabla f(x) - \nabla f(y) \| \leq L \| x - y \|. \]
P.S. The smallest such \( L \) is called the Lipschitz constant of \( \nabla f \) on \( S \). We also say that \( f \) has an \( L \)-Lipschitz gradient on \( S \).


## cv-descent-gradient-lipschitz
```yaml
id: cv-descent-gradient-lipschitz
deck: optimization
tags: [optimization, gradient-descent, convergence, taylors expansion, lipschitz]
note_type: basic
```
Let \( f: \mathbb{R}^n \to \mathbb{R} \) be a function of class \( \mathscr{C}^{1,1} \) with Lipschitz constant \( L > 0 \) on \( \mathbb{R}^n \). What can we say about the gradient descent initialized at \( x_0 \) with constant step size \( \tau \)?

For any \( x_0 \in \mathbb{R}^n \), for any step size \( \tau > 0 \) 
\[ \forall k \in \mathbb{N}, \quad f(x_{k+1}) - f(x_k) \leq \tau (\tau L - 1) \|\nabla f(x_k)\|^2 \]
**Proof:**  
Recall the gradient descent update rule:
\[ x_{k+1} = x_k - \tau \nabla f(x_k). \]
Then we apply mean value theorem by assume that \( \xi \in \mathbb{B}(x_k, \| x_{k+1} - x_k \|) \), we have:
\[ f(x_{k+1}) - f(x_k) = \langle \nabla f(\xi), - \tau \nabla f(x_k) \rangle. \]
\[ f(x_{k+1}) - f(x_k) \leq \langle \nabla f(\xi) - \nabla f(x_k), - \tau \nabla f(x_k) \rangle + \langle \nabla f(x_k), - \tau \nabla f(x_k) \rangle. \]
\[ f(x_{k+1}) - f(x_k) \leq \tau \| \nabla f(\xi) - \nabla f(x_k) \| \| \nabla f(x_k) \| - \tau \| \nabla f(x_k) \|^2. \]
\[ \boxed{ \| \nabla f(\xi) - \nabla f(x_k) \| \leq L \| \xi - x_k \| \leq L \| x_{k+1} - x_k \| = L \| - \tau \nabla f(x_k) \| } \]
\[ f(x_{k+1}) - f(x_k) \leq \tau^2 L \| \nabla f(x_k) \|^2 - \tau \| \nabla f(x_k) \|^2 = \tau (\tau L - 1) \|\nabla f(x_k)\|^2. \]


## cv-descent-gradient-lipschitz-convex
```yaml
id: cv-descent-gradient-lipschitz-convex
deck: optimization
tags: [optimization, gradient-descent, convergence, taylors expansion, lipschitz, convexity]
note_type: basic
```
\( f \) in \( \mathscr{C}^{1,1} \) with Lipschitz constant \( L > 0 \) on \( \mathbb{R}^n \) and convex (minimiser \( x^* \)). What can we say about the gradient descent initialized at \( x_0 \) with constant step size \( \tau \in (0, \frac{1}{2L}) \) ?
What is the convergence rate \( r \) such that \( f(x_k) - f(x^*) \leq r \| x_k - x^* \| \) for some constant \( r > 0 \) ?

Recall that for a function with \( L \)-Lipschitz gradient, we have:
\[ \forall k \in \mathbb{N}, \quad f(x_{k+1}) \leq f(x_k) -\frac{\tau}{2} \| \nabla f(x_k) \|^2. \]
By the convexity of \( f \), we have:
\[ f(x_k) \leq f(x^*) + \langle \nabla f(x_k), x_k - x^* \rangle \]
Combining the two inequalities, we get:
\[ f(x_{k+1}) \leq f(x^*) + \langle \nabla f(x_k), x_k - x^* \rangle - \frac{\tau}{2} \| \nabla f(x_k) \|^2. \]
Extracting \( \frac{1}{2\tau} \), we have:
\[ f(x_{k+1}) - f(x^*) \leq \frac{1}{2\tau} \left( 2\tau \langle \nabla f(x_k), x_k - x^* \rangle - \tau^2 \| \nabla f(x_k) \|^2 \right). \]
Completing the square, we get:
\[ f(x_{k+1}) - f(x^*) \leq \frac{1}{2\tau} \left( \| x_k - x^* \|^2 - \| x_k - x^* - \tau \nabla f(x_k) \|^2 \right). \]
Using the update rule of gradient descent, we have:
\[ f(x_{k+1}) - f(x^*) \leq \frac{1}{2\tau} \left( \| x_k - x^* \|^2 - \| x_{k+1} - x^* \|^2 \right). \]
By the negativity \( f(x_{k}) - f(x_{k-1}) \) 
\[ k (f(x_{k}) - f(x^*)) \leq \sum_{i=1}^{k} f(x_{i}) - f(x^*) \leq \sum_{i=1}^{k} \frac{1}{2\tau} \left( \| x_{i-1} - x^* \|^2 - \| x_{i} - x^* \|^2 \right). \]
\[ k (f(x_{k}) - f(x^*)) \leq \frac{1}{2\tau} \left( \| x_0 - x^* \|^2 - \| x_{k} - x^* \|^2 \right) \leq \frac{1}{2\tau} \| x_0 - x^* \|^2. \]
Thus, we have:
\[ \boxed{ f(x_k) - f(x^*) \leq \frac{\| x_0 - x^* \|^2}{2 \tau k} } \]
This shows that the convergence rate is \( O\left(\frac{1}{k}\right) \), which is sublinear.


