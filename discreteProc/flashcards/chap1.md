# Discrete Processes — Flashcards Deck

## ce-def-theorem
```yaml
id: ce-def-theorem
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Let \(X \in L^1\), we denote \( \mathbb{E}[X \mid \mathcal{G}] \) as the conditional expectation of \(X\) given a sigma-algebra \( \mathcal{G} \). What are the defining properties of \( \mathbb{E}[X \mid \mathcal{G}] \)?

1. \( \mathbb{E}[X \mid \mathcal{G}] \) is \( \mathcal{G} \)-measurable.
2. For all \( A \in \mathcal{G} \) 
\[ \mathbb{E}[X \mathbf{1}_A] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mathbf{1}_A] \]
Which can be rewritten as:
\[ \int_A X \, d\mathbb{P} = \int_A \mathbb{E}[X \mid \mathcal{G}] \, d\mathbb{P} \]
This is very useful in practice be cause it allows us to compute conditional expectations by integrating over events in \( \mathcal{G} \).
3. (equivalent to 2) For all \( Z \in L^1(\Omega, \mathcal{G}, \mathbb{P}) \)
\[ \mathbb{E}[X Z] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] Z] \]
Which can be rewritten as:
\[ \int_\Omega X Z \, d\mathbb{P} = \int_\Omega \mathbb{E}[X \mid \mathcal{G}] Z \, d\mathbb{P} \]


## ce-extended-def
```yaml
id: ce-def-extended
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Give a more general property of \( \mathbb{E}[X \mid \mathcal{G}] \) for \( X \in L^1 \), \( A \in \mathcal{G} \) that is equivalent to:
\[ \mathbb{E}[X \mathbf{1}_A] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mathbf{1}_A] \]

For all \(Z\) measurable with respect to \( \mathcal{G} \) such that \( XZ \in L^1 \):
\[ \mathbb{E}[X Z] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] Z] \]


## ce-upper_bound
```yaml
id: ce-upper_bound
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Let \(X \in L^1\) and \( \mathcal{G} \subseteq \mathcal{F} \) be a sigma-algebra. Show that:
\[ \mathbb{E}[X \mid \mathcal{G}] \in L^1 \]

By Jensen's inequality, we have:
\[ |\mathbb{E}[X \mid \mathcal{G}]| \leq \mathbb{E}[|X| \mid \mathcal{G}] \]
Integrating both sides over \( \Omega \):
\[ \int_\Omega |\mathbb{E}[X \mid \mathcal{G}]| \, d\mathbb{P} \leq \int_\Omega \mathbb{E}[|X| \mid \mathcal{G}] \, d\mathbb{P} = \mathbb{E}[|X|] < \infty \]
Thus, \( \mathbb{E}[X \mid \mathcal{G}] \in L^1 \).


## ce-event
```yaml
id: ce-event
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```

Let \(X \in L^1\), \( \mathcal{G} \subseteq \mathcal{F} \) be a sigma-algebra, and \(A \in \mathcal{G}\). What is \( \mathbb{E}[X \mid A] \)?

The conditional expectation of \(X\) given the event \(A\) is defined as:
\[ \mathbb{E}[X \mid A] = \frac{\mathbb{E}[X \mathbf{1}_A]}{\mathbb{P}(A)} \]


## ce-Y-discrete
```yaml
id: ce-Y-discrete
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Let \(X \in L^1\) and \(Y\) be a discrete random variable taking values in a countable set \( \{y_i\}_{i \in I} = E \). How can we express \( \mathbb{E}[X \mid Y] \)?

The conditional expectation \( \mathbb{E}[X \mid Y] \) can be expressed as:
\[ \mathbb{E}[X \mid Y] = \sum_{y \in E} \mathbb{E}[X \mid Y = y] \mathbf{1}_{\{Y = y\}} \]
\[ \mathbb{E}[X \mid Y] = \sum_{y \in E} \frac{\mathbb{E}[X \mathbf{1}_{\{Y = y\}}]}{\mathbb{P}(Y = y)} \mathbf{1}_{\{Y = y\}} \]


## ce-XY-discrete
```yaml
id: ce-XY-discrete
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Let \(X \, Y \) be two discrete random variables taking values in countable sets \( \{x_i\}_{i \in I} = E_X \) and \( \{y_j\}_{j \in J} = E_Y \). How can we express \( \mathbb{E}[X \mid Y] \)?

The conditional expectation \( \mathbb{E}[X \mid Y] \) can be expressed as:
\[ \mathbb{E}[X \mid Y] = \sum_{y \in E_Y} \mathbb{E}[X \mid Y = y] \mathbf{1}_{\{Y = y\}} \]
Where:
\[ \mathbb{E}[X \mid Y = y] = \sum_{x \in E_X} x \, \mathbb{P}(X = x \mid Y = y) \]


## ce-XY-joint-density
```yaml
id: ce-XY-joint-density
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Let \(X \, Y \) be two discrete random variables with joint density \( p_{X,Y} \). How can we express \( \mathbb{E}[X \mid Y] \)?

The conditional expectation \( \mathbb{E}[X \mid Y] \) can be expressed as:
\[ \mathbb{E}[X \mid Y = y] = \int_{\mathbb{R}} x \, p_{X \mid Y = y}(x) \, dx \]
Where:
\[ p_{X \mid Y = y}(x) = \frac{p_{X,Y}(x,y)}{p_Y(y)} \]


## L2-proj
```yaml
id: L2-proj
deck: discrete_processes
tags: [bilinear, inner-product, projection]
note_type: basic
```
Let \(H\) be a Hilbert space with inner product \( \langle \cdot, \cdot \rangle \) and \(W\) be a closed subspace of \(H\). Let \(x \in H\) and \(w_0 \in W\) be the orthogonal projection of \(x\) onto \(W\). Prove that \(w_0\) is the unique minimizer of the distance \( \|x - w\| \) for \( w \in W \).

\[ x - w = x - w_0 + w_0 - w \]
We have:
\[ \|x - w\|^2 = \|x - w_0\|^2 + \|w_0 - w\|^2 \]
This is because \( x - w_0 \) is orthogonal to \( W \), so \( \langle x - w_0, w_0 - w \rangle = 0 \).
Thus:
\[ \|x - w\|^2 \geq \|x - w_0\|^2 \]
with equality if and only if \( w = w_0 \).


## ce-L2-proj
```yaml
id: ce-L2-proj
deck: discrete_processes
tags: [probability, conditional-expectation]
note_type: basic
```
Let \(X \in L^2\) and \( \mathcal{G} \subseteq \mathcal{F} \) be a sigma-algebra. How can we interpret \( \mathbb{E}[X \mid \mathcal{G}] \) in terms of projection?

The conditional expectation \( \mathbb{E}[X \mid \mathcal{G}] \) can be interpreted as the orthogonal projection of \(X\) onto the closed subspace \( L^2(\Omega, \mathcal{G}, \mathbb{P}) \) of \( L^2(\Omega, \mathcal{F}, \mathbb{P}) \). This means that \( \mathbb{E}[X \mid \mathcal{G}] \) is the element in \( L^2(\Omega, \mathcal{G}, \mathbb{P}) \) that minimizes the mean squared error:
\[ \mathbb{E}[(X - Z)^2] \]
for all \( Z \in L^2(\Omega, \mathcal{G}, \mathbb{P}) \).
We can verify this by checking the properties for orthogonal projection:
1. \( \mathbb{E}[X \mid \mathcal{G}] \) is \( \mathcal{G} \)-measurable.
2. For all \( Z \in L^2(\Omega, \mathcal{G}, \mathbb{P}) \):
\[ \mathbb{E}[(X - \mathbb{E}[X \mid \mathcal{G}]) Z] = 0 \]
This is equivalent to the property:
\[ \mathbb{E}[X Z] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] Z] \]
This suggests that the error \( X - \mathbb{E}[X \mid \mathcal{G}] \) is orthogonal to all elements in \( L^2(\Omega, \mathcal{G}, \mathbb{P}) \), and hence \( \mathbb{E}[X \mid \mathcal{G}] \) is the best approximation of \(X\) in that subspace.


## ce-linearity
```yaml
id: ce-linearity
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```

Let \(X, Y \in L^1\) and \(a, b \in \mathbb{R}\). Show that the conditional expectation operator is linear, i.e.:
\[ \mathbb{E}[aX + bY \mid \mathcal{G}] = a \mathbb{E}[X \mid \mathcal{G}] + b \mathbb{E}[Y \mid \mathcal{G}] \]

We need to verify the two defining properties of conditional expectation for \( aX + bY \):
1. \( a \mathbb{E}[X \mid \mathcal{G}] + b \mathbb{E}[Y \mid \mathcal{G}] \) is \( \mathcal{G} \)-measurable since both \( \mathbb{E}[X \mid \mathcal{G}] \) and \( \mathbb{E}[Y \mid \mathcal{G}] \) are \( \mathcal{G} \)-measurable.
2. For all \( A \in \mathcal{G} \):
\[ \mathbb{E}[(aX + bY) \mathbf{1}_A] = a \mathbb{E}[X \mathbf{1}_A] + b \mathbb{E}[Y \mathbf{1}_A] \]
Using the properties of expectation and linearity, we have:
\[ \mathbb{E}[(aX + bY) \mathbf{1}_A] = a \mathbb{E}[X \mathbf{1}_A] + b \mathbb{E}[Y \mathbf{1}_A] = a \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mathbf{1}_A] + b \mathbb{E}[\mathbb{E}[Y \mid \mathcal{G}] \mathbf{1}_A] \]
\[ = \mathbb{E}[(a \mathbb{E}[X \mid \mathcal{G}] + b \mathbb{E}[Y \mid \mathcal{G}]) \mathbf{1}_A] \]
Thus, \( \mathbb{E}[aX + bY \mid \mathcal{G}] = a \mathbb{E}[X \mid \mathcal{G}] + b \mathbb{E}[Y \mid \mathcal{G}] \).


## ce-positivity
```yaml
id: ce-positivity
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) such that \(X \geq 0\) almost surely. Show that \( \mathbb{E}[X \mid \mathcal{G}] \geq 0 \) almost surely.

If \(X \geq 0\) almost surely, then for any \( A \in \mathcal{G} \):
\[ \mathbb{E}[X \mathbf{1}_A] \geq 0 \]
Using the defining property of conditional expectation:
\[ \mathbb{E}[X \mathbf{1}_A] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mathbf{1}_A] \]
Since this holds for all \( A \in \mathcal{G} \), it follows that \( \mathbb{E}[X \mid \mathcal{G}] \geq 0 \) almost surely.


## ce-jensen
```yaml
id: ce-jensen
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) and \( \varphi: \mathbb{R} \to \mathbb{R} \) be a convex function such that \( \varphi(X) \in L^1 \). Show that:
\[ \varphi(\mathbb{E}[X \mid \mathcal{G}]) \leq \mathbb{E}[\varphi(X) \mid \mathcal{G}] \]

By the definition of convexity, for any \( x_0 \in \mathbb{R} \), there exists a subgradient \( m \) such that:
\[ \varphi(x) \geq \varphi(x_0) + m (x - x_0) \quad \forall x \in \mathbb{R} \]
Setting \( x_0 = \mathbb{E}[X \mid \mathcal{G}] \) we obtain:
\[ \varphi(X) \geq \varphi(\mathbb{E}[X \mid \mathcal{G}]) + m (X - \mathbb{E}[X \mid \mathcal{G}]) \]
Taking conditional expectation on both sides:
\[ \mathbb{E}[\varphi(X) \mid \mathcal{G}] \geq \varphi(\mathbb{E}[X \mid \mathcal{G}]) + m (\mathbb{E}[X \mid \mathcal{G}] - \mathbb{E}[X \mid \mathcal{G}]) \]
This simplifies to:
\[ \mathbb{E}[\varphi(X) \mid \mathcal{G}] \geq \varphi(\mathbb{E}[X \mid \mathcal{G}]) \]


## ce-tower
```yaml
id: ce-tower
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) and \( \mathcal{H} \subseteq \mathcal{G} \subseteq \mathcal{F} \) be sigma-algebras. Show that:
\[ \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mid \mathcal{H}] = \mathbb{E}[X \mid \mathcal{H}] \]

As \( X \in L^1 \), \( \mathbb{E}[X \mid \mathcal{G}] \in L^1 \), and \( \mathbb{E}[X \mid \mathcal{H}] \in L^1 \). We need to verify the defining property of conditional expectation for \( \mathbb{E}[X \mid \mathcal{G}] \) with respect to \( \mathcal{H} \):
For all \( A \in \mathcal{H} \):
\[ \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mathbf{1}_A] = \mathbb{E}[X \mathbf{1}_A] \]
This follows from the fact that \( A \in \mathcal{H} \subseteq \mathcal{G} \).
By the uniqueness of conditional expectation, we conclude that: 
\[ \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mid \mathcal{H}] = \mathbb{E}[X \mid \mathcal{H}] \]


## ce-trivial
```yaml
id: ce-trivial
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) and \( \mathcal{G} = \{\emptyset, \Omega\} \) be the trivial sigma-algebra. Then what is \( \mathbb{E}[X \mid \mathcal{G}] \)?

The conditional expectation \( \mathbb{E}[X \mid \mathcal{G}] \) is equal to the constant random variable \( \mathbb{E}[X] \)
This is equivalent to saying that:
1. The trivial sigma-algebra is independent of any other sub sigma-algebra.
2. \( \mathbb{E}[X] \) is the best constant approximation of \(X\). 


## ce-independence
```yaml
id: ce-independence
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) and \( \mathcal{G} \subseteq \mathcal{F} \) be a sigma-algebra such that \(X\) is independent of \( \mathcal{G} \). What is \( \mathbb{E}[X \mid \mathcal{G}] \)?

The conditional expectation \( \mathbb{E}[X \mid \mathcal{G}] \) is equal to the constant random variable \( \mathbb{E}[X] \)
This is because for any \( A \in \mathcal{G} \):
\[ \mathbb{E}[X \mathbf{1}_A] = \mathbb{E}[X] \mathbb{P}(A) = \mathbb{E}[\mathbb{E}[X] \mathbf{1}_A] \]
Thus by the uniqueness of conditional expectation, we conclude that \( \mathbb{E}[X \mid \mathcal{G}] = \mathbb{E}[X] \) almost surely.


## ce-Xmeasurable
```yaml
id: ce-Xmeasurable
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) be \( \mathcal{G} \)-measurable. What is \( \mathbb{E}[X \mid \mathcal{G}] \)?

The conditional expectation \( \mathbb{E}[X \mid \mathcal{G}] \) is equal to \(X\) almost surely.
Since the conditional expectation is a random variable that is \( \mathcal{G} \)-measurable and satisfies (for all \( A \in \mathcal{G} \)):
\[ \mathbb{E}[X \mathbf{1}_A] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] \mathbf{1}_A] \]
We immediately see that \(X\) itself satisfies these properties, and by the uniqueness of conditional expectation, we conclude that \( \mathbb{E}[X \mid \mathcal{G}] = X \) almost surely.


## ce-take-outside
```yaml
id: ce-take-outside
deck: discrete_processes
tags: [probability, conditional-expectation, properties]
note_type: basic
```
Let \(X \in L^1\) and \(Z \in L^1(\Omega, \mathcal{G}, \mathbb{P})\). Show that:
\[ \mathbb{E}[X Z \mid \mathcal{G}] = Z \mathbb{E}[X \mid \mathcal{G}] \]

We need to verify the defining properties of conditional expectation for \( XZ \):
Let \( A \in \mathcal{G} \):
\[ \int_A \mathbb{E}[XZ \mid \mathcal{G}] \, d\mathbb{P} =  \int_A XZ \, d\mathbb{P} = \int_A \mathbb{E}[X \mid \mathcal{G}] Z \, d\mathbb{P} \]
Second equality holds because \(Z\) is \( \mathcal{G} \)-measurable (An extended definition of conditional expectation). Thus, by the uniqueness of conditional expectation, we conclude that:
\[ \mathbb{E}[X Z \mid \mathcal{G}] = Z \mathbb{E}[X \mid \mathcal{G}] \]

