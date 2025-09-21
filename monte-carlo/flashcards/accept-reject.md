# Accept-Reject Sampling


## ar-hypothesis
```yaml
id: ar-hypothesis
deck: monte-carlo
tags: [monte-carlo, random variables, sampling]
note_type: basic
```
Assume that \( f \) is a pdf and \( g \) is a pdf such that \( f(x) \leq M g(x) \) for \(M > 0\).
We note that \( g \) is easy to sample from and \( M \) is known. Describe the accept-reject algorithm to sample from \( f \).

- Sample \( Y \sim g \) and \( U \sim \mathscr{U}(0,1) \).
- If \( U \leq \frac{f(Y)}{M g(Y)} \), accept \( Y \) as a sample from \( f \).
- Otherwise, reject \( Y \) and repeat the process until a sample is accepted.
- The accepted samples will be distributed according to the pdf \( f \).


## ar-acceptance-rate
```yaml
id: ar-acceptance-rate
deck: monte-carlo
tags: [monte-carlo, random variables, sampling, property]
note_type: basic
```
In the accept-reject algorithm, if we denote 
\[ p = \mathbb{P}(U \leq \frac{f(Y)}{M g(Y)}) \quad N: \text{ Number of trials before acceptance} \]
what is the distribution of \( N \) and what is its expectation?

The random variable \( N \) follows a geometric distribution with parameter \( p \). The probability mass function is given by
\[
\mathbb{P}(N = k) = (1-p)^{k-1} p, \quad k = 1, 2, \ldots
\]
The expectation of \( N \) is given by
\[
\mathbb{E}[N] = \frac{1}{p}
\]


## ar-calculate-p
```yaml
id: ar-calculate-p
deck: monte-carlo
tags: [monte-carlo, random variables, sampling, property]
note_type: basic
```
In the accept-reject algorithm, if we denote 
\[ p = \mathbb{P}(U \leq \frac{f(Y)}{M g(Y)}) \quad Y \sim g \]
show that \( p = \frac{1}{M} \).

\[
    \begin{aligned}
    p &= \mathbb{P}\left(U \le \frac{f(Y)}{M g(Y)}\right) \\
    &= \mathbb{E}_Y\!\left[\mathbb{P}\left(U \le \frac{f(Y)}{M g(Y)} \mid Y\right)\right] \\
    &= \mathbb{E}_Y\!\left[\frac{f(Y)}{M g(Y)}\right] \\
    &= \int \frac{f(y)}{M g(y)} g(y)\, dy \\
    &= \frac{1}{M} \int f(y)\, dy \\
    &= \frac{1}{M}.
    \end{aligned} 
\]
Recall that \( N \sim \text{Geom}(p) \), where \( p = \frac{1}{M} \). Thus \( \mathbb{E}[N] = M \). We thus want to find a \( g \) such that 
\[ \sup_{x \in \mathbb{R}} \frac{f(x)}{g(x)} \]
is minimized.


## ar-why-init
```yaml
id: ar-why
deck: monte-carlo
tags: [monte-carlo, random variables, sampling, proof]
note_type: basic
```
Given the hypotheses of the accept-reject algorithm, what's the core formula to prove that the accepted samples are distributed according to \( f \)?

\[ \mathbb{P}(Y \leq y \mid U \leq \frac{f(Y)}{M g(Y)}) = \int_{-\infty}^{y} f(t) \, dt = F(y) \]


## ar-why-proof
```yaml
id: ar-why-proof
deck: monte-carlo
tags: [monte-carlo, random variables, sampling, proof]
note_type: basic
```
Let \( F(y) = \int_{-\infty}^{y} f(t) \, dt \), let \( G(y) = \int_{-\infty}^{y} g(t) \, dt = \mathbb{P}(Y \leq y) = \mathbb{P}(A) \), and let \( \mathbb{P}(U \leq \frac{f(Y)}{M g(Y)}) = \mathbb{P}(B) = p \). Show that
\[ \mathbb{P}(Y \leq y \mid U \leq \frac{f(Y)}{M g(Y)}) = \int_{-\infty}^{y} f(t) \, dt = F(y) \]

By conditional probability, we have
\[ \mathbb{P}(A | B) = \frac{\mathbb{P}(B \mid A) \mathbb{P}(A)}{\mathbb{P}(B)} = \frac{\mathbb{P}(B \mid A) \cdot G(y)}{\frac{1}{M}} \]
We have already calculated \( \mathbb{P}(B) = p = \frac{1}{M} \). Now, we calculate \( \mathbb{P}(B \mid A) \):
\[ \mathbb{P}(B \mid A) = \mathbb{P} ( U \leq \frac{f(Y)}{M g(Y)} \mid Y \leq y ) = \frac{\mathbb{P}(U \leq \frac{f(Y)}{M g(Y)} \cap Y \leq y )}{G(y)} \]
\[= \int_{-\infty}^{y} \frac{\mathbb{P}\left(U \leq \frac{f(t)}{M g(t)} \mid Y = t\right)}{G(y)} g(t) \, dt = \frac{1}{G(y)} \int_{-\infty}^{y} \frac{f(t)}{M g(t)} g(t) \, dt = \frac{1}{M} \cdot \frac{F(y)}{G(y)} \]
Putting it all together, we have
\[ \mathbb{P}(Y \leq y \mid U \leq \frac{f(Y)}{M g(Y)}) = \frac{\frac{1}{M} \cdot \frac{F(y)}{G(y)} \cdot G(y)}{\frac{1}{M}} = F(y) \]

