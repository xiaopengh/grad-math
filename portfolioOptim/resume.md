# Portfolio Management - Summary of Key Content

## 1. Returns and Rate of Returns

**Types of Returns:**
- **Return**: $R_T = \frac{W_T}{W_0} - 1$
- **Log Return**: $R_T^c = \ln(\frac{W_T}{W_0})$
- **Total Return**: $R_T = \frac{P_T + Div(0,T)}{P_0} - 1$

**Rate Definitions:**
- Monetary rate: $1 + r_m \times T = 1 + R_T$
- Actuarial rate: $(1 + r_a)^T = 1 + R_T$
- Continuous rate: $\exp(r_c \times T) = 1 + R_T$

## 2. Statistical Definitions

**For random variable X:**
- Mean: $E[X]$
- Variance: $Var[X] = E[X^2] - E[X]^2$
- Skewness: $Skew[X] = E[(\frac{X-E(X)}{\sigma(X)})^3]$
- Kurtosis: $Kur[X] = E[(\frac{X-E(X)}{\sigma(X)})^4]$

**Properties:**
- For $X \sim N(m, \sigma^2)$: $Skew(X) = 0$ and $Kur(X) = 3$
- Leptokurtic: $Kur(X) > 3$ (fat tails)
- Platykurtic: $Kur(X) < 3$

## 3. Statistical Tests

**Berra and Jarque Test:**
$$\hat{BJ}(X) = \frac{n}{6}[\hat{Skew}(X)]^2 + \frac{n}{24}[\hat{Kur}(X) - 3]^2 \approx \chi^2(2)$$

Reject normality at level α if $\hat{BJ}(x) > \chi^2_{1-\alpha}(2)$

## 4. Utility Functions

**Definition:** $u: \mathbb{R} \to \mathbb{R}$ continuous, strictly increasing, twice differentiable

**Key Concepts:**
- **Certain Equivalent**: $u[C_u(X)] = E[u(X)]$
- **Risk Premium**: $\Pi_u(X) = E[X] - C_u(X)$
- **Risk Aversion Measure**: $-\frac{u''(a)}{u'(a)}$

**Risk Attitudes:**
- Risk averse: $u$ concave $\Leftrightarrow \Pi_u(X) \geq 0$
- Risk taker: $u$ convex $\Leftrightarrow \Pi_u(X) \leq 0$
- Risk neutral: $u$ affine $\Leftrightarrow \Pi_u(X) = 0$

## 5. Markowitz Framework - Without Risk-Free Asset

**Portfolio Notation:**
- $\pi = (\pi_1, ..., \pi_n)^T$ where $\pi_i$ = weight in asset $i$
- $\pi' 1_n = 1$ for investment portfolios
- $\pi' 1_n = 0$ for self-financing portfolios
- $R^\pi_T = \pi' R_T$
- $m_\pi = E(R^\pi_T) = \pi' M$
- $\sigma^2_\pi = Var(R^\pi_T) = \pi' \Sigma \pi$

**Optimization Problem:**
$$(P) \begin{cases} \min_\pi \pi' \Sigma \pi \\ \pi' M = m \\ \pi' 1_n = 1 \end{cases}$$

**Key Portfolios:**
- $\pi_a = \frac{1}{a}\Sigma^{-1}1_n$ (minimum variance portfolio)
- $\omega_{a,b} = \frac{\Sigma^{-1}(M - \frac{b}{a}1_n)}{\|M - \frac{b}{a}1_n\|_{\Sigma^{-1}}}$ (self-financing, unit variance)

Where: $a = \langle 1_n, 1_n \rangle_{\Sigma^{-1}} = 1_n' \Sigma^{-1} 1_n$ and $b = \langle M, 1_n \rangle_{\Sigma^{-1}} = M' \Sigma^{-1} 1_n$

**Efficient Frontier:**
$$\mathcal{F} = \{\pi_a + \lambda \omega_{a,b}, \lambda \in \mathbb{R}\}$$
$$\mathcal{F}^+ = \{\pi_a + \lambda \omega_{a,b}, \lambda \geq 0\}$$ (efficient)
$$\mathcal{F}^- = \{\pi_a + \lambda \omega_{a,b}, \lambda \leq 0\}$$ (inefficient)

**Properties:**
- $m_{\pi_a} = \frac{b}{a}$, $\sigma_{\pi_a} = \frac{1}{\sqrt{a}}$
- $m_{\omega_{a,b}} = \|M - \frac{b}{a}1_n\|_{\Sigma^{-1}}$, $\sigma_{\omega_{a,b}} = 1$
- $cov(R^{\pi_a}, R^{\omega_{a,b}}) = 0$
- $\mathcal{F}(\sigma, m)$ forms a hyperbola

**Two Funds Theorem:** All efficient portfolios can be built from any two distinct efficient portfolios.

## 6. Markowitz Framework - With Risk-Free Asset

**Notation:** $\Pi = (\pi_0, \pi)^T$ where $\pi_0$ = weight in risk-free asset, return $r_0$

**Optimization Problem:**
$$(Q) \begin{cases} \min_\pi \pi' \Sigma \pi \\ \pi' M + (1 - \pi' 1_n)r_0 = m \end{cases}$$

**Tangent Portfolio:**
$$\pi_T = \frac{1}{b - r_0 a}\Sigma^{-1}(M - r_0 1_n)$$

**Capital Market Line (CML):**
$$\mathcal{C} = \{\lambda \Pi_T + (1-\lambda)\Pi_0, \lambda \in \mathbb{R}\}$$

Where $\Pi_T = (0, \pi_T)^T$ and $\Pi_0 = (1, 0)^T$

**Properties:**
- For $\Pi \in \mathcal{C}$: $m_\Pi = \lambda m_{\Pi_T} + (1-\lambda)r_0$ and $\sigma_\Pi = |\lambda|\sigma_{\Pi_T}$
- $\mathcal{C}(\sigma, m)$ is a cone tangent to $\mathcal{F}(\sigma, m)$ at $(\sigma_{\Pi_T}, m_{\Pi_T})$
- All efficient portfolios combine only $\Pi_T$ and $\Pi_0$

## 7. Security Market Line (SML)

**Theorem:**
For any investment portfolio $\Pi_P$:
$$m_P - r_0 = (m_T - r_0)\rho(R_P, R_T)\frac{\sigma_P}{\sigma_T}$$

**Beta Definition:**
$$\beta_T(P) = \rho(R_T, R_P)\frac{\sigma_P}{\sigma_T} = \frac{cov(R_T, R_P)}{Var(R_T)}$$

**SML Equation:**
$$m_P - r_0 = (m_T - r_0)\beta_T(P)$$

**Decomposition:**
$$R_P - r_0 = (R_T - r_0)\beta_T(P) + \epsilon$$

where $\epsilon$ is independent from $R_T$ with $E[\epsilon] = 0$

**Risk Decomposition:**
$$\sigma^2_P = \sigma^2_T \beta_T(P)^2 + \sigma^2_\epsilon$$
- Systematic risk: $\sigma_T |\beta_T(P)|$ (remunerated)
- Idiosyncratic/specific risk: $\sigma_\epsilon$ (not remunerated)

## 8. Performance Indicators

**Sharpe Ratio:**
$$SR_P = \frac{m_P - r_0}{\sigma_P}$$
- Independent of leverage
- Maximal for portfolios on CML
- Measures return per unit of total risk

**Jensen Index:**
$$JI_P = m_P - [r_0 + \beta_{M,P}(m_M - r_0)]$$
- Measures excess return above SML
- Dependent on leverage
- Should be zero under CAPM

**Treynor Index:**
$$TI_P = \frac{m_P - r_0}{\beta_M(P)}$$
- Independent of leverage
- Measures return per unit of systematic risk
- Should be constant under CAPM