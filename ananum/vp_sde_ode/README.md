# VP Diffusion SDE vs Backward Probability-Flow ODE (1D)

Source: project 2 from `evaluation_TP_M1_P26_v1s.pdf`, rewritten in Markdown so it is easier to verify. Formula numbers are kept from the PDF.

## Project Title

**2 VP Diffusion SDE vs Backward Probability-Flow ODE (1D), code `vp_sde_ode`**

We consider a *variance-preserving* (VP) diffusion in 1D and its corresponding *probability-flow ODE*. The purpose is to simulate:

1. the **forward SDE** with Euler-Maruyama,
2. the **backward ODE** with RK4 / Heun (vectorized) or `odeint` (non-vectorized, penalty),

and compare, at `n_T` intermediary times, the empirical marginal distributions by overlaid histograms.

## Target (initial) distribution

Let the initial law `p_0` be the normalized sum of two indicator functions:

```math
p_0(x)
=
\frac{1}{2}\mathbf{1}_{[-2,-1]}(x)
+
\frac{1}{2}\mathbf{1}_{[1,2]}(x).
\tag{4}
```

## Forward VP diffusion (SDE)

Fix a constant schedule (step) `\beta > 0` and consider the VP SDE:

```math
dX_t
=
-\frac{\beta}{2}X_t\,dt
+
\sqrt{\beta}\,dW_t,
\qquad
X_0 \sim p_0.
\tag{5}
```

This SDE has the explicit conditional law

```math
\operatorname{law}(X_t \mid X_0=x_0)
\sim
\mathcal{N}\!\left(a(t)x_0, s^2(t)\right),
\qquad
a(t):=e^{-\beta t/2},
\qquad
s^2(t):=1-e^{-\beta t}.
\tag{6}
```

## Analytic expressions

Analytic expressions for `p_t(x)` and `\partial_x \log p_t(x)`: using (6) and (4), for `t > 0` the marginal density `p_t` is:

```math
p_t(x)
=
\frac{1}{2a(t)}
\left[
\Phi\!\left(\frac{x+2a(t)}{s(t)}\right)
-
\Phi\!\left(\frac{x+a(t)}{s(t)}\right)
+
\Phi\!\left(\frac{x-a(t)}{s(t)}\right)
-
\Phi\!\left(\frac{x-2a(t)}{s(t)}\right)
\right],
\tag{7}
```

```math
p'_t(x)
=
\frac{1}{2a(t)s(t)}
\left[
\varphi\!\left(\frac{x+2a(t)}{s(t)}\right)
-
\varphi\!\left(\frac{x+a(t)}{s(t)}\right)
+
\varphi\!\left(\frac{x-a(t)}{s(t)}\right)
-
\varphi\!\left(\frac{x-2a(t)}{s(t)}\right)
\right].
\tag{8}
```

Note from the PDF: you are **NOT** requested to prove any of the formulas (7), (8), (9); doing so will result in a penalty.

Here `\Phi` is the standard normal CDF, `s(t)=\sqrt{1-e^{-\beta t}}`, and `\varphi` is the standard normal PDF. Note that (8) is obtained from (7) by differentiation. Hence we know explicitly the quantity `\partial_x \log p_t(x)`, called the *score*:

```math
\partial_x \log p_t(x)
=
\frac{p'_t(x)}{p_t(x)},
\qquad
t>0.
\tag{9}
```

## Backward probability-flow ODE

To the forward SDE (5), we associate the ODE:

```math
Y'_t(\omega)
=
-\frac{\beta}{2}Y_t(\omega)
-
\frac{\beta}{2}
\partial_y \log p_t\!\left(Y_t(\omega)\right),
\qquad
t \in (0,T].
\tag{10}
```

To run it backward, integrate (10) from `t = T` down to `t = \epsilon` (small `\epsilon > 0`) using the time grid `(t_n)` in decreasing order. `Y_t` is written as a random variable because the value `Y_T` is not fixed but is sampled from a standard normal variable.

Each sampled value results in another ODE trajectory; we are interested in the distribution of `Y_t(\omega)` for `t` in some temporal grid.

**Note (avoid `t = 0`).** Since `s(t) -> 0` as `t` decreases to `0`, evaluate the ODE only for `t >= \epsilon`, for example `\epsilon = 10^{-3}`, when computing (9).

## Parameters

Example choices:

```text
T = 2
beta = 1
n_SDE = 10^5      # number of SDE trajectories; adjust if necessary
n_T = 10          # number of snapshot times
Delta t = 10^-3   # SDE time step
snapshot times: 0 < t_1 < ... < t_{n_T} = T, e.g. log-uniform
n_ODE >= 5 * 10^3 # number of ODE trajectories; adjust if necessary
```

## What to do

### 1. Forward SDE simulation (Euler-Maruyama)

Simulate (5) with step `\Delta t` for `n_SDE` trajectories. Store samples

```math
\{X_{t_m}^{(k)}\}_{k=1}^{n_{SDE}}
```

at each snapshot time `t_m`, `m = 1, ..., n_T`.

### 2. Backward ODE simulation

Sample `Y_T` at terminal time; for each randomness `\omega_\ell`, solve (10) backward from `T` to `\epsilon`. Store samples at the same snapshot times `{t_m}`.

Ideally you should use a RK4 or Heun scheme, **VECTORIZED** to advance all ODEs at the same time. Otherwise use `odeint` non-vectorized (penalty).

### 3. Comparison by marginal snapshots

For each

```math
t_m \in \{0,t_1,\ldots,t_{n_T}=T\},
```

plot on the same figure:

- a histogram (`density=True`) of the forward SDE samples

```math
\{X_{t_m}^{(k)}: k \le n_{SDE}\},
```

- a histogram (`density=True`) of the ODE samples

```math
\{Y_{t_m}(\omega_\ell): \ell \le n_{ODE}\},
```

with transparency (`alpha`) so both are visible. Use identical bins and identical `x`-range for the two histograms.

## Required / questions

- Code implementing (5) (Euler-Maruyama), (7)-(9), and (10).
- A grid of `n_T` plots overlaying the two histograms at each snapshot time.
- What do you observe? Can you explain theoretically?

## Implementation file

- Notebook: `vp_sde_ode.ipynb`
