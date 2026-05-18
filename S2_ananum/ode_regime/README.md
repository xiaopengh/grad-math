# ODE System, code `ode_regime`

Extracted instructions from `evaluation_TP_M1_P26_v1s.pdf`.

## General project rules

- Add comments explaining the code.
- Do not use hard-coded parameter values after assigning them: reuse the parameter variable.
- Refer to formula numbers from the subject when possible.
- Keep the implementation understandable for an M1 colleague.
- If an implementation choice differs from a class correction, explain why.
- Keep the Python code clear and efficient; avoid loops except when they are needed for time stepping.
- The program must run without configuration and should use only standard classroom packages.
- The program should start with a comment containing the author name(s), chosen project, and date.
- For submission, the expected file format is `.py` or `.ipynb`.

## Model

For classes `j = 1, ..., J`, simulate

```text
dX_j/dt = -lambda_j X_j + p(1 - m) q_j L_X,    X_j(0) = X_j0
dY_j/dt = p m lambda_j X_j + g Y_j,            Y_j(0) = 0
L_X(t) = sum_k lambda_k X_k(t)
```

The simulation must keep all classes non-negative.

## Parameters

```text
T = 300
J = 10
lambda_j = 10 ** linspace(-6, 3, J)
q_j = 1 / J
p = 0.95
m = 10 ** (-8)
g = 0.1
X_init = 10 ** 6
X_j0 = X_init / J
```

## Required plots and comparisons

- Plot the evolution of `sum_j X_j(t)` and `sum_j Y_j(t)`.
- Plot class proportions `X_j(t) / sum_a X_a(t)` and the same quantity for `Y_j`.
- Plot histograms/bar charts of classes `X_j` with respect to `j`, and the same for `Y_j`.
- Compare the baseline dynamics with the model where `g sqrt(Y_j)` replaces `g Y_j` in the ODE for `Y_j`.
- Compare with the stochastic model

```text
dY_j = (p m lambda_j X_j + g sqrt(Y_j)) dt
       + sqrt(g sqrt(Y_j)) dW_t^j
```

where the Brownian motions are independent and all `X_j`, `Y_j` remain non-negative.

- By changing parameters, look for a regime where the final class histogram of `Y_j` is unimodal with its maximum in the middle.

## Implementation file

- Notebook: `ode_regime.ipynb`
- Replace the `TODO` author placeholder in the first code cell before submitting.
