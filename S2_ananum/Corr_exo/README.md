# LaTeX Correction Project, subject code `Corr_exo`

Extracted instructions from `evaluation_TP_M1_P26_v1s.pdf`.

## General project rules

- This project has no numerical programming.
- The correction must be written in LaTeX.
- Because there is no numerical programming, the maximum mark is `15/20`.
- Penalty for not writing in LaTeX: `15` points.
- The source should identify the author name(s), chosen project, and date.

## Exercises to correct

Each exercise is worth `5` points.

### Exercise 3.1: Existence of Implicit Schemes

Let `Psi: R_+ x R x R -> R` be Lipschitz with respect to all arguments, and let `h > 0`.

1. Show that

```text
y = x + h Psi(t, x, y)
```

has a unique solution for sufficiently small `h`, and provide a numerical method to compute it. The hint is to use Picard iterations. The solution is denoted `y = s(t, x, h)`.

2. Let `y` be a solution of the implicit equation, and let `phi` be defined by

```text
y = x + h phi(t, x).
```

Provide the formula for `phi` in terms of `s` and `Psi`, and show that `phi` is well-defined and Lipschitz for sufficiently small `h`.

### Exercise 3.2: Euler-Lagrange Multipliers, Finding the Derivative

Let `f, g: R^2 -> R` be `C^2`. Suppose `g(x, y) = 0` has a unique solution `y = Y(x)` near `x0`, with `Y` of class `C^1`. Let `y0 = Y(x0)` and assume `partial_y g(x0, y0) != 0`. Define `F(x) = f(x, Y(x))`. Introduce

```text
L(lambda, x, y) = f(x, y) + lambda g(x, y).
```

1. Show that there exists `lambda0` such that

```text
partial_y L(lambda0, x0, y0) = partial_lambda L(lambda0, x0, y0) = 0.
```

2. Show that

```text
partial_x F(x0) = partial_x L(lambda0, x0, y0).
```

3. Explain how this computes the gradient for `f(x, y) = x + y`, `g(x, y) = x^2 + y^2 - 1`.

4. Generalize to `f: R^n x R^n -> R`, `g: R^n x R^n -> R^n`.

### Exercise 3.3: Multi-Step SDE Scheme

With the notation from the course, consider

```text
Y_{n+1} = Y_n + (3/2) a_n h - (1/2) a_{n-1} h + b_n sqrt(h) xi_n,
```

where `xi_n` are discrete i.i.d. variables with mean `m` and finite variance `sigma^2`, independent of the filtration. The functions `a`, `b` are independent of time and `a`, `b`, `a'`, `b'`, `a''`, `b''` are bounded.

1. Find `m` and `sigma^2` such that the scheme is weakly consistent.
2. Decide whether the scheme is strongly consistent.

## Implementation file

- LaTeX source: `corr_exo.tex`
- Replace the `TODO` author placeholder before submitting.
