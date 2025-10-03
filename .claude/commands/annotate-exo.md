# Annotate Exercise Questions

Documents to be annotated: $ARGUMENTS

**Instructions:**

1. **Consult course materials**: Use content exclusively from lecture.pdf or poly.pdf in the same directory as the input tex file.

2. **Add reminder environments**: Place reminders within exercises using the format:
   ```latex
   \begin{exercise}
       ... % exercise content
       \begin{reminder}
           ... % reminder content
       \end{reminder}
       ... % more exercise content
   \end{exercise}
   ```

3. **When to annotate**: Add reminders for key theorems, definitions, formulas, complex derivations, specialized techniques, and to highlight common pitfalls.

4. **Reminder content**: Keep concise but informative. Include theorem names, mathematical notation, key concepts, relevant assumptions, and brief explanations.

5. **Placement**: Position reminders strategically within exercises where most relevant, typically at the end or before complex steps.

**Example:**
```latex
\begin{exercise}[Gauss-Markov Theorem Application]
Let $X$ be the design matrix and $Y = X\beta + \epsilon$ be the linear model.
\begin{reminder}
The Gauss-Markov theorem: under linearity, unbiasedness, and homoscedasticity assumptions, OLS estimator $\hat{\beta} = (X^T X)^{-1}X^T Y$ is BLUE.
\end{reminder}
Show that $\hat{\beta}$ has minimum variance among all linear unbiased estimators.
\begin{reminder}
For any other linear unbiased estimator $\tilde{\beta} = CY$, use that $\text{Var}(\tilde{\beta}) - \text{Var}(\hat{\beta})$ is positive semi-definite.
\end{reminder}
\end{exercise}
```