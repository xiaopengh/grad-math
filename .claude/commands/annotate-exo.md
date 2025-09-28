# Annotate Exercise Questions

Documents to be annotated: $ARGUMENTS

**Instructions:**

**Above all:** Consult course materials within the same directory as the input tex file and exclusively reference content from the provided course materials!

1. Annotate the exercise questions in the provided LaTeX files.
2. Each exercise is enclosed in the `\begin{exercise} ... \end{exercise}` environment. To annotate an exercise, use reminder environments as shown below:
```latex
\begin{exercise}
    ... % exercise content
    \begin{reminder}
        ... % reminder content
    \end{reminder}
    ... % more exercise content
\end{exercise}
```

3. **Annotation Guidelines:**
   - Add reminders for key theorems, definitions, or formulas needed to solve the exercise
   - Include relevant background knowledge that students might need
   - Mention important techniques or methods applicable to the problem
   - Reference related concepts from previous chapters or exercises
   - Highlight common pitfalls or mistakes to avoid

4. **When to Add Reminders:**
   - Before complex mathematical derivations
   - When specialized theorems are required
   - For exercises involving multiple concepts
   - When notation might be confusing
   - Before applying non-trivial techniques

5. **Content of Reminders:**
   - Keep reminders concise but informative
   - Use proper mathematical notation
   - Reference specific theorem names when applicable
   - Include brief explanations of key concepts
   - Mention relevant assumptions or conditions

**Example with good annotation practice:**
```latex
\begin{exercise}[Gauss-Markov Theorem Application]
Let $X$ be the design matrix and $Y = X\beta + \epsilon$ be the linear model.
\begin{reminder}
Recall that the Gauss-Markov theorem states that under the assumptions of linearity, unbiasedness, and homoscedasticity, the OLS estimator $\hat{\beta} = (X^T X)^{-1}X^T Y$ is the Best Linear Unbiased Estimator (BLUE).
\end{reminder}
Show that $\hat{\beta}$ has minimum variance among all linear unbiased estimators.
\begin{reminder}
Use the fact that for any other linear unbiased estimator $\tilde{\beta} = CY$, we have $\text{Var}(\tilde{\beta}) - \text{Var}(\hat{\beta})$ is positive semi-definite.
\end{reminder}
\end{exercise}
```

6. **Formatting Notes:**
   - Place the reminder at the **end** of each exercise. 
   - Place reminders strategically within the exercise content where they are most relevant
   - Ensure reminders are contextually appropriate to the immediate problem
   - Maintain the natural flow and readability of the exercise despite added annotations
   - Avoid over-annotating - only add reminders where they genuinely help understanding