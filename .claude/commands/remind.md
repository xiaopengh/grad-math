# Generate Exercise Reminders Document

Generate a standalone reference document with helpful reminders for solving exercises.

**Arguments:**
- $COURSE_FILE: Path to the course material file (lecture.pdf or poly.pdf)
- $EXERCISE_FILE: Path to the exercise sheet LaTeX file

**Instructions:**

1. **Parse the exercise file**: Extract all exercises from the LaTeX file to understand what topics and concepts are covered.

2. **Consult course materials**: Use content exclusively from the course file to find relevant theorems, definitions, formulas, and techniques.

4. **Content organization**: Group reminders by topic or theme rather than by individual exercise. This creates a more useful reference document.

5. **What to include**:
   - Theorem statements with names
   - Key definitions with mathematical notation
   - Important formulas and identities
   - Common techniques and methods
   - Relevant assumptions and conditions
   - Useful lemmas and corollaries

6. **Cross-referencing**: When appropriate, indicate which exercises might use each reminder (e.g., "Relevant for Exercises 2, 5").

7. **Clarity**: Write reminders to be self-contained and clear, as students will reference this separately from both the course material and exercises.

**Example Output Structure:**
```latex
\section{Linear Regression}

\subsection{Gauss-Markov Theorem}
\textbf{Statement:} Under the assumptions of linearity ($Y = X\beta + \epsilon$), unbiasedness ($\mathbb{E}[\epsilon|X] = 0$), and homoscedasticity ($\text{Var}(\epsilon|X) = \sigma^2 I$), the OLS estimator
$$\hat{\beta} = (X^T X)^{-1}X^T Y$$
is the Best Linear Unbiased Estimator (BLUE).

\textit{Relevant for: Exercises 1, 3, 5}

\subsection{Properties of OLS}
\begin{itemize}
\item Unbiasedness: $\mathbb{E}[\hat{\beta}] = \beta$
\item Variance: $\text{Var}(\hat{\beta}) = \sigma^2(X^TX)^{-1}$
\item Residuals: $\hat{\epsilon} = Y - X\hat{\beta} = (I - H)Y$ where $H = X(X^TX)^{-1}X^T$
\end{itemize}

\textit{Relevant for: Exercises 2, 4}
```

**Output:**
- Filename: `reminders_[exercise_sheet_name].tex`