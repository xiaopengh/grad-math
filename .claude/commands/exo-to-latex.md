# Convert PDF or Image Files to LaTeX Exercise Format

Documents to be converted: $ARGUMENTS

**Instructions:**
1. These documents contain exercise questions from various mathematical fields.
2. The converted file should follow the exercise structure shown below:
```latex
\begin{exercise}
    ... % exercise content 
\end{exercise}
```
3. For questions with multiple subquestions:
   - Use `\begin{enumerate}` or `\begin{itemize}` environments for proper formatting
   - Follow the enumeration style of the original document
   - Always use LaTeX environments instead of hard-coded numbering like "1)"
   - Preserve the original question numbering scheme (a), b), c) or i), ii), iii), etc.)

4. Additional formatting guidelines:
   - Use appropriate mathematical notation with `\(...\)` for inline math and `\[...\]` for display math
   - Preserve any diagrams or figures by describing them or using TikZ when possible
   - Maintain the logical structure and hierarchy of the original document
   - Include any special instructions or hints that appear in the original

5. If the document quality is poor or text is unclear:
   - Note any uncertainties with comments like `% unclear text: [best guess]`
   - Preserve the overall structure even if some details are missing

6. **File Output:** Generate the LaTeX output file in the same directory as the source file, using the same filename before `.` but ending with`.tex`

**Example output structure:**
```latex
\begin{exercise}
Let $f: \mathbb{R} \to \mathbb{R}$ be a continuous function.
\begin{enumerate}
    \item[(a)] Show that $f$ is uniformly continuous.
    \item[(b)] Prove or disprove: $f$ is bounded.
\end{enumerate}
\end{exercise}
```