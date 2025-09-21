# Auto-Anki

A Python tool for automatically converting markdown-based flashcards into TSV (tab-separated values) files that can be imported directly into Anki.

## Project Overview

This project streamlines the process of creating Anki flashcards from structured markdown files. Instead of manually creating cards in Anki's interface, you can write your flashcards in markdown format with YAML frontmatter and automatically convert them to TSV files that can be imported directly into Anki.

## Project Structure

```
auto-anki/
├── src/auto_anki/         # Main package source code
│   ├── __init__.py        # Package initialization
│   ├── models.py          # Data models for flashcards
│   ├── parser.py          # Markdown parser with YAML frontmatter
│   ├── tsv_generator.py   # TSV file generator for Anki import
│   └── cli.py            # Command-line interface
├── tests/                 # Test suite
├── discrete_processes/    # Source markdown files with flashcard content
│   └── chap1.md          # Example: Discrete Processes flashcards
├── output/               # Generated TSV files (created by tool)
├── archive/              # Archived materials
├── pdf/                  # Source PDF materials
├── pyproject.toml        # Project configuration and dependencies
├── CLAUDE.md            # Development guidelines for Claude Code
└── README.md            # This file
```

## Flashcard Format

The markdown files in `discrete_processes/` follow a structured format with specific parsing rules:

### Format Structure

```markdown
## card-id
```yaml
id: unique-card-id
deck: deck_name
tags: [tag1, tag2, tag3]
note_type: basic|cloze|image_occlusion
```

Front of card (question) content here...

Back of card (answer) content here...
```

### Key Parsing Rules

1. **Card Separation**: Each flashcard starts with `## card-id` header
2. **Metadata**: YAML frontmatter block contains card configuration
3. **Front/Back Separation**: **The front and back of each card are always separated by a single empty line**
4. **Content**: Everything after the YAML block until the empty line is the **front (question)**
5. **Answer**: Everything after the empty line until the next card header is the **back (answer)**
6. **Mathematical Expressions**: All mathematical formulas use LaTeX delimiters compatible with MathJax in Anki:
   - Inline math: `\( formula \)`
   - Display math: `\[ formula \]`

### Example

```markdown
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
This is very useful in practice because it allows us to compute conditional expectations by integrating over events in \( \mathcal{G} \).
3. (equivalent to 2) For all \( Z \in L^1(\Omega, \mathcal{G}, \mathbb{P}) \)
\[ \mathbb{E}[X Z] = \mathbb{E}[\mathbb{E}[X \mid \mathcal{G}] Z] \]
```

In this example:
- **Front**: The question asking about defining properties
- **Back**: The numbered list of three defining properties (separated by the empty line)

## TSV Output Format

The tool generates TSV (tab-separated values) files with the following columns:

1. **Front**: Question content with LaTeX math notation (`\( \)` for inline, `\[ \]` for display)
2. **Back**: Answer content with `<br>` tags for line breaks
3. **Tags**: Space-separated list of tags
4. **Deck**: Deck name from YAML frontmatter
5. **Note Type**: Note type (basic, cloze, etc.)

This format can be directly imported into Anki using the "Import" feature.

## Development Environment

This project uses UV for Python package management and follows strict coding standards outlined in `CLAUDE.md`.

### Key Technologies
- **UV**: Fast Python package and environment management
- **Ruff**: Code formatting and linting
- **Pytest**: Testing framework
- **Pydantic v2**: Data validation
- **Click**: Command-line interface

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd auto-anki

# Install dependencies
uv sync
```

### Development Commands

```bash
# Run tests
uv run pytest

# Format code
uv run ruff format .

# Check linting
uv run ruff check .

# Type checking
uv run mypy src/
```

## Usage

The Auto-Anki CLI provides several commands to work with your flashcard files:

### Command Overview

```bash
# Get help and see all available commands
uv run auto-anki --help

# Get help for a specific command
uv run auto-anki convert --help
uv run auto-anki validate --help
uv run auto-anki preview --help
```

### Convert Flashcards to TSV

```bash
# Convert to single TSV file (default format)
uv run auto-anki convert discrete_processes/chap1.md

# Explicitly specify TSV format
uv run auto-anki convert discrete_processes/chap1.md --format tsv

# Convert to deck-specific TSV files (one file per deck)
uv run auto-anki convert discrete_processes/chap1.md --format deck-specific

# Specify custom output directory
uv run auto-anki convert discrete_processes/chap1.md --output-dir my_cards

# Full example with all options
uv run auto-anki convert discrete_processes/chap1.md \
    --output-dir output \
    --format deck-specific
```

**Convert Command Options:**
- `--output-dir, -o`: Output directory for generated files (default: `output`)
- `--format, -f`: Output format - `tsv` (single file) or `deck-specific` (one file per deck)

### Validate Flashcard Format

```bash
# Validate markdown file format
uv run auto-anki validate discrete_processes/chap1.md

# Example output:
# ✓ File is valid with 17 flashcards
#
# Summary:
#   Total cards: 17
#   Unique decks: 1
#   Unique tags: 6
#
# Cards per deck:
#   discrete_processes: 17
```

**What validation checks:**
- YAML frontmatter syntax
- Required fields (id, deck)
- Front/back separation (empty line)
- Non-empty content
- Card header format

### Preview Flashcards

```bash
# Preview all cards in the file
uv run auto-anki preview discrete_processes/chap1.md

# Preview specific card by ID
uv run auto-anki preview discrete_processes/chap1.md --card-id ce-def-theorem

# Example output for specific card:
# Deck: discrete_processes
# Tags: probability, conditional-expectation
# Type: basic
#
# FRONT:
# Let \(X \in L^1\), we denote \( \mathbb{E}[X \mid \mathcal{G}] \)...
#
# BACK:
# 1. \( \mathbb{E}[X \mid \mathcal{G}] \) is \( \mathcal{G} \)-measurable...
```

**Preview Command Options:**
- `--card-id, -c`: Show specific card by its ID

### Import into Anki

1. Generate TSV file using the `convert` command
2. Open Anki and go to **File → Import**
3. Select the generated TSV file
4. Configure the import settings:
   - **Field separator**: Tab
   - **Field mapping**: Front, Back, Tags, Deck, Note Type
   - **Note type**: Basic (or create custom type)
   - **Deck**: Choose target deck or use deck column
5. Click **Import**

## Current Status

The project is **complete and fully functional** with:
- ✅ Markdown parser for flashcard format with YAML frontmatter
- ✅ TSV generator with proper Anki formatting
- ✅ Command-line interface with multiple commands
- ✅ Comprehensive testing suite
- ✅ Mathematical notation support (LaTeX with MathJax compatibility)
- ✅ Validation and preview functionality

## Example Output

Converting `discrete_processes/chap1.md` produces a TSV file with 17 flashcards covering probability theory and conditional expectation concepts, ready for direct import into Anki.