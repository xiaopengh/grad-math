"""Tests for flashcard parser."""

import pytest
from pathlib import Path
from auto_anki.parser import FlashcardParser
from auto_anki.models import Flashcard, FlashcardMetadata


class TestFlashcardParser:
    """Test cases for FlashcardParser."""

    def setup_method(self):
        """Set up test fixtures."""
        self.parser = FlashcardParser()

    def test_parse_single_card(self):
        """Test parsing a single card."""
        content = '''## test-card
```yaml
id: test-card
deck: test_deck
tags: [math, probability]
note_type: basic
```

What is the definition of conditional expectation?

The conditional expectation E[X|G] is a random variable that satisfies two properties.'''

        flashcards = self.parser.parse_content(content)

        assert len(flashcards) == 1
        card = flashcards[0]
        assert card.metadata.id == "test-card"
        assert card.metadata.deck == "test_deck"
        assert card.metadata.tags == ["math", "probability"]
        assert card.metadata.note_type == "basic"
        assert "What is the definition" in card.front
        assert "The conditional expectation" in card.back

    def test_parse_multiple_cards(self):
        """Test parsing multiple cards."""
        content = '''## card1
```yaml
id: card1
deck: deck1
tags: [tag1]
note_type: basic
```

Question 1?

Answer 1.


## card2
```yaml
id: card2
deck: deck2
tags: [tag2]
note_type: basic
```

Question 2?

Answer 2.'''

        flashcards = self.parser.parse_content(content)

        assert len(flashcards) == 2
        assert flashcards[0].metadata.id == "card1"
        assert flashcards[1].metadata.id == "card2"
        assert "Question 1" in flashcards[0].front
        assert "Question 2" in flashcards[1].front

    def test_parse_card_with_math(self):
        """Test parsing card with mathematical expressions."""
        content = '''## math-card
```yaml
id: math-card
deck: math
tags: [probability]
note_type: basic
```

Let \\(X \\in L^1\\), what is \\(\\mathbb{E}[X \\mid \\mathcal{G}]\\)?

The conditional expectation satisfies:
\\[ \\mathbb{E}[X \\mathbf{1}_A] = \\mathbb{E}[\\mathbb{E}[X \\mid \\mathcal{G}] \\mathbf{1}_A] \\]'''

        flashcards = self.parser.parse_content(content)

        assert len(flashcards) == 1
        card = flashcards[0]
        assert "\\(X \\in L^1\\)" in card.front
        assert "\\[ \\mathbb{E}[X" in card.back

    def test_missing_yaml_block(self):
        """Test handling of missing YAML block."""
        content = '''## invalid-card

This card has no YAML block.

This should fail parsing.'''

        flashcards = self.parser.parse_content(content)
        assert len(flashcards) == 0

    def test_missing_front_back_separator(self):
        """Test handling of missing front/back separator."""
        content = '''## invalid-card
```yaml
id: invalid-card
deck: test
tags: []
note_type: basic
```

This card has no empty line separator so front and back cannot be distinguished.'''

        with pytest.raises(ValueError, match="missing front/back separator"):
            self.parser.parse_content(content)

    def test_empty_front_or_back(self):
        """Test handling of empty front or back content."""
        content = '''## invalid-card
```yaml
id: invalid-card
deck: test
tags: []
note_type: basic
```

This has front content.

'''

        with pytest.raises(ValueError, match="missing front/back separator"):
            self.parser.parse_content(content)

    def test_invalid_yaml(self):
        """Test handling of invalid YAML."""
        content = '''## invalid-card
```yaml
id: invalid-card
deck: test
tags: [unclosed list
note_type: basic
```

Question?

Answer.'''

        with pytest.raises(ValueError, match="Invalid YAML"):
            self.parser.parse_content(content)