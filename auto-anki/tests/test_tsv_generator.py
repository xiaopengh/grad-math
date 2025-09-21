"""Tests for TSV generator."""

import pytest
from pathlib import Path
from auto_anki.tsv_generator import TSVGenerator
from auto_anki.models import Flashcard, FlashcardMetadata


class TestTSVGenerator:
    """Test cases for TSVGenerator."""

    def test_generate_tsv(self, tmp_path):
        """Test TSV generation."""
        generator = TSVGenerator(tmp_path)

        flashcards = [
            Flashcard(
                metadata=FlashcardMetadata(
                    id="card1",
                    deck="math",
                    tags=["probability", "statistics"],
                    note_type="basic"
                ),
                front="What is \\(E[X]\\)?",
                back="Expected value of X"
            ),
            Flashcard(
                metadata=FlashcardMetadata(
                    id="card2",
                    deck="physics",
                    tags=["mechanics"],
                    note_type="basic"
                ),
                front="What is F = ma?",
                back="Newton's second law"
            )
        ]

        output_file = generator.generate_tsv(flashcards)

        assert output_file.exists()
        assert output_file.name == "flashcards.tsv"

        # Check content
        content = output_file.read_text(encoding='utf-8')
        lines = content.strip().split('\n')

        assert len(lines) == 2

        # Check first card
        parts1 = lines[0].split('\t')
        assert len(parts1) == 5
        assert "What is \\(E[X]\\)?" in parts1[0]
        assert "Expected value of X" in parts1[1]
        assert parts1[2] == "probability statistics"
        assert parts1[3] == "math"
        assert parts1[4] == "basic"

    def test_clean_content(self, tmp_path):
        """Test content cleaning."""
        generator = TSVGenerator(tmp_path)

        content_with_newlines = "Line 1\nLine 2\n\nLine 3"
        cleaned = generator._clean_content(content_with_newlines)

        assert "<br>" in cleaned
        assert "\n" not in cleaned

    def test_generate_deck_specific_tsvs(self, tmp_path):
        """Test generating deck-specific TSVs."""
        generator = TSVGenerator(tmp_path)

        flashcards = [
            Flashcard(
                metadata=FlashcardMetadata(
                    id="math1",
                    deck="mathematics",
                    tags=["algebra"],
                    note_type="basic"
                ),
                front="What is x + 1 = 0?",
                back="x = -1"
            ),
            Flashcard(
                metadata=FlashcardMetadata(
                    id="math2",
                    deck="mathematics",
                    tags=["algebra"],
                    note_type="basic"
                ),
                front="What is 2x = 4?",
                back="x = 2"
            ),
            Flashcard(
                metadata=FlashcardMetadata(
                    id="physics1",
                    deck="physics",
                    tags=["motion"],
                    note_type="basic"
                ),
                front="What is velocity?",
                back="Rate of change of position"
            )
        ]

        output_files = generator.generate_deck_specific_tsvs(flashcards)

        assert len(output_files) == 2

        math_file = next(f for f in output_files if f.name == "mathematics.tsv")
        physics_file = next(f for f in output_files if f.name == "physics.tsv")

        # Check math file has 2 cards
        math_content = math_file.read_text(encoding='utf-8')
        math_lines = math_content.strip().split('\n')
        assert len(math_lines) == 2

        # Check physics file has 1 card
        physics_content = physics_file.read_text(encoding='utf-8')
        physics_lines = physics_content.strip().split('\n')
        assert len(physics_lines) == 1