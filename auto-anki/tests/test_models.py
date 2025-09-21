"""Tests for data models."""

import pytest
from auto_anki.models import FlashcardMetadata, Flashcard


class TestFlashcardMetadata:
    """Test cases for FlashcardMetadata."""

    def test_valid_metadata(self):
        """Test creating valid metadata."""
        metadata = FlashcardMetadata(
            id="test-card",
            deck="test_deck",
            tags=["math", "probability"],
            note_type="basic"
        )

        assert metadata.id == "test-card"
        assert metadata.deck == "test_deck"
        assert metadata.tags == ["math", "probability"]
        assert metadata.note_type == "basic"

    def test_default_values(self):
        """Test default values."""
        metadata = FlashcardMetadata(
            id="test-card",
            deck="test_deck"
        )

        assert metadata.tags == []
        assert metadata.note_type == "basic"

    def test_required_fields(self):
        """Test that required fields raise errors when missing."""
        with pytest.raises(ValueError):
            FlashcardMetadata(deck="test_deck")  # Missing id

        with pytest.raises(ValueError):
            FlashcardMetadata(id="test-card")  # Missing deck


class TestFlashcard:
    """Test cases for Flashcard."""

    def test_valid_flashcard(self):
        """Test creating valid flashcard."""
        metadata = FlashcardMetadata(
            id="test-card",
            deck="test_deck",
            tags=["math"],
            note_type="basic"
        )

        flashcard = Flashcard(
            metadata=metadata,
            front="What is 2 + 2?",
            back="4"
        )

        assert flashcard.metadata.id == "test-card"
        assert flashcard.front == "What is 2 + 2?"
        assert flashcard.back == "4"

    def test_tsx_filename(self):
        """Test TSX filename generation."""
        metadata = FlashcardMetadata(
            id="ce-def-theorem",
            deck="discrete_processes"
        )

        flashcard = Flashcard(
            metadata=metadata,
            front="Question",
            back="Answer"
        )

        assert flashcard.tsx_filename == "ce-def-theorem.tsx"