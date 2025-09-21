"""Data models for flashcard representation."""

from typing import List
from pydantic import BaseModel, Field


class FlashcardMetadata(BaseModel):
    """Metadata for a flashcard from YAML frontmatter."""

    id: str = Field(..., description="Unique identifier for the flashcard")
    deck: str = Field(..., description="Anki deck name")
    tags: List[str] = Field(default_factory=list, description="List of tags")
    note_type: str = Field(default="basic", description="Anki note type")


class Flashcard(BaseModel):
    """Complete flashcard with metadata, front, and back content."""

    metadata: FlashcardMetadata = Field(..., description="Card metadata")
    front: str = Field(..., description="Front content (question)")
    back: str = Field(..., description="Back content (answer)")

    @property
    def tsx_filename(self) -> str:
        """Generate TSX filename from card ID."""
        return f"{self.metadata.id}.tsx"