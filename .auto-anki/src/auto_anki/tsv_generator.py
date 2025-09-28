"""TSV generator for Anki import."""

import csv
from pathlib import Path
from typing import List

from .models import Flashcard


class TSVGenerator:
    """Generator for creating TSV files from flashcards for Anki import."""

    def __init__(self, output_dir: Path) -> None:
        """Initialize the generator."""
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def generate_tsv(self, flashcards: List[Flashcard], filename: str = "flashcards.tsv") -> Path:
        """Generate TSV file for Anki import."""
        output_file = self.output_dir / filename

        with open(output_file, 'w', newline='', encoding='utf-8') as tsvfile:
            # Use tab as delimiter for TSV
            writer = csv.writer(tsvfile, delimiter='\t')

            for flashcard in flashcards:
                # Format: Front, Back, Tags, Deck, Note Type
                writer.writerow([
                    self._clean_content(flashcard.front),
                    self._clean_content(flashcard.back),
                    ' '.join(flashcard.metadata.tags),  # Space-separated tags
                    flashcard.metadata.deck,
                    flashcard.metadata.note_type
                ])

        return output_file

    def _clean_content(self, content: str) -> str:
        """Clean content for Anki import."""
        # Replace newlines with <br> for HTML line breaks in Anki
        content = content.replace('\n', '<br>')

        # Convert LaTeX delimiters to Anki-compatible format
        # Anki uses [latex] ... [/latex] or MathJax $$ ... $$
        # Keep the \( \) and \[ \] format as they work with MathJax in Anki

        # Remove any extra whitespace
        content = ' '.join(content.split())

        return content

    def generate_deck_specific_tsvs(self, flashcards: List[Flashcard]) -> List[Path]:
        """Generate separate TSV files for each deck."""
        deck_cards = {}

        # Group cards by deck
        for flashcard in flashcards:
            deck_name = flashcard.metadata.deck
            if deck_name not in deck_cards:
                deck_cards[deck_name] = []
            deck_cards[deck_name].append(flashcard)

        generated_files = []

        # Generate TSV for each deck
        for deck_name, cards in deck_cards.items():
            filename = f"{deck_name}.tsv"
            output_file = self.generate_tsv(cards, filename)
            generated_files.append(output_file)

        return generated_files