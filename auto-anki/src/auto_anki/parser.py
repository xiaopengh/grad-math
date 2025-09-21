"""Markdown parser for flashcard format with YAML frontmatter."""

import re
from pathlib import Path
from typing import List

import yaml

from .models import Flashcard, FlashcardMetadata


class FlashcardParser:
    """Parser for markdown files containing flashcards."""

    def __init__(self) -> None:
        """Initialize the parser."""
        self.card_header_pattern = re.compile(r'^## (.+)$', re.MULTILINE)
        self.yaml_block_pattern = re.compile(
            r'^```yaml\s*\n(.*?)\n```', re.MULTILINE | re.DOTALL
        )

    def parse_file(self, file_path: Path) -> List[Flashcard]:
        """Parse a markdown file and extract all flashcards."""
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")

        content = file_path.read_text(encoding="utf-8")
        return self.parse_content(content)

    def parse_content(self, content: str) -> List[Flashcard]:
        """Parse markdown content and extract flashcards."""
        flashcards = []

        # Split content by card headers
        card_sections = self._split_by_headers(content)

        for section in card_sections:
            if section.strip():
                flashcard = self._parse_single_card(section)
                if flashcard:
                    flashcards.append(flashcard)

        return flashcards

    def _split_by_headers(self, content: str) -> List[str]:
        """Split content by ## headers, keeping each header with its content."""
        # Find all header positions
        headers = list(self.card_header_pattern.finditer(content))

        if not headers:
            return []

        sections = []
        for i, header in enumerate(headers):
            start = header.start()
            end = headers[i + 1].start() if i + 1 < len(headers) else len(content)
            sections.append(content[start:end])

        return sections

    def _parse_single_card(self, card_content: str) -> Flashcard | None:
        """Parse a single card section."""
        lines = card_content.strip().split('\n')

        if not lines or not lines[0].startswith('## '):
            return None

        # Extract card ID from header
        card_id = lines[0][3:].strip()

        # Find YAML block
        yaml_match = self.yaml_block_pattern.search(card_content)
        if not yaml_match:
            return None

        try:
            yaml_data = yaml.safe_load(yaml_match.group(1))
            metadata = FlashcardMetadata(**yaml_data)
        except (yaml.YAMLError, ValueError) as e:
            raise ValueError(f"Invalid YAML in card {card_id}: {e}")

        # Extract content after YAML block
        yaml_end = yaml_match.end()
        remaining_content = card_content[yaml_end:].strip()

        # Split by empty line to separate front and back
        parts = remaining_content.split('\n\n', 1)

        if len(parts) < 2:
            raise ValueError(f"Card {card_id} missing front/back separator (empty line)")

        front = parts[0].strip()
        back = parts[1].strip()

        if not front or not back:
            raise ValueError(f"Card {card_id} has empty front or back content")

        return Flashcard(
            metadata=metadata,
            front=front,
            back=back
        )