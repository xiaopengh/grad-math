"""Command-line interface for auto-anki."""

from pathlib import Path
from typing import Optional

import click

from .parser import FlashcardParser
from .tsv_generator import TSVGenerator


@click.group()
@click.version_option(version="0.1.0", prog_name="auto-anki")
def cli():
    """Auto-Anki: Convert markdown flashcards to TSX components for Anki import."""
    pass


@cli.command()
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--output-dir",
    "-o",
    type=click.Path(path_type=Path),
    default="output",
    help="Output directory for generated files"
)
@click.option(
    "--format",
    "-f",
    type=click.Choice(["tsv", "deck-specific"]),
    default="tsv",
    help="Output format: tsv (single file) or deck-specific (one TSV per deck)"
)
def convert(
    input_file: Path,
    output_dir: Path,
    format: str
) -> None:
    """Convert markdown flashcards to the specified format."""
    click.echo(f"Processing: {input_file}")

    try:
        # Parse flashcards
        parser = FlashcardParser()
        flashcards = parser.parse_file(input_file)

        if not flashcards:
            click.echo("No flashcards found in the input file.", err=True)
            return

        click.echo(f"Found {len(flashcards)} flashcards")

        # Generate output based on format
        generator = TSVGenerator(output_dir)

        if format == "tsv":
            output_file = generator.generate_tsv(flashcards)
            click.echo(f"Generated TSV file: {output_file}")
        elif format == "deck-specific":
            output_files = generator.generate_deck_specific_tsvs(flashcards)
            click.echo(f"Generated {len(output_files)} deck-specific TSV files:")
            for file in output_files:
                click.echo(f"  {file.name}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        raise click.Abort()


@cli.command()
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
def validate(input_file: Path) -> None:
    """Validate markdown flashcard format."""
    click.echo(f"Validating: {input_file}")

    try:
        parser = FlashcardParser()
        flashcards = parser.parse_file(input_file)

        click.echo(f"✓ File is valid with {len(flashcards)} flashcards")

        # Show summary
        decks = {}
        total_tags = set()

        for card in flashcards:
            deck = card.metadata.deck
            if deck not in decks:
                decks[deck] = 0
            decks[deck] += 1
            total_tags.update(card.metadata.tags)

        click.echo("\nSummary:")
        click.echo(f"  Total cards: {len(flashcards)}")
        click.echo(f"  Unique decks: {len(decks)}")
        click.echo(f"  Unique tags: {len(total_tags)}")

        if decks:
            click.echo("\nCards per deck:")
            for deck, count in sorted(decks.items()):
                click.echo(f"  {deck}: {count}")

    except Exception as e:
        click.echo(f"✗ Validation failed: {e}", err=True)
        raise click.Abort()


@cli.command()
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--card-id",
    "-c",
    type=str,
    help="Show specific card by ID"
)
def preview(input_file: Path, card_id: Optional[str]) -> None:
    """Preview flashcards from markdown file."""
    try:
        parser = FlashcardParser()
        flashcards = parser.parse_file(input_file)

        if card_id:
            # Show specific card
            card = next((c for c in flashcards if c.metadata.id == card_id), None)
            if not card:
                click.echo(f"Card with ID '{card_id}' not found", err=True)
                return

            _preview_single_card(card)
        else:
            # Show all cards
            for i, card in enumerate(flashcards, 1):
                click.echo(f"\n{'='*50}")
                click.echo(f"Card {i}: {card.metadata.id}")
                click.echo(f"{'='*50}")
                _preview_single_card(card)

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        raise click.Abort()




def _preview_single_card(card) -> None:
    """Preview a single flashcard."""
    click.echo(f"Deck: {card.metadata.deck}")
    click.echo(f"Tags: {', '.join(card.metadata.tags)}")
    click.echo(f"Type: {card.metadata.note_type}")
    click.echo()
    click.echo("FRONT:")
    click.echo(card.front)
    click.echo()
    click.echo("BACK:")
    click.echo(card.back)


def main() -> None:
    """Entry point for the CLI."""
    cli()


if __name__ == "__main__":
    main()