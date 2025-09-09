# Copyright (C) 2025  Cash Prokop-Weaver
# Copyright (C) 2025  Leon Rische

# Author: Cash Prokop-Weaver <cash@cashpw.com>
# Author: Leon Rische <emacs@leonrische.me>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Minimal imports to switch to JSON-based exception handling
import sys, json, traceback

def json_excepthook(exc_type, exc_value, exc_traceback):
    error_info = {
        "error": str(exc_value),
        "type": exc_type.__name__,
        "traceback": "".join(traceback.format_exception(exc_type, exc_value, exc_traceback)),
    }
    # Always write to stdout as JSON
    print(json.dumps(error_info))
    sys.exit(1)

# Install our hook
sys.excepthook = json_excepthook

# remaining imports
from datetime import datetime, timezone
from typing import Iterator
from pathlib import Path
import argparse

from models import Indentifier, Review
from review_history import TSVReader

# Load the fsrs code shipped with org-fc
base = Path(__file__).resolve().parent
sys.path.insert(0, str(base / "py_fsrs"))
from py_fsrs import fsrs

def value_from_emacs(value: str):
    if value == "nil":
        return None
    return value

# Try to stick to language conventions
def keys_to_emacs(d: dict) -> dict:
    return {
        k.replace("_", "-"): keys_to_emacs(v) if isinstance(v, dict) else v
        for k, v in d.items()
    }

def keys_from_emacs(d: dict) -> dict:
    return {
        k.replace("-", "_"): keys_from_emacs(v) if isinstance(v, dict) else value_from_emacs(v)
        for k, v in d.items()
    }

RATINGS = {
    "again": fsrs.Rating.Again,
    "hard": fsrs.Rating.Hard,
    "good": fsrs.Rating.Good,
    "easy": fsrs.Rating.Easy,
}

def card_to_dict(card: fsrs.Card, position: str | None = None) -> dict:
    """Convert a fsrs.Card to a dictionary suitable for serialization."""
    card_dict = card.to_dict()
    # Org-fc does not make use of the card_id, so we remove it
    del card_dict["card_id"]

    if card.due is not None:
        card_dict["due"] = card.due.isoformat(timespec="seconds").replace("+00:00", "Z")
    if card.last_review is not None:
        card_dict["last_review"] = card.last_review.isoformat(timespec="seconds").replace("+00:00", "Z")

    if position is not None:
        card_dict["position"] = position

    card_dict = keys_to_emacs(card_dict)
    return card_dict

def initial():
    """Generate an initial FSRS card and print it in Emacs-friendly format.
    Optionally overwrite the card due date with --now argument.
    """

    parser = argparse.ArgumentParser(description="Generate initial FSRS card")
    parser.add_argument("--now", type=str, help="Overwrite the card due date (ISO format)")
    args, _ = parser.parse_known_args(sys.argv[2:])

    card = fsrs.Card()

    # Overwrite due date if --now is provided
    if args.now is not None:
        try:
            # Parse ISO formatted string to datetime and back to ISO string
            card.due = datetime.fromisoformat(args.now)
        except ValueError:
            raise ValueError("Invalid date format for --now. Use ISO format (YYYY-MM-DDTHH:MM:SS).")

    print(json.dumps(card_to_dict(card), indent=2))

def review():
    """Process a review and print the updated FSRS card in Emacs-friendly format."""
    parser = argparse.ArgumentParser(description="Review FSRS card")
    parser.add_argument("--now", type=str, help="Overwrite the current datetime (ISO format)")
    args, _ = parser.parse_known_args(sys.argv[2:])

    # Default to current UTC time
    now = datetime.now(timezone.utc)
    # Overwrite if --now is provided
    if args.now is not None:
        try:
            # Parse ISO formatted string to datetime and back to ISO string
            now = datetime.fromisoformat(args.now)
        except ValueError:
            raise ValueError("Invalid date format for --now. Use ISO format (YYYY-MM-DDTHH:MM:SSZ).")

    json_str = sys.stdin.read()
    try:
        request = json.loads(json_str)
    except json.JSONDecodeError as e:
        raise ValueError(f"Failed to parse JSON: {e}")

    scheduler_dict = request["scheduler"]
    scheduler_dict = keys_from_emacs(scheduler_dict)

    card_dict = request["card"]
    card_dict = keys_from_emacs(card_dict)

    # Mock the card_id
    card_dict["card_id"] = 0

    scheduler = fsrs.Scheduler.from_dict(scheduler_dict)
    card = fsrs.Card.from_dict(card_dict)

    # response_dict = card.to_dict()
    response_dict = scheduler.to_dict()
    response_dict = keys_to_emacs(response_dict)

    try:
        rating = RATINGS[request["rating"]]
    except KeyError:
        raise KeyError(f"Invalid or missing rating: %s", request.get("rating"))

    new_card, _revlog = scheduler.review_card(card, rating, review_datetime=now)

    print(json.dumps(card_to_dict(new_card), indent=2))

def replay_reviews(
        targets: list[tuple[Indentifier, datetime]],
        reviews: Iterator[Review],
        scheduler: fsrs.Scheduler,
        quantize: bool = False,
) -> dict[Indentifier, fsrs.Card]:
    fsrs_cards = {}
    valid_identifiers = set(identifier for identifier, _original_due in targets)

    for identifier, original_due in targets:
        fsrs_cards[identifier] = fsrs.Card()
        fsrs_cards[identifier].due = original_due

    for review in reviews:
        if review.identifier in valid_identifiers:
            next_card, _revlog = scheduler.review_card(
                card=fsrs_cards[review.identifier],
                rating=RATINGS[review.rating],
                review_datetime=review.datetime.astimezone(tz=timezone.utc),
            )

            # Optionally account for the loss in precision when org-fc tracks these values
            if quantize:
                next_card.stability = float(f"{next_card.stability:.6f}") if next_card.stability is not None else None
                next_card.difficulty = float(f"{next_card.difficulty:.6f}") if next_card.difficulty is not None else None

            fsrs_cards[review.identifier] = next_card

    return fsrs_cards

def from_history():
    parser = argparse.ArgumentParser(description="Recompute FSRS card from history")
    parser.add_argument("--history_file", type=str, help="TSV review history file", required=True)
    parser.add_argument("--quantize", action="store_true", help="Quantize stability & difficulty to 6 decimal points")

    args, _ = parser.parse_known_args(sys.argv[2:])

    reader = TSVReader(args.history_file)

    json_str = sys.stdin.read()
    try:
        request = json.loads(json_str)
    except json.JSONDecodeError as e:
        raise ValueError(f"Failed to parse JSON: {e}")

    scheduler_dict = request["scheduler"]
    scheduler_dict = keys_from_emacs(scheduler_dict)
    scheduler = fsrs.Scheduler.from_dict(scheduler_dict)

    targets = request.get("targets")
    if targets is None:
        raise ValueError("Missing 'targets' in request.")

    # Flatten to FSRS-card identifiers
    flat_targets = []
    for card_id, positions in targets.items():
        for pos in positions:
            ident = Indentifier(card_id=card_id, name=pos["name"])
            flat_targets.append((ident, datetime.fromisoformat(pos["original-due"])))

    flat_cards = replay_reviews(flat_targets, reader.read_reviews(), scheduler, quantize=args.quantize)

    # Convert to result dictionary {card_id: review_data}
    result = {}
    for card_id, positions in targets.items():
        result[card_id] = []
        for pos in positions:
            ident = Indentifier(card_id=card_id, name=pos["name"])
            review_data_row = card_to_dict(flat_cards[ident], position=pos["name"])
            result[card_id].append(review_data_row)

    print(json.dumps(result, indent=2))

if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise ValueError("Not enough arguments")

    command = sys.argv[1]
    if not command in ("initial", "review", "from_history"):
        raise ValueError(f"Unknown command: {command}")

    if command == "initial":
        initial()
    elif command == "review":
        review()
    elif command == "from_history":
        from_history()
