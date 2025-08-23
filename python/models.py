from dataclasses import dataclass
from datetime import datetime

@dataclass(frozen=True)
class Indentifier:
    """Unique identifier for a review position of some card."""
    card_id: str
    name: str

@dataclass
class Review:
    """Review history entry"""
    identifier: Indentifier
    rating: str
    datetime: datetime
    # Old versions of org-fc did not track this
    duration: float | None
