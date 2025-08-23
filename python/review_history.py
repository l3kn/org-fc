from datetime import datetime
from typing import Iterator
import csv
import logging

from models import Indentifier, Review


class TSVReader:
    def __init__(self, file_path):
        self.file_path = file_path

    def read_reviews(self) -> Iterator[Review]:
        reviews = []
        with open(self.file_path, "r") as file:
            reader = csv.reader(file, delimiter="\t")
            for row in reader:
                assert len(row) == 10, "Invalid review history entry"
                datetime_str, _path, card_id, pos_name, _ease, _box, _interval, rating, duration, _algo = row

                try:
                    dt = datetime.fromisoformat(datetime_str)
                except ValueError:
                    logging.error(f"Invalid datetime format: {datetime_str}, skipping entry.")
                    continue

                if duration == "":
                    duration = None
                else:
                    try:
                        duration = float(duration)
                    except ValueError:
                        logging.error(f"Invalid duration format: {duration}, skipping entry.")
                        continue

                position = Indentifier(card_id=card_id, name=pos_name)
                yield Review(identifier=position, rating=rating, datetime=dt, duration=duration)
