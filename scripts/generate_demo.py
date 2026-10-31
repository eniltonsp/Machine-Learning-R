"""Generate fictional demonstration records. Never use these for model validation."""
import csv
import random
from datetime import date, timedelta
from pathlib import Path


def main():
    rng = random.Random(20261031)
    destination = Path(__file__).resolve().parents[1] / "data/demo/complaints.csv"
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow([
            "complaint_id", "company", "status", "date", "city", "state",
            "segment", "days_response", "days_resolution", "rating", "text",
        ])
        for number in range(600):
            status = rng.choice(["Resolved", "Unresolved", "Answered", "Unanswered", "In discussion"])
            writer.writerow([
                f"DEMO-{number + 1:04}",
                rng.choice(["Demo Retail Alpha", "Demo Retail Beta", "Demo Finance Gamma", "Demo Services Delta"]),
                status, (date(2025, 1, 1) + timedelta(days=rng.randrange(365))).isoformat(),
                "Fictional city", "Fictional state", "Demo", rng.randrange(31),
                rng.randrange(31, 91), rng.randrange(11),
                rng.choice([
                    "Delayed delivery and difficulty contacting customer support",
                    "Incorrect charge and refund request",
                    "Defective product and replacement request",
                    "Customer support resolved the service request",
                ]),
            ])


if __name__ == "__main__":
    main()
