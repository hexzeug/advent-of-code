from pathlib import Path
from datetime import datetime
import sys
import shutil
import os

YEAR = "2025"
BASE = Path(YEAR)
TEMPLATE = BASE / "template.rs"
CARGO = BASE / "Cargo.toml"

if not BASE.exists():
    raise SystemExit(f"{YEAR}/ folder not found")

if not TEMPLATE.exists():
    raise SystemExit(f"{TEMPLATE} not found - create your Rust template first")

# Get day
if len(sys.argv) > 1:
    day = int(sys.argv[1])
else:
    day = datetime.now().day

day_str = f"{day:02d}"
day_folder = BASE / day_str

part1 = day_folder / "part1.rs"
part2 = day_folder / "part2.rs"

# Create folder
day_folder.mkdir(exist_ok=True)

def copy_template(target: Path):
    if target.exists():
        print(f"Skipped existing {target}")
        return

    shutil.copy(TEMPLATE, target)
    print(f"Created {target}")

copy_template(part1)
copy_template(part2)

# Prepare Cargo.toml entries
bin_block = f"""
[[bin]]
name = "{day_str}_part1"
path = "{day_str}/part1.rs"

[[bin]]
name = "{day_str}_part2"
path = "{day_str}/part2.rs"
"""

cargo_text = CARGO.read_text()

if f'name = "{day_str}_part1"' in cargo_text:
    print("Cargo.toml already contains this day — skipping.")
else:
    CARGO.write_text(cargo_text.rstrip() + "\n" + bin_block)
    print("Updated Cargo.toml")

os.system(f"python3 fetch_input.py --day {day_str}")

print(f"\n✅ Day {day_str} is ready!")
print(f"Run with: cargo run --bin {day_str}_part1")
