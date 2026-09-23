#!/usr/bin/env python3
"""Run project setup after Git creates a new worktree."""

import json
from pathlib import Path
import subprocess
import sys


def main():
    if len(sys.argv) != 5:
        sys.exit("Usage: setup-worktree.py CONFIG PREVIOUS_HEAD NEW_HEAD CHECKOUT_FLAG")
    config_path, previous_head, _new_head, checkout_flag = sys.argv[1:]

    # Git supplies a null previous HEAD and flag 1 for initial checkouts.
    if not previous_head or set(previous_head) != {"0"} or checkout_flag != "1":
        return

    # The main worktree is listed first. NUL separators preserve unusual paths.
    listing = subprocess.check_output(
        ["git", "worktree", "list", "--porcelain", "-z"], text=True
    )
    first_worktree = listing.split("\0\0", 1)[0].split("\0")
    main_worktree = Path(first_worktree[0].removeprefix("worktree ")).resolve()

    # Git invokes the hook at the new worktree's root.
    worktree = Path.cwd()

    # Bare repositories have no main checkout to copy files from.
    # Also skip the main checkout itself, e.g. the initial checkout after cloning.
    if "bare" in first_worktree or main_worktree == worktree:
        return

    # Parse the configuration.
    config = json.loads(Path(config_path).read_text(encoding="utf-8"))
    if not isinstance(config, dict):
        raise ValueError("Config must be a JSON object")
    commands = config.get("commands", [])
    files = config.get("files", [])
    for name, entries in (("commands", commands), ("files", files)):
        if not isinstance(entries, list) or not all(isinstance(item, str) for item in entries):
            raise ValueError(f"{name} must be a list of strings")
    for filename in files:
        if not filename or Path(filename).is_absolute() or ".." in Path(filename).parts:
            raise ValueError(f"File must be a relative path inside the worktree: {filename!r}")

    # Run any necessary setup commands for the worktree.
    for command in commands:
        print(f"Running: {command}", flush=True)
        subprocess.run(["bash", "-c", command], check=True)

    # Copy any necessary untracked files from the the main worktree into the new one.
    for filename in files:
        print(f"Copying: {filename}", flush=True)
        with (main_worktree / filename).open(encoding="utf-8", newline="") as source:
            contents = source.read()
        # Ensure any hardcoded paths in the copied file point to the new worktree.
        contents = contents.replace(str(main_worktree), str(worktree))
        destination = worktree / filename
        destination.parent.mkdir(parents=True, exist_ok=True)
        # Replace the file itself so even an existing symlink becomes an independent copy.
        destination.unlink(missing_ok=True)
        destination.write_text(contents, encoding="utf-8", newline="")


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as error:
        sys.exit(error.returncode)
    except (OSError, ValueError) as error:
        sys.exit(f"Worktree setup: {error}")
