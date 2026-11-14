#!/usr/bin/env python3
"""Static, data-free release checks for the public BrainNormR source tree."""

from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "STATIC_RELEASE_AUDIT.txt"


def balanced_r_delimiters(path: Path) -> tuple[bool, str]:
    text = path.read_text(encoding="utf-8")
    stack: list[tuple[str, int]] = []
    pairs = {")": "(", "}": "{", "]": "["}
    state = "code"
    quote = ""
    line = 1
    index = 0
    while index < len(text):
        char = text[index]
        if char == "\n":
            line += 1
        if state == "comment":
            if char == "\n":
                state = "code"
        elif state == "string":
            if char == "\\":
                index += 1
            elif char == quote:
                state = "code"
                quote = ""
        else:
            if char == "#":
                state = "comment"
            # R backtick-quoted names (for example, `[[`) may contain
            # delimiter characters that are not part of the expression tree.
            elif char in {'"', "'", "`"}:
                state = "string"
                quote = char
            elif char in "({[":
                stack.append((char, line))
            elif char in (")", "}", "]"):
                if not stack or stack[-1][0] != pairs[char]:
                    return False, f"unmatched {char} at line {line}"
                stack.pop()
        index += 1
    if state == "string":
        return False, "unterminated string"
    if stack:
        return False, f"unclosed delimiter {stack[-1][0]} from line {stack[-1][1]}"
    return True, "balanced delimiters"


def contains_persian(text: str) -> bool:
    return any("\u0600" <= char <= "\u06ff" for char in text)


checks: list[tuple[str, bool, str]] = []


def add(label: str, passed: bool, detail: str = "") -> None:
    checks.append((label, bool(passed), detail))


required = [
    "BrainNormR_v5_FINAL_PUBLIC.R",
    "BrainNormR_v5_FINAL_PUBLIC_ALL_ONLY.R",
    "BrainNormR_v5_FINAL_PUBLIC_THREE_GROUP.R",
    "03_biomarker_module.R",
    "09_make_publication_figure_suite.R",
    "10_final_pipeline_audit.R",
    "15_run_normative_deviation_profiles.R",
    "Config/BrainNormR_mode_config.R",
    "Config/Deviation_profile_config.R",
    "docs/DEVIATION_PROFILE_ANALYSIS.md",
    "tests/test_deviation_profile_helpers.R",
    "README.md",
    "RUN_INSTRUCTIONS.md",
    "VERSION",
]
for relative in required:
    add(f"Required file: {relative}", (ROOT / relative).is_file())

version = (ROOT / "VERSION").read_text(encoding="utf-8").strip()
add("Release version is v5.6.0", version.startswith("v5.6.0"), version)

profile_text = (ROOT / "15_run_normative_deviation_profiles.R").read_text(
    encoding="utf-8"
)
profile_tokens = [
    "Balanced_LevelMeasure_Burden",
    "run_independent_outcome_null",
    "run_block_preserving_null",
    "run_profile_stability",
    "Threshold_TopDecile",
    "Demographic_Audit",
    "Directional_Audit",
]
for token in profile_tokens:
    add(f"Deviation-profile implementation: {token}", token in profile_text)

biomarker_text = (ROOT / "03_biomarker_module.R").read_text(encoding="utf-8")
add(
    "Primary biomarker burden excludes cognition",
    "INCLUDE_COGNITION_IN_OVERALL_BURDEN <- FALSE" in biomarker_text,
)
add(
    "Cognition-inclusive burden is retained as sensitivity",
    "IndividualBurden_AllPlusCog" in biomarker_text,
)

for path in sorted(ROOT.rglob("*.R")):
    passed, detail = balanced_r_delimiters(path)
    add(f"R delimiter scan: {path.relative_to(ROOT)}", passed, detail)

text_files = [
    path
    for path in ROOT.rglob("*")
    if path.is_file()
    and path.name != ".Rhistory"
    and path.suffix.lower() in {".r", ".md", ".txt", ".csv", ".yml", ".yaml"}
]
persian_files = []
for path in text_files:
    try:
        text = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        continue
    if contains_persian(text):
        persian_files.append(str(path.relative_to(ROOT)))
add(
    "Public source and documentation are English-only",
    not persian_files,
    ", ".join(persian_files),
)

personal_path_pattern = re.compile(
    r"(?:[A-Za-z]:[/\\](?:Users|Documents and Settings)[/\\]|/Users/|/home/[^/\s]+/)"
)
personal_path_files = []
for path in text_files:
    try:
        text = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        continue
    if personal_path_pattern.search(text):
        personal_path_files.append(str(path.relative_to(ROOT)))
add(
    "No user-specific absolute paths in public files",
    not personal_path_files,
    ", ".join(personal_path_files),
)

private_data = [
    path
    for path in (ROOT / "Data").rglob("*")
    if path.is_file()
    if path.name not in {".gitkeep", "README.md"}
]
add("No participant-level input data included", not private_data)

generated_results = [
    path
    for path in (ROOT / "Results").rglob("*")
    if path.is_file()
    if path.name not in {".gitkeep", "README.md"}
]
add("No generated analysis results included", not generated_results)

transient_artifacts = [
    path
    for path in ROOT.rglob("*")
    if path.is_file()
    and (path.name == ".Rhistory" or path.suffix.lower() in {".pyc", ".tmp"})
]
add(
    "No transient history, cache or temporary files",
    not transient_artifacts,
    ", ".join(str(path.relative_to(ROOT)) for path in transient_artifacts),
)

failed = [item for item in checks if not item[1]]
lines = [
    "BrainNormR v5.6.0 — STATIC RELEASE AUDIT",
    "=" * 72,
    "Static audit only. Run the included R smoke test in the target R environment.",
    "",
]
for label, passed, detail in checks:
    status = "PASS" if passed else "FAIL"
    suffix = f" — {detail}" if detail else ""
    lines.append(f"[{status}] {label}{suffix}")
lines.extend(
    [
        "",
        f"Checks run: {len(checks)}",
        f"Checks failed: {len(failed)}",
    ]
)
OUTPUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
print("\n".join(lines[-3:]))
sys.exit(1 if failed else 0)
