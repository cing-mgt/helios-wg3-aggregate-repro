# HELIOS WG3 - Survey analysis code (Figure 3: data types availability)
# Author: Francesco Cremonesi
# Repro tips:
# - Tested with: Python 3.11, pandas>=2.2, numpy>=1.26, matplotlib>=3.8
# - Input: de-identified survey responses in Excel (first sheet)
# - Output: PNG and TIFF for publication

from __future__ import annotations
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# ------------------ Config ------------------
from pathlib import Path

HERE = Path(__file__).resolve()
ROOT = HERE.parents[2]  # .../helios-wg3-aggregate-repro
DATA_PATH = ROOT / "data" / "WG3_survey_responses.xlsx"

# check if filepath exists
if not DATA_PATH.exists():
    raise FileNotFoundError(f"Input not found: {DATA_PATH}\n"
                            f"Tip: ensure the Excel is in /data at project root.")

FIGDIR = ROOT / "figures"
FIGDIR.mkdir(parents=True, exist_ok=True)

# --- Tol Light palette ---
TOL_LIGHT = {
    "blue":  "#4477AA",
    "red":   "#EE6677",
    "cyan":  "#66CCEE",
    "green": "#228833",
    "yellow":"#CCBB44",
    "purple":"#AA3377",
    "grey":  "#BBBBBB"
}

# --- Initializations ---
SKIP_OTHER = True
ZERO_LONG_FOR = {
    "genotype",
    "genetic modifiers",
    "whole genome sequencing (wgs)",
    "whole exome sequencing (wes)",
    "targeted ngs/gene panels",
    "gwas",
}

def norm(s) -> str:
    return str(s).strip().lower()

def main():
    # 1) Load
    df = pd.read_excel(DATA_PATH, sheet_name=0)

    # 2) Identify columns by names"
    avail, longi = {}, {}
    for c in df.columns:
        cs = str(c).strip()
        if cs.startswith("Do you have available data of this type for any hemoglobinopathy?"):
            label = cs.split("?")[-1].strip().lstrip("-").strip()
            if SKIP_OTHER and norm(label).startswith("other data type (please specify"):
                continue
            avail[norm(label)] = (label, c)

        elif cs.startswith("Do you have multiple time points"):
            label = cs.split("-")[-1].strip()
            if SKIP_OTHER and norm(label).startswith("other data type (please specify"):
                continue
            longi[norm(label)] = (label, c)

    # 3) Build tidy table + enforce domain rule
    rows = []
    for key, (pretty, acol) in avail.items():
        s = df[acol].astype(str).str.strip().str.lower()
        y, n = (s == "yes").sum(), (s == "no").sum()
        u = len(s) - y - n

        l_yes = 0
        if key in longi:
            ls = df[longi[key][1]].astype(str).str.strip().str.lower()
            l_yes = (ls == "yes").sum()

        if key in ZERO_LONG_FOR:
            l_yes = 0

        rows.append({
            "data_type": pretty,
            "yes": y, "no": n, "unknown": u,
            "yes_longitudinal": l_yes,
            "yes_cross_sectional": max(0, y - l_yes)
        })

    counts = pd.DataFrame(rows).sort_values("yes", ascending=True)

    # 4) Plot
    plt.rcParams.update({'font.size': 12})
    fig, ax = plt.subplots(figsize=(10, 8))

    colors = {
        'unknown': '#DDDDDD',
        'no': '#EE8866',
        'yes_cs': '#44BB99',
        'yes_long': '#BBCC33'
    }

    y_pos = np.arange(len(counts))
    ax.barh(y_pos, counts["unknown"], color=colors['unknown'], label='Unknown')
    ax.barh(y_pos, counts["no"], left=counts["unknown"], color=colors['no'], label='No')
    ax.barh(y_pos, counts["yes_cross_sectional"],
            left=counts["unknown"] + counts["no"], color=colors['yes_cs'], label='Yes: cross-sectional')
    ax.barh(y_pos, counts["yes_longitudinal"],
            left=counts["unknown"] + counts["no"] + counts["yes_cross_sectional"],
            color=colors['yes_long'], label='Yes: longitudinal')

    # Labels on segments (only if >0)
    for i, row in enumerate(counts.itertuples()):
        u, n, ycs, ylong = row.unknown, row.no, row.yes_cross_sectional, row.yes_longitudinal
        left1 = u
        left2 = left1 + n
        left3 = left2 + ycs
        if u:     ax.text(u/2, i, str(u), ha='center', va='center', color='black')
        if n:     ax.text(left1 + n/2, i, str(n), ha='center', va='center', color='black')
        if ycs:   ax.text(left2 + ycs/2, i, str(ycs), ha='center', va='center', color='black')
        if ylong: ax.text(left3 + ylong/2, i, str(ylong), ha='center', va='center', color='black')

    # Y labels (tidy a few names)
    def fmt(label: str) -> str:
        l = str(label).strip()
        low = l.lower()
        fixes = {
            "ngs": "NGS", "wgs": "WGS", "wes": "WES", "gwas": "GWAS",
            "procedures and treatments": "Procedures and Treatments",
            "procedures and treatment": "Procedures and Treatments",
        }
        return fixes.get(low, l[:1].upper() + l[1:] if l.islower() else l)

    ax.set_yticks(y_pos)
    ax.set_yticklabels([fmt(dt) for dt in counts["data_type"]], fontweight='bold')
    ax.set_xticks([])
    ax.set_title('Do you have data of this type?', fontweight='bold')
    ax.xaxis.grid(True, linestyle='--', alpha=0.3)
    for sp in ['top', 'right', 'bottom', 'left']:
        ax.spines[sp].set_visible(False)
    ax.legend(loc='center left', bbox_to_anchor=(1.02, 0.5), frameon=True,
              framealpha=1.0, edgecolor='black', labelspacing=1.2)
    plt.tight_layout()

    # 5) Save
    fig.savefig(FIGDIR / "Manuscript_Figure3.png", dpi=300)
    fig.savefig(FIGDIR / "Manuscript_Figure3.tiff", dpi=300, bbox_inches="tight", format="tiff")


if __name__ == "__main__":
    main()

