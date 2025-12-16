"""
Figure 5 (revised): Two-panel stacked bars (Centralized vs Federated)

Within each panel:
  - Bar 1 (Willingness): Yes/No among eligible respondents
      Eligibility = multi-site collaboration willingness == "Yes"
  - Bar 2 (Permission): permission status among those who answered "Yes" to the corresponding model
      (i.e., conditional/branching logic; blanks are structural and excluded)

Outputs:
  - PNG (300 dpi)
  - TIFF (300 dpi, deflate)

Requirements:
  /usr/bin/python3 -m pip install pandas numpy matplotlib pillow openpyxl
"""

from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from PIL import Image
from matplotlib.lines import Line2D

# -----------------------------
# 1) INPUT / OUTPUT PATHS
# -----------------------------
INPUT_FILE = Path("../../data/WG3_survey_responses.xlsx")
SHEET_NAME = 0  # or "Sheet1"

FIGDIR = Path("../../figures")
FIGDIR.mkdir(parents=True, exist_ok=True)

png_path = FIGDIR / "Manuscript_Figure5_bar.png"
tif_path = FIGDIR / "Manuscript_Figure5_bar.tiff"

# -----------------------------
# 2) COLUMN NAMES (MUST match Excel export exactly)
# -----------------------------
COL_MULTI_SITE = "Would you be open to participating in a multi-site research collaboration?"

COL_CENT_WILL = (
    "Would you be willing to share your data in a centralized database outside of your institution for research purposes?"
)
COL_CENT_PERM = (
    "If yes, do you have permission (e.g. informed consent, ethics approval) to share and reuse this data?"
)

COL_FED_WILL = (
    "Would you be willing to participate in a federated study, where data is not shared outside of your institution, "
    "but only aggregate statistics are extracted from your data and shared externally?"
)
COL_FED_PERM = (
    "If yes, do you have permission (e.g. informed consent, ethics approval) to reuse this data?"
)

# -----------------------------
# 3) HELPERS
# -----------------------------
def norm_str(x) -> str:
    if pd.isna(x):
        return ""
    return str(x).strip()

def norm_yesno(x) -> str:
    s = norm_str(x).lower()
    if s == "yes":
        return "Yes"
    if s == "no":
        return "No"
    return ""  # blank/other

def norm_permission(x) -> str:
    s = norm_str(x)
    s_low = s.lower()

    if s_low == "yes":
        return "Yes"
    if "no but i could obtain it" in s_low or "no, but i could obtain it" in s_low:
        return "No, but could obtain"
    if s_low == "no":
        return "No, could not obtain it"

    # For branching logic: blanks/NA mean "question not shown" (structural missingness)
    return ""

def stacked_bar(ax, x0, segments, colors, total=None, label_inside=True):
    """
    Draw one stacked bar at x0 given list of (label, value).
    If total is provided, percentages are computed as val/total.
    """
    bottom = 0
    if total is None:
        total = sum(v for _, v in segments) if segments else 0
    total_safe = total if total > 0 else 1

    for lab, val in segments:
        if val <= 0:
            continue

        ax.bar(
            x0, val, bottom=bottom, width=0.6,
            color=colors[lab], edgecolor="white", linewidth=0.8
        )

        if label_inside:
            pct = 100 * val / total_safe
            ax.text(
                x0, bottom + val/2,
                f"{int(val)}\n({pct:.0f}%)",
                ha="center", va="center",
                fontsize=10, linespacing=1.5
            )

        bottom += val

    return bottom

# -----------------------------
# 4) READ + NORMALIZE
# -----------------------------
df = pd.read_excel(INPUT_FILE, sheet_name=SHEET_NAME)
df.columns = df.columns.map(lambda c: str(c).strip())

required = [COL_MULTI_SITE, COL_CENT_WILL, COL_CENT_PERM, COL_FED_WILL, COL_FED_PERM]
missing = [c for c in required if c not in df.columns]
if missing:
    raise ValueError(
        "Missing expected columns:\n- " + "\n- ".join(missing) +
        "\n\nTip: print(df.columns.tolist()) and update COL_* strings to match exactly."
    )

# Normalize core answers
df["_multi_site"] = df[COL_MULTI_SITE].map(norm_yesno)
df["_cent_will"]  = df[COL_CENT_WILL].map(norm_yesno)
df["_cent_perm"]  = df[COL_CENT_PERM].map(norm_permission)
df["_fed_will"]   = df[COL_FED_WILL].map(norm_yesno)
df["_fed_perm"]   = df[COL_FED_PERM].map(norm_permission)

# Eligible = multi-site collaboration willingness "Yes"
eligible = df[df["_multi_site"] == "Yes"].copy()

# --- Willingness counts among eligible ---
cent_will_counts = eligible["_cent_will"].value_counts(dropna=False).to_dict()
fed_will_counts  = eligible["_fed_will"].value_counts(dropna=False).to_dict()

cent_yes_n = int(cent_will_counts.get("Yes", 0))
cent_no_n  = int(cent_will_counts.get("No", 0))

fed_yes_n = int(fed_will_counts.get("Yes", 0))
fed_no_n  = int(fed_will_counts.get("No", 0))

# --- Permission counts among those willing "Yes" (branching: exclude blanks) ---
cent_yes = eligible[eligible["_cent_will"] == "Yes"].copy()
fed_yes  = eligible[eligible["_fed_will"] == "Yes"].copy()

perm_order = ["Yes", "No, but could obtain", "No, could not obtain"]

cent_perm_counts = (
    cent_yes["_cent_perm"]
    .replace("", np.nan)
    .dropna()
    .value_counts()
    .to_dict()
)
fed_perm_counts = (
    fed_yes["_fed_perm"]
    .replace("", np.nan)
    .dropna()
    .value_counts()
    .to_dict()
)

cent_perm_vals = [int(cent_perm_counts.get(k, 0)) for k in perm_order]
fed_perm_vals  = [int(fed_perm_counts.get(k, 0))  for k in perm_order]

# -----------------------------
# 5) PLOT: two panels, two bars each
# -----------------------------
colors_will = {
    "Yes": "#4477AA",  # blue
    "No":  "#EE6677",  # red
}
colors_perm = {
    "Yes": "#228833",                 # green
    "No, but could obtain": "#CCBB44",# yellow
    "No, could not obtain": "#AA3377",                  # purple
}

fig, axes = plt.subplots(1, 2, figsize=(12.5, 6.2), sharey=True)

panels = [
    ("Centralized data-sharing model", axes[0],
     [("Yes", cent_yes_n), ("No", cent_no_n)],
     list(zip(perm_order, cent_perm_vals))),
    ("Federated analysis model", axes[1],
     [("Yes", fed_yes_n), ("No", fed_no_n)],
     list(zip(perm_order, fed_perm_vals))),
]

for title, ax, will_segments, perm_segments in panels:
    ax.set_title(title, fontsize=13, fontweight="bold")

    xpos = [0, 1]
    ax.set_xticks(xpos)
    ax.set_xticklabels(
        ["Willingness to\nparticipate in collaboration", "Permission to\nshare and reuse data"],
        fontsize=11
    )

    # Denominators
    will_total_cent = cent_yes_n + cent_no_n
    will_total_fed = fed_yes_n + fed_no_n
    perm_total_cent = sum(cent_perm_vals)
    perm_total_fed = sum(fed_perm_vals)

    # In your loop (inside for title, ax, will_segments, perm_segments in panels:)
    if title.startswith("Centralized"):
        stacked_bar(ax, xpos[0], will_segments, colors_will, total=will_total_cent, label_inside=True)
        stacked_bar(ax, xpos[1], perm_segments, colors_perm, total=perm_total_cent, label_inside=True)
    else:
        stacked_bar(ax, xpos[0], will_segments, colors_will, total=will_total_fed, label_inside=True)
        stacked_bar(ax, xpos[1], perm_segments, colors_perm, total=perm_total_fed, label_inside=True)

    ax.grid(axis="y", linestyle="--", alpha=0.35)
    ax.set_axisbelow(True)

axes[0].set_ylabel("Number of respondents", fontsize=12)

# -----------------------------
# 6) LEGEND (below panels, with section headers on separate rows)
# -----------------------------
legend_items = [
    # Row 1: Willingness header (force full row)
    Line2D([0], [0], color="none", label="Willingness to\nparticipate in collaboration"),
    Line2D([0], [0], color="none", label=""),
    Line2D([0], [0], color="none", label=""),
    Line2D([0], [0], color="none", label=""),

    # Row 2: Willingness categories
    Line2D([0], [0], color=colors_will["Yes"], lw=8, label="Yes"),
    Line2D([0], [0], color=colors_will["No"],  lw=8, label="No"),
    Line2D([0], [0], color="none", label=""),
    Line2D([0], [0], color="none", label=""),

    # Row 3: Permission header (force full row)
    Line2D([0], [0], color="none", label="Permission to\nshare and reuse data"),
    Line2D([0], [0], color="none", label=""),
    Line2D([0], [0], color="none", label=""),
    Line2D([0], [0], color="none", label=""),

    # Row 4: Permission categories
    Line2D([0], [0], color=colors_perm["Yes"], lw=8, label="Yes"),
    Line2D([0], [0], color=colors_perm["No, but could obtain"], lw=8, label="No, but could obtain"),
    Line2D([0], [0], color=colors_perm["No, could not obtain"], lw=8, label="No, could not obtain"),
    Line2D([0], [0], color="none", label=""),
]

leg = fig.legend(
    handles=legend_items,
    loc="lower center",
    ncol=4,
    frameon=False,
    bbox_to_anchor=(0.5, -0.12),
    fontsize=11,
)

for text in leg.get_texts():
    if text.get_text() in [
        "Willingness to\nparticipate in collaboration",
        "Permission to\nshare and reuse data"
    ]:
        text.set_fontweight("bold")
        text.set_ha("center")

plt.tight_layout(rect=[0, 0.12, 1, 1])

# -----------------------------
# 7) SAVE: PNG + 300 dpi TIFF
# -----------------------------
fig.savefig(png_path, dpi=300, bbox_inches="tight")

with Image.open(png_path) as im:
    im = im.convert("RGB")
    im.save(tif_path, dpi=(300, 300), compression="tiff_deflate")

print("Saved:", png_path)
print("Saved:", tif_path)
