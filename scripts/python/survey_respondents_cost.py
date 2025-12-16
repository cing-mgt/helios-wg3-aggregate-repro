"""
Figure 1 (revised): Respondents by country and COST ITC status
- Horizontal stacked bar chart
- X-axis: number of respondents
- Y-axis: country
- Colors: Turquoise = ITC, Magenta = non-ITC
- Saves PDF (vector) + TIFF (300 dpi)

Requirements:
  python3 -m pip install pandas matplotlib pillow openpyxl
"""

from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
from PIL import Image

# -----------------------------
# INPUT / OUTPUT
# -----------------------------
INPUT_FILE = Path("../../data/WG3_survey_responses.xlsx")
SHEET_NAME = 0

COL_COUNTRY = "Country of residence"
COL_STATUS  = "ITC_status"   # ITC / non-ITC

OUTDIR = Path("../../figures")
OUTDIR.mkdir(parents=True, exist_ok=True)

pdf_path = OUTDIR / "Manuscript_Figure1_bars.pdf"
png_path = OUTDIR / "Manuscript_Figure1_bars.png"
tif_path = OUTDIR / "Manuscript_Figure1_bars.tiff"

# -----------------------------
# READ DATA
# -----------------------------
df = pd.read_excel(INPUT_FILE, sheet_name=SHEET_NAME)
df.columns = df.columns.map(str.strip)

df[COL_COUNTRY] = df[COL_COUNTRY].astype(str).str.strip()
df[COL_STATUS]  = df[COL_STATUS].astype(str).str.strip()

# Normalise labels if needed
df[COL_STATUS] = df[COL_STATUS].replace({
    "Non-ITC": "non-ITC",
    "non ITC": "non-ITC",
})

# -----------------------------
# AGGREGATE COUNTS
# -----------------------------
counts = (
    df.groupby([COL_COUNTRY, COL_STATUS])
      .size()
      .unstack(fill_value=0)
)

# Ensure both columns exist
for col in ["ITC", "non-ITC"]:
    if col not in counts.columns:
        counts[col] = 0

# Sort by total respondents
# Sort countries alphabetically
counts = counts.sort_index()


# -----------------------------
# PLOT
# -----------------------------
fig_h = max(4.5, 0.35 * len(counts))
fig, ax = plt.subplots(figsize=(7.5, fig_h))

# COST colours
COLOR_ITC     = "#40E0D0"  # turquoise
COLOR_NON_ITC = "#C000C0"  # magenta

ax.barh(
    counts.index,
    counts["ITC"],
    color=COLOR_ITC,
    edgecolor="white",
    label="ITC"
)

ax.barh(
    counts.index,
    counts["non-ITC"],
    left=counts["ITC"],
    color=COLOR_NON_ITC,
    edgecolor="white",
    label="non-ITC"
)

# Labels inside bars
for i, country in enumerate(counts.index):
    itc = counts.loc[country, "ITC"]
    non = counts.loc[country, "non-ITC"]

    if itc > 0:
        ax.text(itc / 2, i, str(itc),
                va="center", ha="center", fontsize=10, fontweight="bold")

    if non > 0:
        ax.text(itc + non / 2, i, str(non),
                va="center", ha="center", fontsize=10, fontweight="bold")

# Axis formatting
ax.set_xlabel("Number of respondents", fontsize=12)
ax.set_ylabel("")
ax.tick_params(axis="y", labelsize=10)

ax.grid(axis="x", linestyle="--", alpha=0.35)
ax.set_axisbelow(True)
ax.invert_yaxis()

# Legend
ax.legend(
    title="COST status",
    frameon=False,
    loc="center left",
    bbox_to_anchor=(1.02, 0.5),
    title_fontsize=11,
    fontsize=11
)

plt.tight_layout(rect=[0, 0, 0.85, 1])

# -----------------------------
# EXPORT
# -----------------------------
fig.savefig(pdf_path)
fig.savefig(png_path, dpi=300, bbox_inches="tight")

with Image.open(png_path) as im:
    im.convert("RGB").save(
        tif_path, dpi=(300, 300), compression="tiff_deflate"
    )

print("Saved:", pdf_path)
print("Saved:", tif_path)