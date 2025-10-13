# HELIOS WG3 – Two-panel funnel plot (Figure 5: Centralized vs Federated)
# Author: Francesco Cremonesi
# Repro tips:
# - Tested with: Python 3.11, pandas>=2.2, numpy>=1.26, matplotlib>=3.8
# - Input: de-identified survey responses in Excel
# - Output: PNG and TIFF for publication

# --- Imports ---
from pathlib import Path
from plotly.subplots import make_subplots
import plotly.graph_objects as go
from PIL import Image

# --- Output dir ---
FIGDIR = Path("../../figures")
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
CENTRAL_COLOR = TOL_LIGHT["blue"]
FED_COLOR     = TOL_LIGHT["red"]

# --- Data ---
funnel_step_central = [
    "Total responses", "Valid responses",
    "Willing to<br>participate in<br>multi-site collaboration",
    "Willing to<br>share data<br>centrally",
    "Has sharing permission<br>or could obtain it"
]
funnel_number_central = [48, 44, 42, 30, 26]

funnel_step_fed = [
    "Total responses", "Valid responses",
    "Willing to<br>participate in<br>multi-site collaboration",
    "Willing to<br>participate in<br>federated study",
    "Has reuse permission<br>or could obtain it"
]
funnel_number_fed = [48, 44, 42, 38, 35]

# --- Helper: clamp widths so each step ≤ previous ---
def clamp_nonincreasing(seq):
    out, last = [], None
    for v in seq:
        v2 = v if last is None else min(v, last)
        out.append(v2); last = v2
    return out

x_central_display = clamp_nonincreasing(funnel_number_central)
x_fed_display     = clamp_nonincreasing(funnel_number_fed)

# --- Figure ---
fig = make_subplots(
    rows=1, cols=2,
    specs=[[{"type":"funnel"}, {"type":"funnel"}]],
    horizontal_spacing=0.20,
    subplot_titles=('Centralized data study', 'Federated study'),
)

for ann in fig['layout']['annotations']:
    ann['font'] = dict(size=20)

# Centralized
fig.add_trace(
    go.Funnel(
        y=funnel_step_central,
        x=x_central_display,
        text=funnel_number_central,
        texttemplate="%{text}",
        textposition="inside",
        insidetextfont=dict(size=26),
        marker=dict(color=CENTRAL_COLOR, line=dict(color="white", width=0.8)),
        opacity=0.95,
    ),
    row=1, col=1
)

# Federated
fig.add_trace(
    go.Funnel(
        y=funnel_step_fed,
        x=x_fed_display,
        text=funnel_number_fed,
        texttemplate="%{text}",
        textposition="inside",
        insidetextfont=dict(size=26),
        marker=dict(color=FED_COLOR, line=dict(color="white", width=0.8)),
        opacity=0.95,
    ),
    row=1, col=2
)

fig.update_layout(
    height=520, width=1600, showlegend=False,
    font=dict(family="Arial", size=22, color="#000000"),
    paper_bgcolor="white", plot_bgcolor="white",
    margin=dict(t=70, b=50, l=90, r=90),
    yaxis =dict(categoryorder="array", categoryarray=funnel_step_central),
    yaxis2=dict(categoryorder="array", categoryarray=funnel_step_fed),
    uniformtext=dict(minsize=12, mode="show"),
)

# --- Save ---
png_path = FIGDIR / "Manuscript_Figure5.png"
tif_path = FIGDIR / "Manuscript_Figure5.tiff"

fig.write_image(str(png_path), scale=3)  # needs kaleido

with Image.open(png_path) as im:
    im = im.convert("RGB")
    im.save(tif_path, dpi=(300, 300), compression="tiff_deflate")

print(f"Saved {png_path} and {tif_path} (300 dpi)")
