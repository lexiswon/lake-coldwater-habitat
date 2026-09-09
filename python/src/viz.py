"""Shared chart styling: fixed categorical colors, diverging good/bad-year colors.

Palette follows a validated categorical order (fixed hue slots, never cycled)
so a lake or model always gets the same color across every figure in the repo.
"""

import matplotlib.pyplot as plt
import seaborn as sns

SURFACE = "#fcfcfb"
TEXT_PRIMARY = "#0b0b0b"
TEXT_SECONDARY = "#52514e"
GRID = "#e3e2dd"

# Fixed categorical order, slots 1-5 of the validated 8-hue theme.
LAKE_COLORS = {
    "AL": "#2a78d6",  # blue
    "BM": "#eb6834",  # orange
    "CR": "#1baf7a",  # aqua
    "SP": "#eda100",  # yellow
    "TR": "#e87ba4",  # magenta
}

# Slots 6-8, kept visually distinct from the lake palette above.
MODEL_COLORS = {
    "PCR": "#008300",  # green
    "Random Forest": "#4a3aa7",  # violet
    "XGBoost": "#e34948",  # red
}

# Diverging: bad <-> good year, neutral gray midpoint.
YEAR_TYPE_COLORS = {
    "Bad year": "#e34948",
    "Normal year": "#c9c8c2",
    "Good year": "#2a78d6",
}
CATEGORY_COLORS = {
    "Below Normal": YEAR_TYPE_COLORS["Bad year"],
    "Near Normal": YEAR_TYPE_COLORS["Normal year"],
    "Above Normal": YEAR_TYPE_COLORS["Good year"],
}

SEQUENTIAL_BLUE = ["#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"]


def set_style() -> None:
    sns.set_theme(style="whitegrid")
    plt.rcParams.update(
        {
            "figure.facecolor": SURFACE,
            "axes.facecolor": SURFACE,
            "savefig.facecolor": SURFACE,
            "axes.edgecolor": GRID,
            "axes.labelcolor": TEXT_PRIMARY,
            "text.color": TEXT_PRIMARY,
            "xtick.color": TEXT_SECONDARY,
            "ytick.color": TEXT_SECONDARY,
            "grid.color": GRID,
            "axes.titlecolor": TEXT_PRIMARY,
            "font.size": 11,
        }
    )
