"""Loading and light prep for the lake CVHT modeling dataset."""

from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_PATH = REPO_ROOT / "Final_Analysis_Ready_Data.csv"

LAKE_NAMES = {
    "AL": "Allequash",
    "BM": "Big Muskellunge",
    "CR": "Crystal",
    "SP": "Sparkling",
    "TR": "Trout",
}

LAKE_COLORS = {
    "AL": "#03045E",
    "BM": "#0077B6",
    "CR": "#00B4D8",
    "SP": "#48CAE4",
    "TR": "#90E0EF",
}

OUTCOME_COL = "CVHT_standardized"
YEAR_COL = "Year"
LAKE_COL = "lakeid"
DROP_COLS = {OUTCOME_COL, YEAR_COL, LAKE_COL, "n_months_sampled"}


def load_master_table(path: Path = DATA_PATH) -> pd.DataFrame:
    """Load the master lake-year predictor/outcome table used across all models."""
    df = pd.read_csv(path)
    df["lake_name"] = df[LAKE_COL].map(LAKE_NAMES)
    return df


def predictor_columns(df: pd.DataFrame) -> list[str]:
    return [c for c in df.columns if c not in DROP_COLS and c != "lake_name"]
