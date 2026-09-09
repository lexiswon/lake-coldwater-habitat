"""Good-year / bad-year probabilistic forecasting layer.

Ports ``03-ProbabilisticModel (XG_RF).r``: fit a normal distribution to a
deterministic model's residuals, draw a 100-member ensemble per lake-year,
and classify each lake-year into tercile-based categories (Below Normal /
Near Normal / Above Normal — i.e. bad / normal / good coldwater-habitat
year). Also reproduces the Heidke Skill Score (HSS) and Ranked Probability
Skill Score (RPSS) used to judge those probabilistic forecasts against a
climatology baseline.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

RANDOM_STATE = 42
N_ENSEMBLE = 100
CATEGORIES = ["Below Normal", "Near Normal", "Above Normal"]
CATEGORY_LABELS = {"Below Normal": "Bad year", "Near Normal": "Normal year", "Above Normal": "Good year"}


def tercile_bins(observed: np.ndarray) -> tuple[float, float]:
    below_normal = np.quantile(observed, 0.33)
    above_normal = np.quantile(observed, 0.67)
    return below_normal, above_normal


def categorize(values: np.ndarray, below_normal: float, above_normal: float) -> np.ndarray:
    """Bucket values into CATEGORIES, returned as plain strings (not pd.Categorical)."""
    bins = [-np.inf, below_normal, above_normal, np.inf]
    return pd.cut(values, bins=bins, labels=CATEGORIES).astype(str).to_numpy()


def build_ensemble(observed: np.ndarray, predicted: np.ndarray, rng: np.random.Generator) -> pd.DataFrame:
    """100-member ensemble per year from predicted + N(mean_error, sd_error)."""
    errors = observed - predicted
    mu, sigma = errors.mean(), errors.std(ddof=1)
    members = rng.normal(mu, sigma, size=(len(predicted), N_ENSEMBLE))
    members = np.clip(members + predicted[:, None], 0, None)
    return pd.DataFrame(members, columns=[f"member_{i}" for i in range(N_ENSEMBLE)])


def forecast_table(years: np.ndarray, observed: np.ndarray, predicted: np.ndarray) -> pd.DataFrame:
    """Per-year category probabilities, predicted/observed category, for one lake+model."""
    rng = np.random.default_rng(RANDOM_STATE)
    below_normal, above_normal = tercile_bins(observed)
    ensemble = build_ensemble(observed, predicted, rng)

    rows = []
    for i, year in enumerate(years):
        member_cats = categorize(ensemble.iloc[i].to_numpy(), below_normal, above_normal)
        proportions = pd.Series(member_cats).value_counts(normalize=True).reindex(CATEGORIES, fill_value=0.0)
        predicted_cat = proportions.idxmax()
        observed_cat = categorize(np.array([observed[i]]), below_normal, above_normal)[0]
        rows.append(
            {
                "year": year,
                "observed_cvht": observed[i],
                "predicted_cvht": predicted[i],
                "prob_below_normal": proportions["Below Normal"],
                "prob_near_normal": proportions["Near Normal"],
                "prob_above_normal": proportions["Above Normal"],
                "predicted_category": predicted_cat,
                "observed_category": observed_cat,
                "predicted_year_type": CATEGORY_LABELS[predicted_cat],
                "observed_year_type": CATEGORY_LABELS[str(observed_cat)],
            }
        )
    return pd.DataFrame(rows)


def _rps(probabilities: np.ndarray, one_hot: np.ndarray) -> float:
    """Ranked probability score for one forecast, cumulative-sum form (matches the R port)."""
    rps = 0.0
    for i in range(2, len(probabilities) + 1):
        rps += (probabilities[: i - 1].sum() - one_hot[: i - 1].sum()) ** 2
    return rps


def skill_scores(table: pd.DataFrame) -> dict[str, float]:
    """Heidke Skill Score and Ranked Probability Skill Score vs. a 1/3-1/3-1/3 climatology."""
    n = len(table)
    ncat = len(CATEGORIES)

    hits = (table["predicted_category"] == table["observed_category"]).sum()
    expected = n / ncat
    hss = (hits - expected) / (n - expected)

    prob_cols = ["prob_below_normal", "prob_near_normal", "prob_above_normal"]
    rps_pred, rps_clim = [], []
    for _, row in table.iterrows():
        probs = row[prob_cols].to_numpy(dtype=float)
        one_hot = np.array([1.0 if row["observed_category"] == c else 0.0 for c in CATEGORIES])
        climatology = np.full(ncat, 1 / ncat)
        rps_pred.append(_rps(probs, one_hot) / (ncat - 1))
        rps_clim.append(_rps(climatology, one_hot) / (ncat - 1))

    rpss = 1 - (np.median(rps_pred) / np.median(rps_clim))
    return {"hss": hss, "rpss": rpss, "n_years": n}


def confusion_matrix(table: pd.DataFrame) -> pd.DataFrame:
    return pd.crosstab(table["observed_category"], table["predicted_category"]).reindex(
        index=CATEGORIES, columns=CATEGORIES, fill_value=0
    )
