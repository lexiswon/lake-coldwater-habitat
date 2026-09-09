"""Deterministic CVHT models: PCR / Random Forest / XGBoost, per-lake leave-one-out CV.

Mirrors the modeling choices in ``02-Deterministic_Models.R``: median-impute
predictors, drop zero-variance columns, standardize, reduce to 3 principal
components, then fit one of three regressors. Each lake is modeled
independently with leave-one-out cross-validation (refit per fold, like the
original R loop) so a lake's small sample size doesn't leak across models.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestRegressor
from sklearn.feature_selection import VarianceThreshold
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import LeaveOneOut
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from xgboost import XGBRegressor

from .data import LAKE_COL, OUTCOME_COL, predictor_columns

RANDOM_STATE = 42

MODEL_SPECS = {
    "PCR": lambda: LinearRegression(),
    "Random Forest": lambda: RandomForestRegressor(
        n_estimators=300, max_features=3, min_samples_leaf=5, random_state=RANDOM_STATE, n_jobs=-1
    ),
    "XGBoost": lambda: XGBRegressor(
        n_estimators=300,
        learning_rate=0.05,
        max_depth=6,
        min_child_weight=5,
        reg_lambda=0,
        random_state=RANDOM_STATE,
        verbosity=0,
        n_jobs=-1,
    ),
}


def _make_pipeline(estimator) -> Pipeline:
    return Pipeline(
        [
            ("impute", SimpleImputer(strategy="median")),
            ("zero_var", VarianceThreshold()),
            ("scale", StandardScaler()),
            ("pca", PCA(n_components=3, random_state=RANDOM_STATE)),
            ("model", estimator),
        ]
    )


def run_loocv_for_lake(lake_df: pd.DataFrame, model_name: str) -> np.ndarray:
    """Leave-one-out predictions for a single lake and model, in row order."""
    predictor_cols = predictor_columns(lake_df)
    X = lake_df[predictor_cols].to_numpy(dtype=float)
    y = lake_df[OUTCOME_COL].to_numpy(dtype=float)

    preds = np.full(len(lake_df), np.nan)
    loo = LeaveOneOut()
    for train_idx, test_idx in loo.split(X):
        pipe = _make_pipeline(MODEL_SPECS[model_name]())
        pipe.fit(X[train_idx], y[train_idx])
        preds[test_idx] = pipe.predict(X[test_idx])
    return preds


def run_all_models(df: pd.DataFrame) -> pd.DataFrame:
    """Run PCR/RF/XGBoost LOOCV for every lake; returns one tidy observed/predicted table."""
    rows = []
    for lake_id, lake_df in df.groupby(LAKE_COL, sort=False):
        lake_df = lake_df.reset_index(drop=True)
        preds = {name: run_loocv_for_lake(lake_df, name) for name in MODEL_SPECS}
        out = pd.DataFrame(
            {
                "lake_id": lake_id,
                "lake_name": lake_df["lake_name"],
                "year": lake_df["Year"],
                "observed_cvht": lake_df[OUTCOME_COL],
                "pred_pcr": preds["PCR"],
                "pred_rf": preds["Random Forest"],
                "pred_xgb": np.clip(preds["XGBoost"], 0, None),
            }
        )
        rows.append(out)
    return pd.concat(rows, ignore_index=True)


def score_table(export_all: pd.DataFrame) -> pd.DataFrame:
    """R^2 / RMSE / MAE per lake per model, long format."""
    model_cols = {"pred_pcr": "PCR", "pred_rf": "Random Forest", "pred_xgb": "XGBoost"}
    records = []
    for lake_id, g in export_all.groupby("lake_id", sort=False):
        for col, model_name in model_cols.items():
            obs, pred = g["observed_cvht"], g[col]
            resid = obs - pred
            ss_res = (resid**2).sum()
            ss_tot = ((obs - obs.mean()) ** 2).sum()
            r2 = 1 - ss_res / ss_tot if ss_tot > 0 else np.nan
            rmse = np.sqrt((resid**2).mean())
            mae = resid.abs().mean()
            records.append(
                {"lake_id": lake_id, "model": model_name, "r_squared": r2, "rmse": rmse, "mae": mae}
            )
    return pd.DataFrame(records)
