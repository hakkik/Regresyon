from __future__ import annotations

from itertools import combinations
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import scipy.stats as stats
import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.linear_model import Ridge
from sklearn.preprocessing import StandardScaler
from statsmodels.stats.diagnostic import het_breuschpagan
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.stats.stattools import durbin_watson


BASE_DIR = Path(__file__).resolve().parent
PLOTS_DIR = BASE_DIR / "plots"
COLUMNS = ["verim", "isik", "sicaklik", "su", "mineraller"]


def read_dataset(path: Path) -> pd.DataFrame:
    data = pd.read_csv(path, sep=r"\s+", quotechar='"', engine="python")
    data.columns = COLUMNS
    data["mineraller"] = data["mineraller"].astype("category")
    return data


def save_dataset(data: pd.DataFrame, path: Path) -> None:
    data.to_csv(path, sep=" ", index=False)


def describe_data(name: str, data: pd.DataFrame) -> None:
    print(f"\n=== {name}: betimsel istatistikler ===")
    print(data.describe(include="all"))


def normality_report(label: str, series: pd.Series) -> None:
    clean = pd.Series(series).replace([np.inf, -np.inf], np.nan).dropna()
    shapiro_stat, shapiro_p = stats.shapiro(clean)
    ks_stat, ks_p = stats.kstest(clean, "norm", args=(clean.mean(), clean.std(ddof=1)))

    print(f"\n=== Normallik: {label} ===")
    print(f"Shapiro-Wilk: statistic={shapiro_stat:.4f}, p-value={shapiro_p:.4g}")
    print(f"Kolmogorov-Smirnov: statistic={ks_stat:.4f}, p-value={ks_p:.4g}")


def run_normality_block(prefix: str, data: pd.DataFrame) -> None:
    log_verim = np.log(data["verim"].where(data["verim"] > 0))
    sqrt_verim = sqrt_transform(data["verim"])

    normality_report(f"{prefix} verim", data["verim"])
    plot_qq(f"{prefix} verim Q-Q", data["verim"], f"{prefix}_verim_qq.png")

    normality_report(f"{prefix} log(verim)", log_verim)
    plot_qq(f"{prefix} log(verim) Q-Q", log_verim, f"{prefix}_log_verim_qq.png")

    normality_report(f"{prefix} sqrt(verim - min(verim) + 1)", sqrt_verim)
    plot_qq(
        f"{prefix} sqrt(verim - min(verim) + 1) Q-Q",
        sqrt_verim,
        f"{prefix}_sqrt_verim_qq.png",
    )


def sqrt_transform(series: pd.Series) -> pd.Series:
    return np.sqrt(series - series.min() + 1)


def boxplot_outlier_positions(series: pd.Series) -> list[int]:
    q1 = series.quantile(0.25)
    q3 = series.quantile(0.75)
    iqr = q3 - q1
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    return np.flatnonzero((series < lower) | (series > upper)).tolist()


def plot_qq(label: str, series: pd.Series, filename: str) -> None:
    clean = pd.Series(series).replace([np.inf, -np.inf], np.nan).dropna()
    fig = sm.qqplot(clean, line="45", fit=True)
    fig.suptitle(label)
    fig.tight_layout()
    fig.savefig(PLOTS_DIR / filename, dpi=140)
    plt.close(fig)


def plot_boxplot(series: pd.Series, outlier_positions: list[int], filename: str) -> None:
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.boxplot(series, vert=True)
    ax.set_title("Verim boxplot")
    ax.set_ylabel("verim")

    for position in outlier_positions:
        ax.annotate(
            str(position + 1),
            xy=(1, series.iloc[position]),
            xytext=(1.08, series.iloc[position]),
            color="red",
            fontsize=9,
        )

    fig.tight_layout()
    fig.savefig(PLOTS_DIR / filename, dpi=140)
    plt.close(fig)


def plot_pairs(data: pd.DataFrame, filename: str) -> None:
    axes = pd.plotting.scatter_matrix(
        data[["verim", "isik", "sicaklik", "su"]],
        figsize=(8, 8),
        diagonal="hist",
    )
    fig = axes[0, 0].figure
    fig.suptitle("Degiskenler arasi iliski")
    fig.tight_layout()
    fig.savefig(PLOTS_DIR / filename, dpi=140)
    plt.close(fig)


def plot_index_diagnostic(
    values: pd.Series,
    threshold,
    title: str,
    ylabel: str,
    filename: str,
    label_mask,
) -> None:
    x = np.arange(1, len(values) + 1)
    fig, ax = plt.subplots(figsize=(9, 5))
    ax.plot(x, values, linestyle="none", marker="*", markersize=10)
    ax.set_title(title)
    ax.set_xlabel("Index")
    ax.set_ylabel(ylabel)

    if isinstance(threshold, tuple):
        for line in threshold:
            ax.axhline(line, color="red", linewidth=1)
    else:
        ax.axhline(threshold, color="red", linewidth=1)

    for idx, value in enumerate(values, start=1):
        if bool(label_mask.iloc[idx - 1] if hasattr(label_mask, "iloc") else label_mask[idx - 1]):
            ax.annotate(str(idx), xy=(idx, value), xytext=(4, 4), textcoords="offset points", color="red")

    fig.tight_layout()
    fig.savefig(PLOTS_DIR / filename, dpi=140)
    plt.close(fig)


def plot_influence_diagnostics(model, influence: pd.DataFrame, prefix: str) -> None:
    n = len(influence)
    k = int(model.df_model)
    leverage_limit = 2 * (k + 1) / n
    cooks_limit = 4 / n if n > 50 else 4 / (n - k - 1)

    plot_index_diagnostic(
        influence["hat"],
        leverage_limit,
        "Leverage Value by Hat value",
        "Hat value",
        f"{prefix}_leverage_hat.png",
        influence["hat"] > leverage_limit,
    )
    plot_index_diagnostic(
        influence["standardized_resid"],
        (-2, 2),
        "Outlier by Standardized residuals",
        "Standardized Residuals",
        f"{prefix}_standardized_residuals.png",
        influence["standardized_resid"].abs() > 2,
    )
    plot_index_diagnostic(
        influence["studentized_resid"],
        (-3, 3),
        "Outlier by Studentized residuals",
        "Studentized Residuals",
        f"{prefix}_studentized_residuals.png",
        influence["studentized_resid"].abs() > 3,
    )
    plot_index_diagnostic(
        influence["cooks_d"],
        cooks_limit,
        "Influential Obs by Cooks distance",
        "Cook's distance",
        f"{prefix}_cooks_distance.png",
        influence["cooks_d"] > cooks_limit,
    )


def plot_fitted_vs_studentized(model, filename: str) -> None:
    influence = model.get_influence().summary_frame()
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.scatter(model.fittedvalues, influence["student_resid"])
    ax.axhline(0, color="black", linewidth=1)
    ax.axhline(2, color="red", linewidth=1, linestyle="--")
    ax.axhline(-2, color="red", linewidth=1, linestyle="--")
    ax.set_xlabel("Tahmin Degerleri")
    ax.set_ylabel("Student Tip Artiklar")
    ax.set_title("Tahmin Degerleri vs Student Tip Artiklar")
    fig.tight_layout()
    fig.savefig(PLOTS_DIR / filename, dpi=140)
    plt.close(fig)


def fit_model(data: pd.DataFrame):
    model_data = data.copy()
    model_data["sqrt_verim"] = sqrt_transform(model_data["verim"])
    return smf.ols("sqrt_verim ~ isik + sicaklik + su + C(mineraller)", data=model_data).fit()


def model_report(name: str, model) -> None:
    print(f"\n=== {name}: OLS model ozeti ===")
    print(model.summary())
    print(f"\n=== {name}: %99 guven araliklari ===")
    print(model.conf_int(alpha=0.01))


def influence_report(model, n: int) -> pd.DataFrame:
    influence = model.get_influence()
    frame = influence.summary_frame()
    k = int(model.df_model)
    leverage_limit = 2 * (k + 1) / n
    cooks_limit = 4 / n if n > 50 else 4 / (n - k - 1)

    result = pd.DataFrame(
        {
            "hat": frame["hat_diag"],
            "standardized_resid": frame["standard_resid"],
            "studentized_resid": frame["student_resid"],
            "cooks_d": frame["cooks_d"],
        }
    )

    print("\n=== Etki ve aykiri gozlem kontrolleri ===")
    print(f"Leverage esigi: {leverage_limit:.4f}")
    print("Yuksek leverage satirlari (1-bazli):", one_based(result.index[result["hat"] > leverage_limit]))
    print("Std. artik > 2 veya < -2 (1-bazli):", one_based(result.index[result["standardized_resid"].abs() > 2]))
    print("Student artik > 3 veya < -3 (1-bazli):", one_based(result.index[result["studentized_resid"].abs() > 3]))
    print(f"Cook uzakligi esigi: {cooks_limit:.4f}")
    print("Etkili gozlemler (1-bazli):", one_based(result.index[result["cooks_d"] > cooks_limit]))
    return result


def one_based(index_values) -> list[int]:
    return [int(i) + 1 for i in index_values]


def assumption_report(model) -> None:
    bp_stat, bp_pvalue, f_stat, f_pvalue = het_breuschpagan(model.resid, model.model.exog)
    print("\n=== Varsayim testleri ===")
    print(f"Breusch-Pagan: LM={bp_stat:.4f}, p-value={bp_pvalue:.4g}, F={f_stat:.4f}, F p-value={f_pvalue:.4g}")
    print(f"Durbin-Watson: {durbin_watson(model.resid):.4f}")
    print("\nVIF:")
    print(vif_table(model))


def vif_table(model) -> pd.DataFrame:
    exog = pd.DataFrame(model.model.exog, columns=model.model.exog_names)
    return pd.DataFrame(
        {
            "variable": exog.columns,
            "VIF": [variance_inflation_factor(exog.values, i) for i in range(exog.shape[1])],
        }
    )


def aic_selection(data: pd.DataFrame) -> None:
    predictors = ["isik", "sicaklik", "su", "C(mineraller)"]
    model_data = data.copy()
    model_data["sqrt_verim"] = sqrt_transform(model_data["verim"])

    scores = []
    for size in range(1, len(predictors) + 1):
        for subset in combinations(predictors, size):
            formula = "sqrt_verim ~ " + " + ".join(subset)
            model = smf.ols(formula, data=model_data).fit()
            scores.append((model.aic, formula))

    best_aic, best_formula = min(scores, key=lambda item: item[0])
    backward = smf.ols("sqrt_verim ~ " + " + ".join(predictors), data=model_data).fit()

    print("\n=== Degisken secimi (AIC) ===")
    print(f"En dusuk AIC: {best_aic:.4f}")
    print(f"En iyi formül: {best_formula}")
    print(f"Tam model AIC: {backward.aic:.4f}")


def ridge_report(data: pd.DataFrame) -> None:
    model_data = data.copy()
    y = sqrt_transform(model_data["verim"])
    x = pd.get_dummies(model_data[["isik", "sicaklik", "su", "mineraller"]], drop_first=True)
    scaler = StandardScaler()
    x_scaled = scaler.fit_transform(x)
    lambdas = np.arange(0.0, 1.05, 0.05)
    coefs = []

    for alpha in lambdas:
        ridge = Ridge(alpha=alpha)
        ridge.fit(x_scaled, y)
        coefs.append(ridge.coef_)

    coef_frame = pd.DataFrame(coefs, columns=x.columns, index=lambdas)
    coef_frame.index.name = "lambda"

    print("\n=== Ridge regresyon katsayilari ===")
    print(coef_frame)

    ax = coef_frame.plot(figsize=(9, 5))
    ax.axhline(0, color="black", linewidth=1)
    ax.set_xlabel("lambda")
    ax.set_ylabel("standardize katsayi")
    ax.set_title("Ridge katsayilarinin lambda ile degisimi")
    ax.figure.tight_layout()
    ax.figure.savefig(PLOTS_DIR / "ridge_coefficients.png", dpi=140)
    plt.close(ax.figure)


def main() -> None:
    PLOTS_DIR.mkdir(exist_ok=True)

    data1 = read_dataset(BASE_DIR / "data1.txt")
    describe_data("data1", data1)
    run_normality_block("data1", data1)

    first_outliers = boxplot_outlier_positions(data1["verim"])
    print("\nBoxplot aykirilar (1-bazli):", one_based(first_outliers))
    plot_boxplot(data1["verim"], first_outliers, "data1_verim_boxplot.png")
    data2 = data1.drop(index=first_outliers).reset_index(drop=True)
    save_dataset(data2, BASE_DIR / "data2_python.txt")

    describe_data("data2_python", data2)
    run_normality_block("data2", data2)
    plot_pairs(data2, "data2_pairs.png")

    first_model = fit_model(data2)
    model_report("Ilk model", first_model)
    influence = influence_report(first_model, len(data2))
    plot_influence_diagnostics(first_model, influence, "data2_model")

    k = int(first_model.df_model)
    leverage_limit = 2 * (k + 1) / len(data2)
    cooks_limit = 4 / len(data2)
    second_outliers = sorted(
        set(influence.index[influence["hat"] > leverage_limit])
        | set(influence.index[influence["standardized_resid"].abs() > 2])
        | set(influence.index[influence["studentized_resid"].abs() > 3])
        | set(influence.index[influence["cooks_d"] > cooks_limit])
    )
    print("Python kriterlerine gore 2. tur cikarilacaklar (1-bazli):", one_based(second_outliers))
    data3 = data2.drop(index=second_outliers).reset_index(drop=True)
    save_dataset(data3, BASE_DIR / "data3_python.txt")

    describe_data("data3_python", data3)
    run_normality_block("data3", data3)
    plot_pairs(data3, "data3_pairs.png")
    final_model = fit_model(data3)
    model_report("Nihai model", final_model)
    plot_fitted_vs_studentized(final_model, "data3_fitted_vs_studentized_residuals.png")
    assumption_report(final_model)
    aic_selection(data3)
    ridge_report(data3)

    print("\nGrafikler plots/ klasorune kaydedildi.")


if __name__ == "__main__":
    main()
