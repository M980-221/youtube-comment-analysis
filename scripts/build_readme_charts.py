#!/usr/bin/env python3
"""Render portfolio figures from the exported R analysis tables.

Usage:
    python scripts/build_readme_charts.py --data-dir results/tables --output-dir docs/images

Requires matplotlib and numpy. This script plots existing results; it does not
fit the R models or replace the analytical scripts.
"""

import argparse
import csv
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.ticker import PercentFormatter, StrMethodFormatter
import numpy as np


ARTISTS = ["Taylor Swift", "Ariana Grande", "Billie Eilish"]
COLORS = ["#3274A1", "#9067A3", "#4B8B62"]
TEXT = "#233345"
MUTED = "#627181"
GRID = "#E7ECF0"


def table(directory, filename):
    with (directory / filename).open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def ordered(rows, field="artist", order=ARTISTS):
    index = {row[field]: row for row in rows}
    return [index[key] for key in order]


def style_axes(ax, grid="x"):
    ax.set_axisbelow(True)
    ax.grid(axis=grid, color=GRID, linewidth=0.8)
    ax.tick_params(length=0, pad=9, colors=MUTED)
    for spine in ax.spines.values():
        spine.set_visible(False)


def header(fig, title, subtitle):
    fig.text(0.065, 0.937, title, fontsize=22, fontweight="bold", color=TEXT)
    fig.text(0.065, 0.884, subtitle, fontsize=12, color=MUTED)


def footer(fig, text):
    fig.text(0.065, 0.055, text, fontsize=10.5, color=MUTED, va="bottom")


def save(fig, directory, filename):
    fig.savefig(directory / filename, dpi=110, facecolor="white")
    plt.close(fig)


def participation(data, output):
    rows = ordered(table(data, "Q4_Unique_Actor_Summary.csv"))
    counts = np.array([int(r["unique_actors"]) for r in rows])
    repeats = np.array([int(r["repeat_actors"]) for r in rows])
    shares = 100 * repeats / counts
    assert counts.tolist() == [2657, 2932, 3174]
    assert all(int(r["total_comments"]) == 3600 for r in rows)
    assert np.allclose(shares, [float(r["repeat_actor_percentage"]) for r in rows], atol=0.005)
    fig = plt.figure(figsize=(14.8, 7.7))
    header(fig, "Participation differs despite equal comment samples",
           "3,600 comments per artist · unique authors identified by channel ID")
    ax1 = fig.add_axes([0.185, 0.22, 0.34, 0.54])
    ax2 = fig.add_axes([0.62, 0.22, 0.285, 0.54])
    y = np.arange(3)
    ax1.barh(y, counts, color=COLORS, height=0.48)
    ax2.barh(y, shares, color=COLORS, height=0.48)
    for ax in [ax1, ax2]:
        style_axes(ax)
        ax.invert_yaxis()
        ax.set_ylim(2.6, -0.6)
    ax1.set_yticks(y, ARTISTS, fontsize=12, color=TEXT)
    ax2.set_yticks(y, [""] * 3)
    ax1.set_xlim(0, 3600)
    ax1.set_xticks([0, 1000, 2000, 3000])
    ax1.xaxis.set_major_formatter(StrMethodFormatter("{x:,.0f}"))
    ax1.set_xlabel("Distinct authors", color=MUTED, labelpad=12)
    ax1.set_title("Unique actors", loc="left", pad=18, fontsize=15, fontweight="bold", color=TEXT)
    ax2.set_xlim(0, 15)
    ax2.set_xticks([0, 5, 10, 15])
    ax2.xaxis.set_major_formatter(PercentFormatter(100, decimals=0))
    ax2.set_xlabel("Share of distinct authors", color=MUTED, labelpad=12)
    ax2.set_title("Repeat commenters", loc="left", pad=18, fontsize=15, fontweight="bold", color=TEXT)
    for idx, value in enumerate(counts):
        ax1.text(value + 65, idx, f"{value:,}", va="center", fontsize=13, color=TEXT, fontweight="bold")
    for idx, value in enumerate(shares):
        ax2.text(value + 0.32, idx, f"{value:.2f}%", va="center", fontsize=13, color=TEXT, fontweight="bold")
    footer(fig, "Repeat commenter = more than one collected comment. These patterns describe the sample, not total fanbase size or loyalty.")
    save(fig, output, "audience_participation.png")


def sentiment(data, output):
    rows = ordered(table(data, "Q9_Artist_Sentiment_Summary.csv"))
    categories = ["Positive", "Neutral", "Negative"]
    counts = np.array([[int(r[f"{category.lower()}_comments"]) for category in categories] for r in rows])
    assert counts.tolist() == [[1446, 1743, 411], [1317, 1808, 475], [1338, 1862, 400]]
    totals = counts.sum(axis=1)
    assert totals.tolist() == [3600] * 3
    percentages = counts / totals[:, None] * 100
    fig = plt.figure(figsize=(14.8, 7.7))
    header(fig, "Neutral comments dominate; positive shares differ modestly",
           "Sentimentr classification · 3,600 comments per artist · percentages calculated from exact counts")
    ax = fig.add_axes([0.185, 0.25, 0.73, 0.48])
    style_axes(ax)
    palette = ["#478C68", "#CFD6DE", "#D66D68"]
    left = np.zeros(3)
    for idx, category in enumerate(categories):
        values = percentages[:, idx]
        ax.barh(np.arange(3), values, left=left, label=category, color=palette[idx], height=0.53,
                edgecolor="white", linewidth=1.5)
        for row_idx, value in enumerate(values):
            ax.text(left[row_idx] + value / 2, row_idx, f"{value:.2f}%", ha="center", va="center",
                    fontsize=12, fontweight="bold", color=TEXT if idx == 1 else "white")
        left += values
    ax.set_yticks(range(3), ARTISTS, fontsize=12, color=TEXT)
    ax.set_ylim(2.6, -0.6)
    ax.set_xlim(0, 100)
    ax.set_xticks([0, 25, 50, 75, 100])
    ax.xaxis.set_major_formatter(PercentFormatter(100, decimals=0))
    ax.set_xlabel("Share of collected comments", color=MUTED, labelpad=13)
    ax.legend(loc="upper left", bbox_to_anchor=(0, 1.17), ncol=3, frameon=False, fontsize=12,
              labelcolor=TEXT, handlelength=1.4, columnspacing=2.5)
    footer(fig, "Positive: score > 0.05 · negative: score < −0.05 · otherwise neutral. Lexicon scores are sensitive to lyrics, sarcasm and language.")
    save(fig, output, "sentiment_comparison.png")


def model(data, output):
    rows = ordered(table(data, "Q10_Model_Performance.csv"), "model", ["Baseline tree", "Tuned tree"])
    values = np.array([[float(r["accuracy"]), float(r["macro_f1"])] for r in rows]) * 100
    cells = table(data, "Q10_Tuned_Confusion_Matrix.csv")
    matrix = np.zeros((3, 3), dtype=int)
    for row in cells:
        matrix[ARTISTS.index(row["Actual"]), ARTISTS.index(row["Predicted"])] = int(row["Comments"])
    assert matrix.tolist() == [[691, 7, 22], [605, 86, 29], [638, 19, 63]]
    assert matrix.sum() == 2160 and np.all(matrix.sum(axis=1) == 720)
    assert abs(np.trace(matrix) / matrix.sum() - float(rows[1]["accuracy"])) < 1e-9
    shares = matrix / matrix.sum(axis=1)[:, None] * 100
    fig = plt.figure(figsize=(14.8, 8.1))
    header(fig, "Tuning improves the tree, but predictions remain concentrated",
           "Held-out test set: 2,160 comments · 720 per artist · three equally sized classes")
    ax1 = fig.add_axes([0.09, 0.235, 0.35, 0.51])
    ax2 = fig.add_axes([0.635, 0.255, 0.28, 0.485])
    style_axes(ax1, "y")
    x = np.arange(2)
    width = 0.32
    for idx, (row, color) in enumerate(zip(rows, ["#9FAFBE", "#3274A1"])):
        positions = x + (idx - 0.5) * width
        ax1.bar(positions, values[idx], width=width, label=row["model"], color=color)
        for pos, value in zip(positions, values[idx]):
            ax1.text(pos, value + 1.6, f"{value:.2f}", ha="center", fontsize=11, fontweight="bold", color=TEXT)
    ax1.set_xticks(x, ["Accuracy", "Macro F1"], color=TEXT, fontsize=12)
    ax1.set_ylim(0, 55)
    ax1.set_yticks([0, 10, 20, 30, 40, 50])
    ax1.set_ylabel("Score × 100", color=MUTED, labelpad=8)
    ax1.axhline(100 / 3, color=MUTED, linewidth=1, linestyle=(0, (4, 4)))
    ax1.text(1.43, 100 / 3 + 1, "33.33% accuracy\nmajority-class baseline", ha="right", va="bottom", fontsize=9.5, color=MUTED)
    ax1.set_title("Test performance", loc="left", fontsize=15, fontweight="bold", color=TEXT, pad=18)
    ax1.legend(frameon=False, loc="upper left", bbox_to_anchor=(0, -0.14), ncol=2, fontsize=10.5, labelcolor=TEXT)
    ax2.imshow(shares, cmap="Blues", vmin=0, vmax=100, aspect="equal")
    short = ["Taylor", "Ariana", "Billie"]
    ax2.set_xticks(range(3), short, fontsize=11, color=TEXT)
    ax2.set_yticks(range(3), short, fontsize=11, color=TEXT)
    ax2.set_xlabel("Predicted artist", color=MUTED, labelpad=11)
    ax2.set_ylabel("Actual artist", color=MUTED, labelpad=10)
    ax2.tick_params(length=0, pad=8)
    for spine in ax2.spines.values():
        spine.set_visible(False)
    ax2.set_xticks(np.arange(-0.5, 3, 1), minor=True)
    ax2.set_yticks(np.arange(-0.5, 3, 1), minor=True)
    ax2.grid(which="minor", color="white", linewidth=2)
    ax2.tick_params(which="minor", bottom=False, left=False)
    for (row_idx, col_idx), count in np.ndenumerate(matrix):
        ax2.text(col_idx, row_idx, f"{count}\n{shares[row_idx, col_idx]:.1f}%", ha="center", va="center",
                 fontsize=12, color="white" if shares[row_idx, col_idx] > 55 else TEXT)
    ax2.set_title("Tuned tree confusion matrix", loc="left", fontsize=15, fontweight="bold", color=TEXT, pad=18)
    footer(fig, "Accuracy gains 3.43 percentage points, yet 89.54% of predictions are Taylor Swift. Each matrix percentage is within its actual class.")
    save(fig, output, "model_evaluation.png")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    plt.rcParams.update({"font.family": "DejaVu Sans", "font.size": 11, "figure.facecolor": "white"})
    participation(args.data_dir, args.output_dir)
    sentiment(args.data_dir, args.output_dir)
    model(args.data_dir, args.output_dir)
    print("Rendered audience_participation.png, sentiment_comparison.png and model_evaluation.png")


if __name__ == "__main__":
    main()
