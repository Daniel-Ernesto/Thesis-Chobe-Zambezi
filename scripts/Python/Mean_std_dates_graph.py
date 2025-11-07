############################### Mean and Std phenological dates graph ######################################
# 1. Import packages
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import matplotlib.patheffects as pe
from calendar import month_abbr

# 2. Load data and set the output
input_path = r"C:\Users\danie\OneDrive\Desktop\MASTER\Temporal\Sites_secondo_tentativo\Pys\NDVI\Tables\Phenology_full2.xlsx"
out_base   = r"C:\Users\danie\OneDrive\Desktop\MASTER\Temporal\Sites_secondo_tentativo\Pys\Final"
pivot_dir  = os.path.join(out_base, "pivot_pointplots")
os.makedirs(pivot_dir, exist_ok=True)

# 3. Define phases and color palettes
fasi = ['green_up', 'maturity', 'peak', 'senescence', 'decline', 'bottom']  # List of phenological phases
phase_palette = {            # Color palette for each phase
    'green_up':  'yellow',
    'maturity':  'green',
    'peak':      'cyan',
    'senescence':'orange',
    'decline':   'red',
    'bottom':    'purple',
}

# 4. Define label colors for site names
# Used to color Y-axis site labels by vegetation group, green for mopane and yellow baikiaea
label_colors = {
    "VSB302": "#439e1b", "VSB303": "#439e1b", "VSB304": "#439e1b",
    "VSN301": "#439e1b", "VSN302": "#439e1b", "VSN306": "#439e1b",
    "VSN307": "#439e1b", "VSN309": "#439e1b", "VSN310": "#439e1b",
    "VSB301": "#e3e218", "VSB305": "#e3e218", "VSB306": "#e3e218",
    "VSN303": "#e3e218", "VSN304": "#e3e218", "VSN305": "#e3e218",
}

# 5. Read input data and prepare the dataset
df = pd.read_excel(input_path)

for fase in fasi:   # Convert all phase columns from date to DOY
    df[fase] = pd.to_datetime(df[fase], errors='coerce').dt.dayofyear

if 'year' not in df.columns:
    raise ValueError("The column 'year' is missing in the input file")   # Ensure 'year' column is present

# Keep only years 2020–2024
years = [y for y in sorted(df['year'].dropna().unique()) if 2020 <= int(y) <= 2024]

# Define a fixed site order, from top to bottom in the plot
custom_sites = [f'VSB{n}' for n in range(301, 307)] + [f'VSN{n}' for n in range(301, 311)]
# Keep only sites available in the dataset
present_sites = [s for s in custom_sites if 'siteID' in df.columns and s in df['siteID'].dropna().unique()]
if not present_sites:
    raise ValueError("None of the expected 15 siteIDs are present in the dataset.")
# Map the sites on y axe
ypos = {site: i for i, site in enumerate(present_sites)}

# 6. Plot configuration
DATE_LABEL_POS = 'below'   # Position of date labels
DATE_LABEL_DY  = 12        # Vertical offset in points
DATE_LABEL_ROT = 45        # Rotation angle of date labels
PHASE_Y_JITTER = dict(zip(fasi, np.linspace(-0.18, 0.18, len(fasi))))  # offset between phases labels
SHOW_GAPS = True           # gap between phases

# Function to convert DOY to string dates (day-month)
def doy_to_datestr_en(doy_float: float, ref_year: int = 2022) -> str:
    """Convert a DOY (day of year) value to a string 'DD-Mon'."""
    if np.isnan(doy_float):
        return ""
    doy = int(round(doy_float))
    doy = max(1, min(366, doy))
    d = pd.Timestamp(year=ref_year, month=1, day=1) + pd.to_timedelta(doy - 1, unit='D')
    return f"{d.day:02d}-{month_abbr[d.month]}"
# Filter dataset to keep only the selected years
df_yrs = df[df['year'].isin(years)].copy()

# 7. Generate the mean ± std phenology plot
fig, ax = plt.subplots(figsize=(16, 9))

for site in present_sites:
    s = df_yrs[df_yrs['siteID'] == site]  # Subset for each site

    # Compute mean and std for each phase
    means = {}
    stds  = {}
    for fase in fasi:
        vals = s[fase].dropna().to_numpy() if fase in s.columns else np.array([])
        if vals.size == 0:
            means[fase] = np.nan
            stds[fase]  = np.nan
        else:
            means[fase] = float(np.nanmean(vals))
            stds[fase]  = float(np.nanstd(vals, ddof=1)) if vals.size >= 2 else 0.0  # std=0 if only 1 value

    # Keep only valid phases
    valid_items = [(fase, means[fase], stds[fase]) for fase in fasi if not np.isnan(means[fase])]
    if not valid_items:
        continue

    y = ypos[site]  # Vertical position of the site

    # Plot a grey horizontal line connecting phases
    xs_sorted = sorted([m for _, m, _ in valid_items])
    if len(xs_sorted) >= 2:
        ax.plot(xs_sorted, [y] * len(xs_sorted), '-', color='0.7', linewidth=1, zorder=1)

        # Annotate day gaps between consecutive phases
        if SHOW_GAPS:
            for x1, x2 in zip(xs_sorted[:-1], xs_sorted[1:]):
                gap = int(round(x2 - x1))
                xm = (x1 + x2) / 2.0
                ax.text(
                    xm, y - 0.18, f'{gap}d',
                    ha='center', va='top', fontsize=7, color='dimgray',
                    path_effects=[pe.withStroke(linewidth=3, foreground='white')],
                    clip_on=True, zorder=3
                )

    # Plot mean points, horizontal error bars and date labels
    for fase, m, sdev in valid_items:
        color = phase_palette[fase]
        yj = y + PHASE_Y_JITTER[fase]

        # Horizontal error bar
        ax.hlines(yj, m - sdev, m + sdev, color=color, linewidth=2.5, zorder=2)
        ax.plot([m - sdev, m - sdev], [yj - 0.07, yj + 0.07], color=color, linewidth=2, zorder=2)  # left
        ax.plot([m + sdev, m + sdev], [yj - 0.07, yj + 0.07], color=color, linewidth=2, zorder=2)  # right

        # Scatter point in mean
        ax.scatter(m, yj, s=50, facecolors=color, edgecolors='black', linewidths=0.6, zorder=3)

        # Annotate phase mean date below the point
        dy = DATE_LABEL_DY if DATE_LABEL_POS == 'above' else -DATE_LABEL_DY
        va = 'bottom' if DATE_LABEL_POS == 'above' else 'top'
        ax.annotate(
            doy_to_datestr_en(m),
            xy=(m, yj), xycoords='data',
            xytext=(0, dy), textcoords='offset points',
            ha='center', va=va, rotation=DATE_LABEL_ROT, rotation_mode='anchor',
            fontsize=7, color='black',
            path_effects=[pe.withStroke(linewidth=3, foreground='white')],
            zorder=4, clip_on=False
        )

# 8. Finalize and save the plot
ax.set_title('Dates of phenophases per site – mean ± std (2020–2024)')
ax.set_xlabel('DOY')
ax.set_xlim(0, 366)
ax.set_ylabel('Sites')
ax.set_yticks(range(len(present_sites)))
ax.set_yticklabels(present_sites)

# Color site labels
for lbl, site in zip(ax.get_yticklabels(), present_sites):
    lbl.set_color(label_colors.get(site, "black"))

ax.invert_yaxis()                  # Puts VSB301 on top
ax.grid(True, axis='x', alpha=0.3)

# Create legend with the phases with associated colors
handles = [Line2D([0], [0], marker='o', linestyle='',
                  markerfacecolor=phase_palette[f],
                  markeredgecolor='black', label=f) for f in fasi]
ax.legend(handles=handles, title='Phase',
          loc='upper left', bbox_to_anchor=(1.02, 1.00),
          borderaxespad=0.0, frameon=False)

# Adjust layout and export figure
fig.tight_layout(rect=[0, 0, 0.82, 1])
fig.savefig(os.path.join(pivot_dir, 'Mean_std_graphic.png'), dpi=200)
plt.show()
plt.close(fig)