############################### S2 Composites - Hyperparameter Tuning with Nested spatial CV (BlockKFold) and testing ##################################### --- import standard e scientifici ---
# 1. Packages import
import json, os
import numpy as np
import pandas as pd
import verde as vd
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
from warnings import filterwarnings

# 2. Mute “warnings” messages (since there are thousands of them and they clog up the console output)
filterwarnings("ignore", message="y_pred contains classes not in y_true")
filterwarnings("ignore", message="Precision and F-score are ill-defined")

# 3. import sklearn for model, tuning, and metrics
from sklearn.ensemble import RandomForestClassifier  # Random Forest classifier
from sklearn.model_selection import GridSearchCV  # Grid for the hyperparameters
from sklearn.metrics import (  # Valutation metrics
    accuracy_score, cohen_kappa_score, confusion_matrix, classification_report
)

# 4. Set the output path
os.makedirs("outputs_spatial_cv", exist_ok=True)  # Create the folder
log_path = os.path.join("outputs_spatial_cv", "spatial_cv_log.txt") # Set the log file path
logf = open(log_path, "w", encoding="utf-8")  # Open the log file

def logprint(*args, **kwargs):
    """Prints on the console and writes the same lines to the log file as well"""
    print(*args, **kwargs)  # Print on console
    print(*args, **kwargs, file=logf, flush=True)  # Write on the file

# 5. Load data and extract coordinates of training points
df = pd.read_csv("Rainy/Rainy_2023_training.csv")  # Load the exported training dataset from GEE
def extract_lon_lat(geo_str):
    """Extracts lon/lat from the GeoJSON string of the .geo column"""
    try:
        obj = json.loads(geo_str)
        lon, lat = obj.get("coordinates", [np.nan, np.nan])  # Read coordinates [lon, lat]
        return pd.Series({"lon": float(lon), "lat": float(lat)})  # Return as Series with lon/lat columns
    except Exception:
        return pd.Series({"lon": np.nan, "lat": np.nan})  # Return NaN in case of problems

# If the lon/lat columns are missing but .geo is present, we extract the coordinates from the .geo column.
if not ({"lon","lat"} <= set(df.columns)) and ".geo" in df.columns:
    df = pd.concat([df, df[".geo"].apply(extract_lon_lat)], axis=1)  # adds lon/lat columns derived from .geo

assert "class" in df.columns, "missing column in the CSV."  # check that the label column exists
df = df.dropna(subset=["lon","lat","class"])  # removes any rows without coordinates or class

# 6. Set Features, labels and coords
drop_cols = [c for c in ["system:index",".geo","source_file","source_species"] if c in df.columns]  # unnecessary columns to discard
X = df.drop(columns=["class","lon","lat"] + drop_cols)  # feature matrix: all columns except class/lon/lat and discards
y = df["class"].astype(int).to_numpy()  # label vector (LULC classes) as integers
coords = df[["lon","lat"]].to_numpy()  # Nx2 array of coordinates (required for blocks)

CLASSES = [1, 2, 3, 5, 6, 7]  # fixed list of classes (as the training dataset)

# 7. Grid Search for best hyperparameters
param_grid = {
    "n_estimators": [100, 200, 300, 400, 500], # Defining all the possible ranges of the hyperparameters
    'max_depth': [None, 10, 20, 30],
    'min_samples_split': [2, 5, 10],
    "max_features": ["sqrt", "log2", 3, 5, 7],
    "min_samples_leaf": [1, 2, 5],
    "max_leaf_nodes": [None, 100, 200, 400],
    "bootstrap": [True],
    "max_samples": [None, 0.5, 0.7, 0.9],
}

# 8. Define the blocks for nested spatial validation
def compute_blocks(coords, spacing_deg):
    """Given an array lon/lat and a spacing in degrees, calculate the block indices (bx, by),
    the list of occupied unique blocks, the counts per block, and the grid extension"""
    min_lon, min_lat = coords[:,0].min(), coords[:,1].min()  # origin (lower left corner) in degrees
    bx = np.floor((coords[:,0] - min_lon) / spacing_deg).astype(int)  # block index in x for each point
    by = np.floor((coords[:,1] - min_lat) / spacing_deg).astype(int)  # y-axis lock index for each point
    blocks = list(zip(bx, by))  # pairs (bx,by) for each ROI
    unique_blocks = sorted(set(blocks))  # set of occupied blocks (at least one point)
    # calculation of the extension (in indexes) of the occupied grid
    max_bx, max_by = max(b for b,_ in unique_blocks), max(b for _,b in unique_blocks)  # maximum occupancy rates
    # point counts per block
    from collections import Counter
    counts = Counter(blocks)  # dictionary (bx,by) -> number of points in the block
    return bx, by, unique_blocks, counts, (0, max_bx, 0, max_by), (min_lon, min_lat)

def plot_blocks(unique_blocks, counts, extent_idx, origin_ll, spacing_deg,
                test_block_set=None, title="Blocks (occupied)", out_path=None):
    """Draw the rectangles of the occupied blocks; if test_block_set is passed, highlight the test blocks"""
    (min_bx, max_bx, min_by, max_by) = (0, extent_idx[1], 0, extent_idx[3])  # limits in occupied block indexes
    min_lon, min_lat = origin_ll  # origin in lon/lat (bottom left corner)
    fig, ax = plt.subplots(figsize=(8, 7))  # create a figure
    # cycle on all occupied blocks
    for (bx, by) in unique_blocks:
        lon0 = min_lon + bx * spacing_deg  # length of the left edge of the block
        lat0 = min_lat + by * spacing_deg  # lat of the lower edge of the block
        is_test = (test_block_set is not None) and ((bx, by) in test_block_set)  # True if this block is in the test set
        fc = (1, 0.6, 0.2, 0.9) if is_test else (0.2, 0.5, 1.0, 0.3)  # full color: orange for test, blue for train
        ec = (0.6, 0.6, 0.6, 1)  # border color: black
        rect = Rectangle((lon0, lat0), spacing_deg, spacing_deg, facecolor=fc, edgecolor=ec, lw=1)  # block rectangle
        ax.add_patch(rect)  # adds the rectangle to the figure
        # writes the number of points in the center of the block
        n = counts.get((bx,by), 0)  # how many points in  blocks
        ax.text(lon0 + spacing_deg*0.5, lat0 + spacing_deg*0.5, str(n),
                ha="center", va="center", fontsize=8)  # label the number
    ax.set_title(title)
    ax.set_xlabel("Longitude (deg)")
    ax.set_ylabel("Latitude (deg)")
    ax.set_aspect("equal", adjustable="box")  # preserve proportions
    # Set the limits
    ax.set_xlim(min_lon - spacing_deg*0.1, min_lon + (max_bx+1)*spacing_deg + spacing_deg*0.1)
    ax.set_ylim(min_lat - spacing_deg*0.1, min_lat + (max_by+1)*spacing_deg + spacing_deg*0.1)
    if out_path:
        plt.tight_layout()
        plt.savefig(out_path, dpi=200)  # save the png in output path
        plt.close(fig)
    else:
        plt.show()

# 9. Nested BlockKFold
spacing_deg = 0.25   # block width in degrees 25°
K_outer, K_inner = 5, 5  # number of external folds for metrics of accuracy and internal for hyperparameters tuning
seed = 42  # random state for reproducibility
# calculate block structure for debugging/plotting
bx, by, unique_blocks, counts, extent_idx, origin_ll = compute_blocks(coords, spacing_deg)  # indexes, blocks, and info
logprint(f"[DEBUG] points: {len(coords)} | occupied blocks: {len(unique_blocks)}")  # base number log
pd.DataFrame(
    [(b[0], b[1], counts[(b[0], b[1])]) for b in unique_blocks],
    columns=["block_x","block_y","#points"]
).to_csv(os.path.join("outputs_spatial_cv", "blocks_counts.csv"), index=False)  # save CSV with counts per block
# plot of occupied blocks (without highlighting tests)
plot_blocks(unique_blocks, counts, extent_idx, origin_ll, spacing_deg,
            test_block_set=None,
            title=f"Blocks occupied (spacing={spacing_deg}°)",
            out_path=os.path.join("outputs_spatial_cv", "blocks_occupied.png"))  # save occupied blocks image in .png

outer = vd.BlockKFold(spacing=spacing_deg, n_splits=K_outer, shuffle=True, random_state=seed)  # defines external block CV
outer_splits = list(outer.split(coords))  # generates the list of external folds (train_idx, test_idx) based on the coordinates
logprint(f"[DEBUG] outer folds: {len(outer_splits)}")  # print how many external folds have been created
for i, (tr, te) in enumerate(outer_splits, 1):
    logprint(f"[DEBUG] fold {i}: train={len(tr)}  test={len(te)}  test%={len(te)/len(coords):.2%}")  # fold size summary

oa, kap = [], []  # lists to save OA and Kappa of external folds
y_true_all, y_pred_all = [], []
block_pairs = list(zip(bx, by))  # list (bx,by) for each sample
# loop on external folds
for fold_id, (tr_idx, te_idx) in enumerate(outer_splits, start=1):
    # determines the set of blocks being tested for this fold
    test_blocks = set([block_pairs[i] for i in te_idx])  # blocks of current test points
    plot_blocks(unique_blocks, counts, extent_idx, origin_ll, spacing_deg,
                test_block_set=test_blocks,
                title=f"Outer fold {fold_id}: test blocks",
                out_path=os.path.join("outputs_spatial_cv", f"blocks_outer_fold_{fold_id}.png"))  # save PNG of the fold

    # split of data into train/test for this fold
    X_tr, y_tr, coords_tr = X.iloc[tr_idx], y[tr_idx], coords[tr_idx]  # split of data into train/test for this fold
    X_te, y_te = X.iloc[te_idx], y[te_idx]  # test set (class/labels)
    # defines internal CV blocks only on the train (for hyperparameter tuning)
    inner = vd.BlockKFold(spacing=spacing_deg, n_splits=K_inner, shuffle=True, random_state=seed+fold_id)  # Internal CV
    inner_splits = list(inner.split(coords_tr))  # internal folds based on training coordinates

    base = RandomForestClassifier(random_state=seed)  # call RF classifier as base
    grid = GridSearchCV(
        base,                 # base classifier
        param_grid,           # hyperparameter grid
        cv=inner_splits,      # Internal block CV
        scoring="balanced_accuracy",  # robust metric for imbalance
        n_jobs=-1,
        verbose=0
    )
    grid.fit(X_tr, y_tr)  # performs tuning on the current fold training

    best_model = grid.best_estimator_  # extracts the best model (with the best parameters of the fold)
    y_pred = best_model.predict(X_te)  # predicts on the external fold test

    this_oa = accuracy_score(y_te, y_pred)  # calculate Overall Accuracy of the external fold
    this_k  = cohen_kappa_score(y_te, y_pred, labels=CLASSES)  # calculate kappa

    oa.append(this_oa)  # save OA of the current fold
    kap.append(this_k)  # save Kappa of the current fold
    y_true_all.extend(y_te)   # accumulate labels for the confusion matrix
    y_pred_all.extend(y_pred) # accumulate prediction for the confusion matrix

    logprint(f"[Fold {fold_id}] OA={this_oa:.3f}  Kappa={this_k:.3f}  best={grid.best_params_}")  # log fold

# summary of mean ± standard deviation of metrics on external folds
oa_mean, oa_std = np.mean(oa), np.std(oa, ddof=1)  # mean and std OA on external folds
kap_mean, kap_std = np.mean(kap), np.std(kap, ddof=1)  # mean and std Kappa on external folds
logprint("\n== Realistic spatial performance (mean ± std) ==")
logprint(f"Overall Accuracy: {oa_mean:.3f} ± {oa_std:.3f}")  # print OA values
logprint(f"Cohen's Kappa  : {kap_mean:.3f} ± {kap_std:.3f}")  # print Kappa values

# confusion matrix reports summing all external fold tests
cm_all = confusion_matrix(y_true_all, y_pred_all, labels=CLASSES) # aggregate confusion matrix
logprint("\nConfusion Matrix aggregate on each fold:\n", cm_all)  # print matrix
logprint("\nAggregate classification report:")
logprint(classification_report(y_true_all, y_pred_all, labels=CLASSES, zero_division=0, digits=4))  # print aggregate report

# save the confusion matrix as a tabular CSV file
pd.DataFrame(cm_all, index=[f"true_{c}" for c in CLASSES], columns=[f"pred_{c}" for c in CLASSES]) \
  .to_csv(os.path.join("outputs_spatial_cv", "confusion_matrix_aggregated.csv"))

# 10. Final hyperparameters to insert in GEE code
final_cv = vd.BlockKFold(spacing=spacing_deg, n_splits=K_inner, shuffle=True, random_state=seed)  # block CV on all data
final_splits = list(final_cv.split(coords))  # fold for the final CV (the best tuned hyperparameters)
final = GridSearchCV(
    RandomForestClassifier(random_state=seed),
    param_grid,
    cv=final_splits,
    scoring="balanced_accuracy",
    n_jobs=-1,
    verbose=0
)
final.fit(X, y)  # overall spatial tuning across the entire dataset
best_params = final.best_params_  # extract final hyperparameters to be brought into GEE
logprint("\n== Final params to use in GEE ==")
logprint(best_params)  # print the dictionary of final parameters

logf.close()  # closes the log file
print(f"\n All saved")