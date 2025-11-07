############################### Savitzky-Golay Filter and phenological phases extraction #####################################
# 1. Packages import
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import savgol_filter
import os

# 2. Load data read the Raw NDVI
csv_path = r'C:\Users\danie\OneDrive\Desktop\MASTER\Temporal\Sites_secondo_tentativo\Pys\NDVI\Tables\NDVI_Timeseries_raw.csv'
df = pd.read_csv(csv_path)
df['date'] = pd.to_datetime(df['date'])           # Convert 'date' column to datetime format
df = df.sort_values(['siteID', 'date'])           # Sort records by siteID and date (ascending)

# 3. Setting the output folders
plots_dir = r'C:\Users\danie\OneDrive\Desktop\MASTER\Temporal\Pys\Final'  # Folder for PNG graphs
tables_dir = r'C:\Users\danie\OneDrive\Desktop\MASTER\Temporal\Pys\Final' # Folder for the table
os.makedirs(plots_dir, exist_ok=True)
os.makedirs(tables_dir, exist_ok=True)

# 4. Lists the sites and the years to analyze
site_ids = df['siteID'].unique()                  # Get all site IDs in the dataset
years = [y for y in df['date'].dt.year.unique() if 2020 <= y <= 2024]  # Filter years (2020–2024 range)

# 5. List to store the results
all_phases = []                                   # to collect all results to export later

# 6. Putting in loop all the combination of year and sites
for site_id in site_ids:                          # Loop for each site
    df_site = df[df['siteID'] == site_id]

    for year in years:                            # Loop for each year
        df_year = df_site[df_site['date'].dt.year == year]

        if len(df_year) < 7:                      # Skip if too few data (7) points for smoothing
            print(f"Too few points in {site_id} {year}, for smoothering.")
            continue

        # Extract NDVI values and dates
        ndvi_raw = df_year['mean'].values         # Raw NDVI values
        ndvi_dates = df_year['date'].values       # date corrisponding

        # Apply the Savitzky–Golay filter for smoothing
        window_length = 7 if len(ndvi_raw) >= 7 else (len(ndvi_raw) // 2) * 2 + 1  # Define 7 as window size
        ndvi_smoothed = savgol_filter(ndvi_raw, window_length=window_length, polyorder=2)  # apply the window to the NDVI raw datas with 2 as poly order
        ndvi_diff = np.gradient(ndvi_smoothed)    # Compute the derivative as gradinete on the smoothed ndvi

        # 7. Detect the phenological phases
        # Peak phase
        mask_peak = (df_year['date'] >= pd.Timestamp(year=year, month=1, day=1)) & \
                    (df_year['date'] <= pd.Timestamp(year=year, month=6, day=1))     # Define the range of the date from jan to jun
        if mask_peak.any():                       # If the Jan–Jun range has data
            ndvi_sub = ndvi_smoothed[mask_peak.to_numpy()]
            peak_rel_idx = np.argmax(ndvi_sub)    # Get the max ndvi value
            peak_idx = np.where(mask_peak)[0][peak_rel_idx]
        else:                                     # Otherwise select the max ndvi value outside the range
            peak_idx = np.argmax(ndvi_smoothed)
        peak_date = df_year.iloc[peak_idx]['date']  # Date of NDVI maximum

        # Senescence phase
        senescence_date = None
        post_peak = np.where(np.arange(len(ndvi_diff)) > peak_idx)[0]  # Define the range that must be after the peak phase
        if len(post_peak) > 0:
            avg_decline = np.mean(ndvi_diff[ndvi_diff < 0])            # Get the average decline derivative value
            candidates = post_peak[ndvi_diff[post_peak] < avg_decline] # Get the points with the faster decline
            if len(candidates) > 0:
                senescence_idx = candidates[0]                         # first significant drop
                senescence_date = df_year.iloc[senescence_idx]['date'] # get the date
            else:
                senescence_idx = None
        else:
            senescence_idx = None

        # Decline phase
        decline_date = None
        decline_mask = (df_year['date'] >= pd.Timestamp(year=year, month=1, day=1)) & \
                       (df_year['date'] <= pd.Timestamp(year=year, month=11, day=1))     # Define the range that must be from jan to nov
        if decline_mask.any():
            ndvi_diff_sub = ndvi_diff[decline_mask.to_numpy()]
            idxs = np.where(decline_mask)[0]
            if senescence_idx is not None:
                valid = idxs[idxs > senescence_idx]                     # must be after the senescence
            else:
                valid = idxs
            if len(valid) > 0:
                sub_diff = ndvi_diff[valid]
                decline_rel_idx = np.argmin(sub_diff)                   # Get the higher diff value
                decline_idx = valid[decline_rel_idx]
                decline_date = df_year.iloc[decline_idx]['date']        # Get the date

        # Bottom phase
        bottom_date = None
        bottom_mask = (df_year['date'] >= pd.Timestamp(year=year, month=7, day=1)) & \
                      (df_year['date'] <= pd.Timestamp(year=year, month=12, day=1))    # Define the range that must be from jul to dec
        if bottom_mask.any():
            ndvi_sub = ndvi_smoothed[bottom_mask.to_numpy()]
            idxs = np.where(bottom_mask)[0]
            min_rel_idx = np.argmin(ndvi_sub)
            min_idx = idxs[min_rel_idx]                                 # Get the minimum ndvi value
            bottom_date = df_year.iloc[min_idx]['date']                 # Get the date

        # Green-up phase
        green_up_date = None
        if bottom_date:
            post_bottom = np.where(np.arange(len(ndvi_diff)) > min_idx)[0]  # must be after the bottom phase
            if len(post_bottom) > 0:
                greenup_sub = ndvi_diff[post_bottom]
                greenup_rel_idx = np.argmax(greenup_sub)                    # Get the maximum positive slop
                green_up_idx = post_bottom[greenup_rel_idx]
                green_up_date = df_year.iloc[green_up_idx]['date']          # Get the date

        # Maturity phase
        mature_date = None
        if green_up_date:
            post_green = np.where(np.arange(len(ndvi_smoothed)) >= green_up_idx)[0]    # must be after the green-up phase
            candidates = post_green[ndvi_smoothed[post_green] >= 0.95 * ndvi_smoothed[peak_idx]] # select the 95% of the value of the peak
            if len(candidates) > 0:
                mature_idx = candidates[0]
                mature_date = df_year.iloc[mature_idx]['date']  # Get the date

        # 8. Plot the curves and the detected phases
        plt.figure(figsize=(10, 6))
        plt.plot(df_year['date'], ndvi_raw, 'o-', label='NDVI raw')         # Raw ndvi in blue
        plt.plot(df_year['date'], ndvi_smoothed, 'r-', label='NDVI smoothed')  # Smoothed ndvi in red

        # Define the phases as vertical dashed lines
        plt.axvline(peak_date, color='cyan', linestyle='--', label=f'Peak {peak_date.date()}') # Peak in sky blue
        if senescence_date:
            plt.axvline(senescence_date, color='orange', linestyle='--', label=f'Senescence {senescence_date.date()}') # Senescence in orange
        if decline_date:
            plt.axvline(decline_date, color='red', linestyle='--', label=f'Decline {decline_date.date()}') # Decline in red
        if bottom_date:
            plt.axvline(bottom_date, color='purple', linestyle='--', label=f'Bottom {bottom_date.date()}') # Bottom in purple
        if green_up_date:
            plt.axvline(green_up_date, color='yellow', linestyle='--', label=f'Green-up {green_up_date.date()}') # Green-up in yellow
        if mature_date:
            plt.axvline(mature_date, color='green', linestyle='--', label=f'Maturity {mature_date.date()}')  # Maturity in green

        plt.title(f'Phenological Phases - {site_id} - {year}')
        plt.xlabel('Date')
        plt.ylabel('NDVI')
        plt.legend()
        plt.grid(True)
        plt.tight_layout()

        # Save the plots as PNG
        plot_path = os.path.join(plots_dir, f'{site_id}_{year}_Phenology.png')
        plt.savefig(plot_path)
        plt.close()

        # 9. Define the columns of the table to export
        phases = {
            'siteID': site_id,
            'year': year,
            'green_up': green_up_date,
            'maturity': mature_date,
            'peak': peak_date,
            'senescence': senescence_date,
            'decline': decline_date,
            'bottom': bottom_date
        }
        all_phases.append(phases)

# 10. Save all the outputs
phases_df = pd.DataFrame(all_phases)             # Convert list of dicts to dataframe
table_path = os.path.join(tables_dir, 'Phenology_all_dates.csv')
phases_df.to_csv(table_path, index=False)        # Export all phase dates to CSV
