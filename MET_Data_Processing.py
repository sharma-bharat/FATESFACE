# Met Data Analysis
# by Bharat Sharma <br>
# sharmabd@ornl.gov <br>
# Site: US-DUKE


# importing libraries
import xarray as xr
import glob
from datetime import datetime
import cftime
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

## Files
# - **ELM-DUKE** : has the nc files that we use to run the current version of ELM-FATES <br>
# - **[FACEMDS_Walker2018](https://data.ess-dive.lbl.gov/view/ess-dive-7807cf86f1dd42a-20181127T173047368940)** : Processed Data (My focus: DUKE) <br>
# - **[DukeFACE_Oren2022](https://data.ess-dive.lbl.gov/view/doi:10.15485/1895465)**: Updated DUKE met Data

# paths

paths = {}
paths[
    "ELM-DUKE"
] = "/Users/ud4/Documents/FACEMDS/MET_Data_Processing/ELM_Data/data/atm/datm7/CLM1PT_data/1x1pt_US-DUK/"
paths[
    "FACEMDS_Walker2018"
] = "/Users/ud4/Documents/FACEMDS/MET_Data_Processing/Walker_2018_FATES_MDS/data/"
paths[
    "DukeFACE_Oren2022"
] = "/Users/ud4/Documents/FACEMDS/MET_Data_Processing/Oren_2022_DUKE_Met/data/"


## ELM Duke Data

key = "ELM-DUKE"
ds_elm_all = xr.open_mfdataset(glob.glob(f"{paths[key]}*.nc"))

# cftime to datetime


def cftime_to_dtime(cftime_object):
    # Convert cftime.DatetimeNoLeap to datetime
    datetime_object = datetime(
        year=cftime_object.year,
        month=cftime_object.month,
        day=cftime_object.day,
        hour=cftime_object.hour,
        minute=cftime_object.minute,
        second=cftime_object.second,
        microsecond=cftime_object.microsecond,
    )
    return datetime_object


time_objects = [cftime_to_dtime(t) for t in ds_elm_all.time.values]
fig1 = plt.figure(figsize=(20, 8))
plt.scatter(
    x=time_objects,
    y=ds_elm_all.PRECTmms,
    c=ds_elm_all.PRECTmms,
    cmap="Reds",
    s=22,
    marker="o",
    label=ds_elm_all.PRECTmms.units,
)
plt.title(f"{key}")
plt.legend()
plt.show()

## Investigating FACEMDS_Walker2018

key = "FACEMDS_Walker2018"
ds_Walker_h = xr.open_dataset(f"{paths[key]}DUKE_forcing_h.nc", decode_times=False)

from datetime import datetime, timedelta


def seconds_to_datetime(seconds, reference_date):
    return reference_date + timedelta(seconds=seconds)


reference_date = datetime(1996, 1, 1, 0, 0)

list_seconds = ds_Walker_h.TIME[...].values
list_seconds = np.asarray(list_seconds, dtype=float)
resulting_datetime = [
    seconds_to_datetime(seconds, reference_date) for seconds in list_seconds
]

fig2 = plt.figure(figsize=(20, 8))
plt.scatter(
    x=resulting_datetime,
    y=ds_Walker_h["Rainf"],
    c=ds_Walker_h["Rainf"],
    cmap="Reds",
    s=22,
    marker="o",
    label=ds_Walker_h["Rainf"].units,
)
plt.title(f"{key}")
plt.legend()
plt.show()

### FACEMDS_Walker CSV files
key = "FACEMDS_Walker2018"
#### Reading Hourly files
df_FACEMDS = {}
df_FACEMDS["h"] = pd.read_csv(glob.glob(f"{paths[key]}DUKE*_h.txt")[0], delimiter="\t")
print(len(df_FACEMDS["h"].columns))
df_FACEMDS["h"].columns


## Oren New Met Data¶
key = "DukeFACE_Oren2022"
var_key = "AT"
### AT : Tair
"""
Oren Data has 3 dirs for AT for different plots, time periods, and sensors. <br>
I intend to use `*_gl.csv` files; i believe these are gap filled. <br>
I will save the mean of the plots for FACEMDS <br>
The common sensor data will be used among all the files.
"""

# Create a common dataframe
# List of column names
common_columns_at = ["Year", "JDT", "DOY", "Time", "Tair"]

# Create an empty DataFrame with the specified columns
df_AT_common_gf = pd.DataFrame(columns=common_columns_at)

# gap filled files
files = sorted(glob.glob(f"{paths[key]}DukeFACE_{var_key}*/*_gf.csv"))


# Define a custom sorting key function to extract the year from the file path
def extract_year(filepath):
    return int(filepath.split(f"{var_key}")[-1][:4])  # Year


# Sort the list of file paths based on the Year
sorted_filepaths = sorted(files, key=extract_year)


plots_cols = ["R1uat", "R2uat", "R3uat", "R4uat", "R5uat", "R6uat", "R7uat", "R8uat"]

selected_columns = plots_cols
for file in files:
    df_tmp = pd.read_csv(file)
    # Calculate the mean of selected columns
    df_tmp["Tair"] = round(df_tmp[selected_columns].mean(axis=1), 2)
    # only saving the common columns
    df_tmp = df_tmp[common_columns_at]
    # Appending all the common columns to the common dataframe
    df_AT_common_gf = df_AT_common_gf.append(df_tmp)
