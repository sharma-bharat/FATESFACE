# Plotting the +/- 20% Calibrated Simulation of RD and ECA
# option to add Obs as well

import argparse
import glob
import os

import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyreadr  # to read .rds files
import xarray as xr

path_in = "/Users/ud4/FATESMDS_analysis/outputs/runs/tests_alp/2024/"
path_rds = "/Users/ud4/Documents/FACEMDS/Models/obs_data"
# path_save_results = "/Users/ud4/Library/CloudStorage/GoogleDrive-bharat.sharma.neu@gmail.com/My Drive/Documents/PostDocORNL/Papers/Sharma_Walker_ORNL_Papers/Evaluation_Paper_Dir/Results"
path_save_results = "/Users/ud4/Library/CloudStorage/GoogleDrive-bharat.sharma.neu@gmail.com/My Drive/Documents/PostDocORNL/Papers/Sharma_Walker_ORNL_Papers/FATESResultsSharing/Results"


# Load the RDS file
# read mean annual
result = pyreadr.read_r(f"{path_rds}/OBS_ax.rds")
# Extract the object from the RDS file (result is a dictionary)
my_data = result[None]  # None is the key for the single R object in the file

# SE
result = pyreadr.read_r(f"{path_rds}/OBS_ase.rds")
my_data_se = result[None]  # None is the key for the single R object in the file

# plot obs?
obs_plot = 1  # 0,1

# global variables
sites = ("US-ORN", "US-DUK")
sim_cases = [
    "RD",
    "ECA",
    # "COnly",
]
run_cases = ["aCO2", "eCO2"]

parm_names = ["d2bl1", "Mbr_leaf", "Mbr_nonleaf", "store_ovrflw", "vmax_nh4"]


Best_Parms = {}
for site in sites:
    Best_Parms[site] = {}
    for sim_case in sim_cases:
        Best_Parms[site][sim_case] = {}

site = "US-ORN"
sim_case = "RD"
Best_Parms[site][sim_case][parm_names[0]] = 0.1272
Best_Parms[site][sim_case][parm_names[1]] = 7.0035e-06
Best_Parms[site][sim_case][parm_names[2]] = 1.6262e-06
Best_Parms[site][sim_case][parm_names[3]] = 10.4027
Best_Parms[site][sim_case][parm_names[4]] = 7.3969e-07

site = "US-ORN"
sim_case = "ECA"
Best_Parms[site][sim_case][parm_names[0]] = 0.0812
Best_Parms[site][sim_case][parm_names[1]] = 1.1875e-7
Best_Parms[site][sim_case][parm_names[2]] = 2.2262e-06
Best_Parms[site][sim_case][parm_names[3]] = 13.99
Best_Parms[site][sim_case][parm_names[4]] = 2.0057e-05

site = "US-DUK"
sim_case = "RD"
Best_Parms[site][sim_case][parm_names[0]] = 0.1122
Best_Parms[site][sim_case][parm_names[1]] = 5.6621e-06
Best_Parms[site][sim_case][parm_names[2]] = 1.7833e-06
Best_Parms[site][sim_case][parm_names[3]] = 11
Best_Parms[site][sim_case][parm_names[4]] = 8.9882e-07

site = "US-DUK"
sim_case = "ECA"
Best_Parms[site][sim_case][parm_names[0]] = 0.111
Best_Parms[site][sim_case][parm_names[1]] = 4.5873e-6
Best_Parms[site][sim_case][parm_names[2]] = 1.7492e-06
Best_Parms[site][sim_case][parm_names[3]] = 4.04
Best_Parms[site][sim_case][parm_names[4]] = 1.3041e-05
Best_Parms[site][sim_case][parm_names[3]] = 4.04
Best_Parms[site][sim_case][parm_names[4]] = 1.3041e-05

# Input
# Create argument parser
parser = argparse.ArgumentParser(
    description=f"Run the script with a specific parameter index {parm_names}."
)
parser.add_argument(
    "--parm_index", type=int, default=1, help="Set the parameter index (default: 1)"
)

args = parser.parse_args()

parameter_index = args.parm_index
# parameter_index = 1  # Input

# python paper_plot_ensemble_best_parm_NPPsims.py --parm_index 2

total_params_members = 125  # actual 125
ranges_pars = range(
    (parameter_index) * 125 + 1, (parameter_index) * 125 + total_params_members
)


case_id = "FACE_r250224_RDornl_enTune"
sim_case = "RD"
run_case = "aCO2"
l_fnames = {}


for site in sites:
    for sim_case in sim_cases:
        for run_case in run_cases:
            if (sim_case == "RD") and (site == "US-ORN"):
                case_id = "FACE_r250224_RDornl_enTune"
            elif (sim_case == "RD") and (site == "US-DUK"):
                case_id = "FACE_r250224_RDduke_enTune"
            elif (sim_case == "ECA") and (site == "US-ORN"):
                case_id = "FACE_r250224_ECAornl_enTune"
            elif (sim_case == "ECA") and (site == "US-DUK"):
                case_id = "FACE_r250224_ECAduke_enTune"
            for i in ranges_pars:
                # print (f"{path_in}{case_id}_{sim_case}_processed/{case_id}_{sim_case}_{site}_{run_case}_{'g{:05d}'.format(i)}.nc")
                l_fnames[f"{site}_{sim_case}_{run_case}_{'g{:05d}'.format(i)}"] = (
                    f"{path_in}{case_id}_{sim_case}_processed/{case_id}_{sim_case}_{site}_{run_case}_{'g{:05d}'.format(i)}.nc"
                )

ds = {}
for idx, key in enumerate(l_fnames.keys()):
    try:
        ds[key] = xr.open_mfdataset(l_fnames[key])
        # print("reading ... ", key)
    except:
        pass


markers_list = [".", "<", "o", "^", ".", "<", "o", "^"]
colors_list = ["red", "green", "blue", "yellow", "red", "green", "blue", "yellow"]


def tmp_legend_name(key):
    return key.replace("FACE_1PFT_r250210_Best_Cal_", "")


# How long do you want the simulation plots?
max_r_years = 150
if run_case == "spins":
    output_freq = "yearly"  # spins
    multiply_factor = 1  # 365 for daily output; else 1 for yearly
else:
    output_freq = "daily"  # spins
    multiply_factor = 365
    if "CO2" in run_case:
        max_r_years = 12 + 5


# dataframe of selected colms
# relevant columns
rel_cols = ["YEAR", "co2", "site", "NPP", "NUP"]
# if you want to save the filtered dataframe to CSV
# my_data[rel_cols].to_csv("tmp/obs_a.csv")
df_obs_ax = my_data[rel_cols]
df_obs_ase = my_data_se[rel_cols]

NPP_units = "gC m-2 y-1"
NUP_units = "gN m-2 y-1"

# Filter the dataset to keep only rows where site is 'ORNL' or 'DUKE'
df_obs_ax_site = df_obs_ax[df_obs_ax["site"].isin(["ORNL", "DUKE"])]
df_obs_ase_site = df_obs_ase[df_obs_ase["site"].isin(["ORNL", "DUKE"])]

filter_co2 = df_obs_ase_site["co2"] == "AMB"
filter_site = df_obs_ase_site["site"] == "ORNL"
df_obs_ase_site[filter_co2][filter_site]

# arrange data into dict!
dict_obs_ax = {}
sites_o = ("ORNL", "DUKE")
# sites_o= ( "ORNL", )

CO2_cases = ("AMB", "ELE")
dict_obs_ax = {}
dict_obs_ase = {}  # standard error
dict_obs_aserel = {}  # rel standard error = Val SE/Var
for site in sites_o:
    dict_obs_ax[site] = {}
    dict_obs_ase[site] = {}
    dict_obs_aserel[site] = {}
    for co2_case in CO2_cases:
        # selected values from mean
        filter_co2 = df_obs_ax_site["co2"] == co2_case
        filter_site = df_obs_ax_site["site"] == site
        dict_obs_ax[site][co2_case] = df_obs_ax_site[filter_co2][filter_site]
        # selected values from standard error
        filter_co2 = df_obs_ase_site["co2"] == co2_case
        filter_site = df_obs_ase_site["site"] == site
        dict_obs_ase[site][co2_case] = df_obs_ase_site[filter_co2][filter_site]
        dict_obs_aserel[site][co2_case] = df_obs_ase_site[filter_co2][
            filter_site
        ].copy()
        dict_obs_aserel[site][co2_case][["NPP", "NUP"]] = (
            df_obs_ase_site[filter_co2][filter_site][["NPP", "NUP"]]
            / df_obs_ax_site[filter_co2][filter_site][["NPP", "NUP"]]
        )


# Set background color
plt.rcParams["figure.facecolor"] = "#F5F5DC"  # Beige background for the entire figure
plt.rcParams["axes.facecolor"] = "#F5F5DC"  # Beige background for the plot area


vars_names = (  # "FATES_GPP",
    # "FATES_NPP",
    # "FATES_NPP-FATES_EXCESS_RESP",
    "NPP_alloc",
)

# ORNL
select_site = "US-ORN"
site_name = "ORNL"
# for site in sites:
if True:
    site = select_site
    for sim_case in sim_cases:
        # if True:
        # if sim_case == sim_cases[0]: continue
        co2_case_plotted = {"AMB": False, "ELE": False}
        plt.figure(figsize=(12, 8))
        for run_case in run_cases:
            for var in vars_names:
                for i in ranges_pars:
                    key = f"{site}_{sim_case}_{run_case}_{'g{:05d}'.format(i)}"
                    # if i ==15: break
                    if site == "US-ORN":
                        time = pd.date_range(
                            "1998-01-01", "2008-12-31", freq="A"
                        )  # Monthly data
                    else:
                        time = pd.date_range(
                            "1996-01-01", "2007-12-31", freq="A"
                        )  # Monthly data
                    if "aCO2" in key:
                        # color_line = "b"
                        cmap = plt.cm.Blues  # Choose a colormap
                        norm = mcolors.Normalize(
                            vmin=0, vmax=total_params_members
                        )  # Normalize color scale
                        # Assign gradient color based on index
                        color_line = cmap(norm(i))
                        marker_line = "o"
                        co2_case = "AMB"
                        tmp_case = "aCO2"
                        color_obs = "green"
                    if "eCO2" in key:
                        # color_line = "r"
                        cmap = plt.cm.Oranges  # Choose a colormap
                        norm = mcolors.Normalize(
                            vmin=0, vmax=total_params_members
                        )  # Normalize color scale
                        # Assign gradient color based on index
                        # color_line = cmap(norm(idx-total_params_members)) # hack to reset index of color range.
                        color_line = cmap(norm(i))
                        marker_line = ">"
                        co2_case = "ELE"
                        tmp_case = "eCO2"
                        color_obs = "purple"
                        ran_twice = 0  # Flag to track execution
                    if obs_plot == 1 and not co2_case_plotted[co2_case]:
                        Mean_DF = dict_obs_ax[site_name][co2_case]
                        Standard_error_DF = dict_obs_ase[site_name][co2_case]
                        # Extracting the relevant columns
                        years = Mean_DF["YEAR"]
                        mean_npp = Mean_DF["NPP"]
                        se_npp = Standard_error_DF["NPP"]
                        # Plot NPP with shaded error bars
                        plt.plot(
                            time,
                            mean_npp,
                            label=f"{tmp_case}_NPP_Obs",
                            color=color_obs,
                            lw=2,
                        )
                        plt.fill_between(
                            time,
                            mean_npp - se_npp,
                            mean_npp + se_npp,
                            label=f"{tmp_case}_NPP_Obs μ ± 1σ$_x̅$",
                            color=color_obs,
                            alpha=0.2,
                        )
                        plt.fill_between(
                            time,
                            mean_npp - se_npp * 1.96,
                            mean_npp + se_npp * 1.96,
                            color=color_obs,
                            alpha=0.15,
                        )
                        # ran_twice +=1
                        # Mark this CO2 case as plotted
                        co2_case_plotted[co2_case] = True
                    if var == "FATES_NPP-FATES_EXCESS_RESP":
                        tmp_var = (
                            (ds[key][var.split("-")[0]] - ds[key][var.split("-")[1]])
                            * 24
                            * 3600
                            * 365
                            * 1000
                        )
                        plt.plot(
                            time,
                            tmp_var.groupby("time.year").mean(),
                            label=tmp_legend_name(key).split("_")[-1]
                            + "_"
                            + "NPP-ExResp",
                            color=color_line,
                            marker=marker_line,
                            linestyle="--",
                            alpha=0.8,
                            linewidth=2,
                        )
                    if var == "NPP_alloc":
                        tmp_var = (
                            (
                                ds[key]["FATES_CROOT_ALLOC"]
                                + ds[key]["FATES_FROOT_ALLOC"]
                                + ds[key]["FATES_LEAF_ALLOC"]
                                + ds[key]["FATES_SEED_ALLOC"]
                                + ds[key]["FATES_STEM_ALLOC"]
                            )
                            * 24
                            * 3600
                            * 365
                            * 1000
                        )
                        plt.plot(
                            time,
                            tmp_var.groupby("time.year").mean(),
                            label=tmp_legend_name(key).split("_")[-1] + "_" + var,
                            color=color_line,
                            marker=marker_line,
                            alpha=0.25,
                            linewidth=2,
                        )
                        # print (key,color_line)
        plt.axhline(y=0, color="k", lw=2, alpha=0.02)
        plt.grid(
            True, linestyle="--", alpha=0.7
        )  # Dashed grid lines with better visibility
        # plt.legend(fontsize=14,loc="upper right")
        plt.ylabel(r"NPP [kg m$^{-2}$ yr$^{-1}$]", fontsize=14)
        plt.xticks(fontsize=14)  # Increase x-axis tick font size
        plt.yticks(fontsize=14)  # Increase y-axis tick font size
        plt.ylim(400, 1800)
        plt.title(
            f"Annual NPP for {select_site} under {sim_case} using +/- 20% Cal-A parms\n",
            fontsize=16,
        )

        # Add colorbar to indicate simulation index mapping
        sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
        sm.set_array([])
        cbar = plt.colorbar(sm)
        cbar.set_label(label=f"{parm_names[parameter_index]}", fontsize=14)
        # Define tick mapping
        original_ticks = np.linspace(
            0, total_params_members, 6
        )  # Example original tick positions (normalized scale)
        mean_parm_val = Best_Parms[site][sim_case][parm_names[parameter_index]]
        mapped_parm_values = np.linspace(mean_parm_val * 0.8, mean_parm_val * 1.2, 6)

        # Apply mapping
        cbar.set_ticks(original_ticks)
        cbar.set_ticklabels(mapped_parm_values)
        cbar.ax.tick_params(labelsize=12)
        print(
            f"{path_save_results}/test_NPP_Best_Parms_{parm_names[parameter_index]}_20p_{select_site}_{sim_case}.pdf"
        )
        plt.savefig(
            f"{path_save_results}/test_NPP_Best_Parms_{parm_names[parameter_index]}_20p_{select_site}_{sim_case}.pdf",
            format="pdf",
            bbox_inches="tight",
        )
## Duke

select_site = "US-DUK"
site_name = "DUKE"
# for site in sites:
if True:
    site = select_site
    for sim_case in sim_cases:
        # if True:
        # if sim_case == sim_cases[0]: continue
        co2_case_plotted = {"AMB": False, "ELE": False}
        plt.figure(figsize=(12, 8))
        for run_case in run_cases:
            for var in vars_names:
                for i in ranges_pars:
                    key = f"{site}_{sim_case}_{run_case}_{'g{:05d}'.format(i)}"
                    # if i ==15: break
                    if site == "US-ORN":
                        time = pd.date_range(
                            "1998-01-01", "2008-12-31", freq="A"
                        )  # Monthly data
                    else:
                        time = pd.date_range(
                            "1996-01-01", "2007-12-31", freq="A"
                        )  # Monthly data
                    if "aCO2" in key:
                        # color_line = "b"
                        cmap = plt.cm.Blues  # Choose a colormap
                        norm = mcolors.Normalize(
                            vmin=0, vmax=total_params_members
                        )  # Normalize color scale
                        # Assign gradient color based on index
                        color_line = cmap(norm(i))
                        marker_line = "o"
                        co2_case = "AMB"
                        tmp_case = "aCO2"
                        color_obs = "green"
                    if "eCO2" in key:
                        # color_line = "r"
                        cmap = plt.cm.Oranges  # Choose a colormap
                        norm = mcolors.Normalize(
                            vmin=0, vmax=total_params_members
                        )  # Normalize color scale
                        # Assign gradient color based on index
                        # color_line = cmap(norm(idx-total_params_members)) # hack to reset index of color range.
                        color_line = cmap(norm(i))
                        marker_line = ">"
                        co2_case = "ELE"
                        tmp_case = "eCO2"
                        color_obs = "purple"
                        ran_twice = 0  # Flag to track execution
                    if obs_plot == 1 and not co2_case_plotted[co2_case]:
                        Mean_DF = dict_obs_ax[site_name][co2_case]
                        Standard_error_DF = dict_obs_ase[site_name][co2_case]
                        # Extracting the relevant columns
                        years = Mean_DF["YEAR"]
                        mean_npp = Mean_DF["NPP"]
                        se_npp = Standard_error_DF["NPP"]
                        # Plot NPP with shaded error bars
                        plt.plot(
                            time,
                            mean_npp,
                            label=f"{tmp_case}_NPP_Obs",
                            color=color_obs,
                            lw=2,
                        )
                        plt.fill_between(
                            time,
                            mean_npp - se_npp,
                            mean_npp + se_npp,
                            label=f"{tmp_case}_NPP_Obs μ ± 1σ$_x̅$",
                            color=color_obs,
                            alpha=0.2,
                        )
                        plt.fill_between(
                            time,
                            mean_npp - se_npp * 1.96,
                            mean_npp + se_npp * 1.96,
                            color=color_obs,
                            alpha=0.15,
                        )
                        # ran_twice +=1
                        # Mark this CO2 case as plotted
                        co2_case_plotted[co2_case] = True
                    if var == "FATES_NPP-FATES_EXCESS_RESP":
                        tmp_var = (
                            (ds[key][var.split("-")[0]] - ds[key][var.split("-")[1]])
                            * 24
                            * 3600
                            * 365
                            * 1000
                        )
                        plt.plot(
                            time,
                            tmp_var.groupby("time.year").mean(),
                            label=tmp_legend_name(key).split("_")[-1]
                            + "_"
                            + "NPP-ExResp",
                            color=color_line,
                            marker=marker_line,
                            linestyle="--",
                            alpha=0.8,
                            linewidth=2,
                        )
                    if var == "NPP_alloc":
                        tmp_var = (
                            (
                                ds[key]["FATES_CROOT_ALLOC"]
                                + ds[key]["FATES_FROOT_ALLOC"]
                                + ds[key]["FATES_LEAF_ALLOC"]
                                + ds[key]["FATES_SEED_ALLOC"]
                                + ds[key]["FATES_STEM_ALLOC"]
                            )
                            * 24
                            * 3600
                            * 365
                            * 1000
                        )
                        plt.plot(
                            time,
                            tmp_var.groupby("time.year").mean(),
                            label=tmp_legend_name(key).split("_")[-1] + "_" + var,
                            color=color_line,
                            marker=marker_line,
                            alpha=0.25,
                            linewidth=2,
                        )
                        # print (key,color_line)
        plt.axhline(y=0, color="k", lw=2, alpha=0.02)
        plt.grid(
            True, linestyle="--", alpha=0.7
        )  # Dashed grid lines with better visibility
        # plt.legend(fontsize=14,loc="upper right")
        plt.ylabel(r"NPP [kg m$^{-2}$ yr$^{-1}$]", fontsize=14)
        plt.xticks(fontsize=14)  # Increase x-axis tick font size
        plt.yticks(fontsize=14)  # Increase y-axis tick font size
        plt.ylim(400, 1800)
        plt.title(
            f"Annual NPP for {select_site} under {sim_case} using +/- 20% Cal-A parms\n",
            fontsize=16,
        )

        # Add colorbar to indicate simulation index mapping
        sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
        sm.set_array([])
        cbar = plt.colorbar(sm)
        cbar.set_label(label=f"{parm_names[parameter_index]}", fontsize=14)
        # Define tick mapping
        original_ticks = np.linspace(
            0, total_params_members, 6
        )  # Example original tick positions (normalized scale)
        mean_parm_val = Best_Parms[site][sim_case][parm_names[parameter_index]]
        mapped_parm_values = np.round(
            np.linspace(mean_parm_val * 0.8, mean_parm_val * 1.2, 6), 4
        )

        # Apply mapping
        cbar.set_ticks(original_ticks)
        cbar.set_ticklabels(mapped_parm_values)
        cbar.ax.tick_params(labelsize=12)
        print(
            f"{path_save_results}/test_NPP_Best_Parms_{parm_names[parameter_index]}_20p_{select_site}_{sim_case}.pdf"
        )
        plt.savefig(
            f"{path_save_results}/test_NPP_Best_Parms_{parm_names[parameter_index]}_20p_{select_site}_{sim_case}.pdf",
            format="pdf",
            bbox_inches="tight",
        )
