# Plotting the final Calibrated Simulation of RD and ECA
# option to add Obs as well

import glob
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyreadr  # to read .rds files
import xarray as xr

path_in = "/Users/ud4/FATESMDS_analysis/outputs/runs/tests_alp/2024/"
path_rds = "/Users/ud4/Documents/FACEMDS/Models/obs_data"
path_save_results = "/Users/ud4/Library/CloudStorage/GoogleDrive-bharat.sharma.neu@gmail.com/My Drive/Documents/PostDocORNL/Papers/Sharma_Walker_ORNL_Papers/FATESResultsSharing/Results"

# Load the RDS file Obs
# read mean annual
result = pyreadr.read_r(f"{path_rds}/OBS_ax.rds")
# Extract the object from the RDS file (result is a dictionary)
my_data = result[None]  # None is the key for the single R object in the file

# SE
result = pyreadr.read_r(f"{path_rds}/OBS_ase.rds")
my_data_se = result[None]  # None is the key for the single R object in the file

# plot obs?
obs_plot = 1  # 0,1


fnames = {}

case_id = "FACE_1PFT_r250210_Best_Cal"

case_ids = [
    "FACE_1PFT_r250210_Best_Cal",  #
]

sites = ("US-ORN", "US-DUK")
run_cases = ["aCO2", "eCO2"]
sim_cases = [
    "RD",
    "ECA",
    # "COnly",
]

for case_id in case_ids:
    for site in sites:
        for run_case in run_cases:
            # COnly
            # fnames[f"{case_id}_{site}_COnly_{run_case}"] = f"{path_in}{case_id}_processed/{case_id}_{site}_{run_case}.nc"
            # RD
            # fn = ['RD','ECA']
            for sim_case in sim_cases:
                fnames[f"{case_id}_{site}_{sim_case}_{run_case}"] = (
                    f"{path_in}{case_id}_{sim_case}_processed/{case_id}_{sim_case}_{site}_{run_case}.nc"
                )
ds = {}
for idx, key in enumerate(fnames.keys()):
    print(key, ":", fnames[key])
    ds[key] = xr.open_mfdataset(fnames[key])


def tmp_legend_name(key):
    return key.replace("FACE_1PFT_r250210_Best_Cal_", "")


## Obs
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


# Plots NPP for models and obs

vars = (  # "FATES_GPP",
    # "FATES_NPP",
    # "FATES_NPP-FATES_EXCESS_RESP",
    # "NPP_alloc",
    "NUP",
)

# NPP for ORNL
select_site = "US-ORN"
site = "ORNL"

for sim_case in sim_cases:
    ran_twice = 0  # Flag to track execution
    plt.figure(figsize=(12, 8))
    print("fig start", sim_case)
    for var in vars:
        for idx, key in enumerate(ds.keys()):
            if sim_case not in key:
                continue
            if f"{select_site}" in key:
                time = pd.date_range("1998-01-01", "2008-12-31", freq="A")
                if "aCO2" in key:
                    color_line = "b"
                    marker_line = "o"
                    co2_case = "AMB"
                    tmp_case = "aCO2"
                    color_obs = "green"
                if "eCO2" in key:
                    color_line = "r"
                    marker_line = ">"
                    co2_case = "ELE"
                    tmp_case = "eCO2"
                    color_obs = "purple"
                if (obs_plot == 1) and (ran_twice < 2):
                    Mean_DF = dict_obs_ax[site][co2_case]
                    Standard_error_DF = dict_obs_ase[site][co2_case]
                    # Extracting the relevant columns
                    years = Mean_DF["YEAR"]
                    mean_npp = Mean_DF["NUP"]
                    se_npp = Standard_error_DF["NUP"]
                    # Plot NPP with shaded error bars
                    plt.plot(
                        time,
                        mean_npp,
                        label=f"{tmp_case}_NUP_Obs",
                        color=color_obs,
                        lw=2,
                    )
                    plt.fill_between(
                        time,
                        mean_npp - se_npp,
                        mean_npp + se_npp,
                        label=f"{tmp_case}_NUP_Obs μ ± 1σ$_x̅$",
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
                    ran_twice += 1

                    if var == "NUP":
                        tmp_var = (
                            (ds[key]["FATES_NH4UPTAKE"] + ds[key]["FATES_NO3UPTAKE"])
                            * 24
                            * 3600
                            * 365
                            * 1000
                        )
                    plt.plot(
                        time,
                        tmp_var.groupby("time.year").mean(),
                        label=tmp_legend_name(key).split("_")[-1] + "_" + "NUP",
                        color=color_line,
                        marker=marker_line,
                        linestyle="--",
                        alpha=0.8,
                        linewidth=2,
                    )
                plt.axhline(y=0, color="k", lw=2, alpha=0.02)
                plt.grid(
                    True, linestyle="--", alpha=0.7
                )  # Dashed grid lines with better visibility
                plt.legend(fontsize=14, loc="upper right")
                plt.ylabel(r"NUP [kg m$^{-2}$ yr$^{-1}$]", fontsize=14)
                plt.xticks(fontsize=14)  # Increase x-axis tick font size
                plt.yticks(fontsize=14)  # Increase y-axis tick font size
                plt.ylim(2, 16)
                plt.title(
                    f"Annual NUP for {select_site} under {sim_case} using Cal-A parms\n",
                    fontsize=16,
                )
    print("fig end", sim_case, select_site)
    print(f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}.pdf")
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}.pdf",
        format="pdf",
        bbox_inches="tight",
    )
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}.png",
        format="png",
        dpi=200,
        bbox_inches="tight",
    )

# NPP for Duke

select_site = "US-DUK"
site = "DUKE"

for sim_case in sim_cases:
    ran_twice = 0  # Flag to track execution
    plt.figure(figsize=(12, 8))
    print("fig start", sim_case)
    for var in vars:
        for idx, key in enumerate(ds.keys()):
            if sim_case not in key:
                continue
            if f"{select_site}" in key:
                time = pd.date_range("1996-01-01", "2007-12-31", freq="A")
                if "aCO2" in key:
                    color_line = "b"
                    marker_line = "o"
                    co2_case = "AMB"
                    tmp_case = "aCO2"
                    color_obs = "green"
                if "eCO2" in key:
                    color_line = "r"
                    marker_line = ">"
                    co2_case = "ELE"
                    tmp_case = "eCO2"
                    color_obs = "purple"
                if (obs_plot == 1) and (ran_twice < 2):
                    Mean_DF = dict_obs_ax[site][co2_case]
                    Standard_error_DF = dict_obs_ase[site][co2_case]
                    # Extracting the relevant columns
                    years = Mean_DF["YEAR"]
                    mean_npp = Mean_DF["NUP"]
                    se_npp = Standard_error_DF["NUP"]
                    # Plot NPP with shaded error bars
                    plt.plot(
                        time,
                        mean_npp,
                        label=f"{tmp_case}_NUP_Obs",
                        color=color_obs,
                        lw=2,
                    )
                    plt.fill_between(
                        time,
                        mean_npp - se_npp,
                        mean_npp + se_npp,
                        label=f"{tmp_case}_NUP_Obs μ ± 1σ$_x̅$",
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
                    ran_twice += 1

                if var == "NUP":
                    tmp_var = (
                        (ds[key]["FATES_NH4UPTAKE"] + ds[key]["FATES_NO3UPTAKE"])
                        * 24
                        * 3600
                        * 365
                        * 1000
                    )
                plt.plot(
                    time,
                    tmp_var.groupby("time.year").mean(),
                    label=tmp_legend_name(key).split("_")[-1] + "_" + "NUP",
                    color=color_line,
                    marker=marker_line,
                    linestyle="--",
                    alpha=0.8,
                    linewidth=2,
                )
                plt.axhline(y=0, color="k", lw=2, alpha=0.02)
                plt.grid(
                    True, linestyle="--", alpha=0.7
                )  # Dashed grid lines with better visibility
                plt.legend(fontsize=14, loc="upper center", ncol=2)
                plt.ylabel(r"NUP [kg m$^{-2}$ yr$^{-1}$]", fontsize=14)
                plt.xticks(fontsize=14)  # Increase x-axis tick font size
                plt.yticks(fontsize=14)  # Increase y-axis tick font size
                plt.ylim(2, 16)
                plt.title(
                    f"Annual NUP for {select_site} under {sim_case} using Cal-A parms\n",
                    fontsize=16,
                )
                # li+=1 # for legend index updating for a site
    print("fig end", sim_case, select_site)
    print(f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}.pdf")
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}.pdf",
        format="pdf",
        bbox_inches="tight",
    )
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}.png",
        format="png",
        dpi=200,
        bbox_inches="tight",
    )

# Responses for ORNL

select_site = "US-ORN"
site = "ORNL"
li = 0  # for legend index
for sim_case in sim_cases:
    ran_once = False
    plt.figure(figsize=(12, 6))
    for var in vars:
        for idx, key in enumerate(ds.keys()):
            if sim_case not in key:
                continue
            if f"{select_site}" in key:
                time = pd.date_range(
                    "1998-01-01", "2008-12-31", freq="A"
                )  # Monthly data
                color_obs = "k"

                if (obs_plot == 1) and (not ran_once):
                    Mean_DF1 = dict_obs_ax[site][CO2_cases[1]].reset_index(drop=True)
                    Mean_DF0 = dict_obs_ax[site][CO2_cases[0]].reset_index(drop=True)

                    Mean_DF = (
                        Mean_DF1[["YEAR", "NPP", "NUP"]]
                        - Mean_DF0[["YEAR", "NPP", "NUP"]]
                    )
                    Mean_DF["YEAR"] = Mean_DF1["YEAR"]

                    Standard_error_DF1 = dict_obs_ase[site][CO2_cases[1]].reset_index(
                        drop=True
                    )
                    Standard_error_DF0 = dict_obs_ase[site][CO2_cases[0]].reset_index(
                        drop=True
                    )

                    Standard_error_DF = (
                        (Standard_error_DF1[["YEAR", "NPP", "NUP"]]) ** 2
                        + (Standard_error_DF0[["YEAR", "NPP", "NUP"]]) ** 2
                    ) ** 0.5
                    Standard_error_DF["YEAR"] = Standard_error_DF1["YEAR"]

                    # Extracting the relevant columns
                    years = Mean_DF["YEAR"]
                    mean_npp = Mean_DF["NUP"]
                    se_npp = Standard_error_DF["NUP"]
                    # Plot NPP with shaded error bars
                    plt.plot(
                        time,
                        mean_npp,
                        label=f"Response_NUP_Obs",
                        color=color_obs,
                        lw=2,
                    )
                    plt.fill_between(
                        time,
                        mean_npp - se_npp,
                        mean_npp + se_npp,
                        label=f"Response_NUP_Obs μ ± 1σ$_x̅$",
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

                    ran_once = True

                if "aCO2" in key:
                    color_line = "blue"
                    marker_line = "^"
                if "eCO2" in key:
                    color_line = "gray"
                    marker_line = "^"
                    continue
                if var == "NUP":
                    tmp_var_a = (
                        (ds[key]["FATES_NH4UPTAKE"] + ds[key]["FATES_NO3UPTAKE"])
                        * 24
                        * 3600
                        * 365
                        * 1000
                    )
                    key_eCO2 = key.replace("aCO2", "eCO2")
                    tmp_var_e = (
                        (
                            ds[key_eCO2]["FATES_NH4UPTAKE"]
                            + ds[key_eCO2]["FATES_NO3UPTAKE"]
                        )
                        * 24
                        * 3600
                        * 365
                        * 1000
                    )
                    plt.plot(
                        time,
                        (tmp_var_e - tmp_var_a).groupby("time.year").mean(),
                        label=tmp_legend_name(key)
                        .split("_")[-1]
                        .replace("aCO2", "Response")
                        + "_"
                        + var,
                        color=color_line,
                        marker=marker_line,
                        alpha=0.8,
                        linewidth=2,
                    )
                plt.axhline(y=0, color="k", lw=2, alpha=0.02)
                plt.grid(
                    True, linestyle="--", alpha=0.7
                )  # Dashed grid lines with better visibility
                plt.legend(fontsize=14)
                plt.ylabel(r"NUP Response [kg m$^{-2}$ yr$^{-1}$]", fontsize=14)
                plt.xticks(fontsize=14)  # Increase x-axis tick font size
                plt.yticks(fontsize=14)  # Increase y-axis tick font size
                plt.ylim(-4, 10)
                plt.title(
                    f"Annual NUP Response for {select_site} under {sim_case} using Cal-A parms\n",
                    fontsize=16,
                )
        # >>> Saving the data frame
        columns = [
            "Time",
            "Mean_NUP",
            "Lower_Bound",
            "Upper_Bound",
            "CNP_Mode",
        ]

        data_array = np.ma.zeros((len(time), len(columns)))
        ## time
        data_array[:, 0] = np.array([t.year for t in time])
        ## others
        data_array[:, 1] = np.array(mean_npp)
        data_array[:, 2] = np.array(mean_npp - se_npp)
        data_array[:, 3] = np.array(mean_npp + se_npp)
        data_array[:, 4] = (
            (tmp_var_e - tmp_var_a).groupby("time.year").mean().values[:, 0]
        )
        # Convert to DataFrame
        df = pd.DataFrame(data_array, columns=columns)
        # print (df)
        df.to_csv(
            f"{path_save_results}/DF_NUP_response_Cal_{select_site}_{sim_case}_{var}.csv",
            index=False,
        )
        # <<<  Saving the dataframe
    print(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}_Response.pdf"
    )
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}_Response.pdf",
        format="pdf",
        bbox_inches="tight",
    )
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}_Response.png",
        format="png",
        dpi=200,
        bbox_inches="tight",
    )

# Responses for DUKE

select_site = "US-DUK"
site = "DUKE"
li = 0  # for legend index
for sim_case in sim_cases:
    ran_once = False
    plt.figure(figsize=(12, 6))
    for var in vars:
        for idx, key in enumerate(ds.keys()):
            if sim_case not in key:
                continue
            if f"{select_site}" in key:
                time = pd.date_range(
                    "1996-01-01", "2007-12-31", freq="A"
                )  # Monthly data
                color_obs = "k"

                if (obs_plot == 1) and (not ran_once):
                    Mean_DF1 = dict_obs_ax[site][CO2_cases[1]].reset_index(drop=True)
                    Mean_DF0 = dict_obs_ax[site][CO2_cases[0]].reset_index(drop=True)

                    Mean_DF = (
                        Mean_DF1[["YEAR", "NPP", "NUP"]]
                        - Mean_DF0[["YEAR", "NPP", "NUP"]]
                    )
                    Mean_DF["YEAR"] = Mean_DF1["YEAR"]

                    Standard_error_DF1 = dict_obs_ase[site][CO2_cases[1]].reset_index(
                        drop=True
                    )
                    Standard_error_DF0 = dict_obs_ase[site][CO2_cases[0]].reset_index(
                        drop=True
                    )

                    Standard_error_DF = (
                        (Standard_error_DF1[["YEAR", "NPP", "NUP"]]) ** 2
                        + (Standard_error_DF0[["YEAR", "NPP", "NUP"]]) ** 2
                    ) ** 0.5
                    Standard_error_DF["YEAR"] = Standard_error_DF1["YEAR"]

                    # Extracting the relevant columns
                    years = Mean_DF["YEAR"]
                    mean_npp = Mean_DF["NUP"]
                    se_npp = Standard_error_DF["NUP"]
                    # Plot NPP with shaded error bars
                    plt.plot(
                        time,
                        mean_npp,
                        label=f"Response_NUP_Obs",
                        color=color_obs,
                        lw=2,
                    )
                    plt.fill_between(
                        time,
                        mean_npp - se_npp,
                        mean_npp + se_npp,
                        label=f"Response_NPP_Obs μ ± 1σ$_x̅$",
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

                    ran_once = True

                if "aCO2" in key:
                    color_line = "blue"
                    marker_line = "^"
                if "eCO2" in key:
                    color_line = "gray"
                    marker_line = "^"
                    continue
                if var == "NUP":
                    tmp_var_a = (
                        (ds[key]["FATES_NH4UPTAKE"] + ds[key]["FATES_NO3UPTAKE"])
                        * 24
                        * 3600
                        * 365
                        * 1000
                    )
                    key_eCO2 = key.replace("aCO2", "eCO2")
                    tmp_var_e = (
                        (
                            ds[key_eCO2]["FATES_NH4UPTAKE"]
                            + ds[key_eCO2]["FATES_NO3UPTAKE"]
                        )
                        * 24
                        * 3600
                        * 365
                        * 1000
                    )
                    plt.plot(
                        time,
                        (tmp_var_e - tmp_var_a).groupby("time.year").mean(),
                        label=tmp_legend_name(key)
                        .split("_")[-1]
                        .replace("aCO2", "Response")
                        + "_"
                        + var,
                        color=color_line,
                        marker=marker_line,
                        alpha=0.8,
                        linewidth=2,
                    )
                plt.axhline(y=0, color="k", lw=2, alpha=0.02)
                plt.grid(
                    True, linestyle="--", alpha=0.7
                )  # Dashed grid lines with better visibility
                plt.legend(fontsize=14, loc="upper center", ncol=2)
                plt.ylabel(r"NUP Response [kg m$^{-2}$ yr$^{-1}$]", fontsize=14)
                plt.xticks(fontsize=14)  # Increase x-axis tick font size
                plt.yticks(fontsize=14)  # Increase y-axis tick font size
                plt.ylim(-2, 8)
                plt.title(
                    f"Annual NUP Response for {select_site} under {sim_case} using Cal-A parms\n",
                    fontsize=16,
                )
                # li+=1 # for legend index updating for a site
        # >>> Saving the data frame
        columns = [
            "Time",
            "Mean_NUP",
            "Lower_Bound",
            "Upper_Bound",
            "CNP_Mode",
        ]

        data_array = np.ma.zeros((len(time), len(columns)))
        ## time
        data_array[:, 0] = np.array([t.year for t in time])
        ## others
        data_array[:, 1] = np.array(mean_npp)
        data_array[:, 2] = np.array(mean_npp - se_npp)
        data_array[:, 3] = np.array(mean_npp + se_npp)
        data_array[:, 4] = (
            (tmp_var_e - tmp_var_a).groupby("time.year").mean().values[:, 0]
        )
        # Convert to DataFrame
        df = pd.DataFrame(data_array, columns=columns)
        # print (df)
        df.to_csv(
            f"{path_save_results}/DF_NUP_response_Cal_{select_site}_{sim_case}_{var}.csv",
            index=False,
        )
        # <<<  Saving the dataframe
    print(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}_Response.pdf"
    )
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}_Response.pdf",
        format="pdf",
        bbox_inches="tight",
    )
    plt.savefig(
        f"{path_save_results}/NUP_Best_Parms_Cal_{select_site}_{sim_case}_Response.png",
        format="png",
        dpi=200,
        bbox_inches="tight",
    )
