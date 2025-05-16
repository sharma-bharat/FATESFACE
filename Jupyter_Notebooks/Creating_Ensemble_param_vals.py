import pandas as pd
import itertools

# load the file with param values
excel_file = 'tmp/FinalRD_Calibration_Ranges_ORNL.xlsx'
fname_save = 'tmp/combinations_RD_ORNL_20250117.csv'
df = pd.read_excel(excel_file)

#only copy the head
random_combinations = df.head(0).copy (deep = True)

# Randomizing one col at a time
for i in range(5):
    for idx in range (5):
        df[df.columns[idx]] = df[df.columns[idx]].sample(frac=1).reset_index(drop=True)
    random_combinations = random_combinations.append(df)
random_combinations.reset_index(inplace=True,drop=True)    

# adding fates_cnp_vmax_no3, fates_cnp_vmax_p
df_comb = pd.DataFrame(random_combinations, columns=df.columns)
df_comb["fates_cnp_vmax_no3"] = df_comb["fates_cnp_vmax_nh4"]
df_comb["fates_cnp_vmax_p"] = df_comb["fates_cnp_vmax_nh4"]/10


df_comb.to_csv(f'{fname_save}', index=False)

print ("\n")

print ("Minimum values of parameters")
print (random_combinations.min())

print ("\n")

print ("Maximum values of parameters")
print (random_combinations.max())
