import pandas as pd

# Load the Excel file
excel_file = "combinations_RD_ORNL.xlsx"  # Replace with your file name

# Read the Excel sheet (replace 'Sheet1' if you have a different sheet name)
df = pd.read_excel(excel_file, sheet_name="Sheet1")

# Save to a space-separated text file with float formatting
df.to_csv("combinations_RD_ORNL.txt", sep=" ", index=False, float_format="%.3f")

