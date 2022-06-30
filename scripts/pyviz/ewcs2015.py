import pandas as pd
pd.options.mode.chained_assignment = None

# Arguments
only_french = True
file_output = "../../includes/ewcs2015_short" + "_fr.csv" if only_french else ".csv"
full_sat = False
remove_eijqi_factor = True

row_data = pd.read_csv('../../UKDA-8098-tab/tab/ewcs6_2015_ukda_1904.tab', sep='\t')
print(row_data.shape)

# filter only people that actually work as employees
data = row_data[(row_data['Q2c'] == 1) & (row_data['Q7'] == 1)]
print(data.shape)

# French filter
filtered_data: pd.DataFrame

if only_french:
    filtered_data = data[data['Country'] == 10]
    print(filtered_data.shape)
else:
    filtered_data = data

# ---------------------------
# Filtered specific data to export

data_filter_details = [
    "Household Size", "Gender", # M=1 F=2
    "Age", "Part/Full time", # Part=1 Full=2
    "Contract type", # Unlimited=1 Limited=2,3,4
    "Number of hour", # per week
    "Earning", # euros
    "Education", # 3=collège 4=lycée 5=bac 6=licence 7=master
    "Satisfaction", # regarding work conditions - 1=very sat 2=sat 3=not very sat 4=not at all sat
    "Autonomy", # Define objectives, own ideas, influence decision
    "Interaction", # helpful colleagues, supportive boss, cooperation
    "Intensity", # high speed, tight deadlines
    "Meaningful" # useful work, job well done
]

data_filter = ['Q1', 'Q2a', 'Q2b', 'Q2d', 'Q11', 'Q24', 'ISCED', 'Q104_euro',
               'Q88',
               'Q61c', 'Q61i', 'Q61n',
               'Q61a', 'Q63d', 'Q70e',
               'Q49a', 'Q49b',
               'Q61j', 'Q61h']

filtered_data = filtered_data[data_filter]
print(filtered_data.shape)

# Filter missing data
filtered_data = filtered_data.replace(r'^s*$', float('NaN'), regex=True)
filtered_data.dropna()

# Remove nodata
filtered_data = filtered_data[~filtered_data['Q2b'].isin([888, 999]) ]
filtered_data = filtered_data[~filtered_data['Q11'].isin(['5', '6', '8', '9'])]
filtered_data = filtered_data[~filtered_data['Q24'].isin([888, 999])]
filtered_data = filtered_data[~filtered_data['Q88'].isin([8, 9])]
filtered_data = filtered_data[~filtered_data['Q104_euro'].isin([88888888.0, 99999999.0])]

for l5 in ['Q61c', 'Q61i', 'Q61n', 'Q61a', 'Q63d', 'Q70e', 'Q61j', 'Q61h']:
    filtered_data = filtered_data[~filtered_data[l5].isin([7, 8, 9, ' '])]

for l7 in ['Q49a', 'Q49b']:
    filtered_data = filtered_data[~filtered_data[l7].isin([8, 9, ' '])]

if full_sat:
    filtered_data = filtered_data[~filtered_data['Q88'].isin([8, 9])]

# -----------
# Recode data

filtered_data['Q2a'].replace({1: 'M', 2: 'F'}, inplace=True)
filtered_data['Q2d'].replace({'1': 'Part-time', '2': 'Full-time'}, inplace=True)
filtered_data['ISCED'].replace(
    {1: 'No education completed',
     2: 'Primary or lower secondary education',
     3: 'Primary or lower secondary education',
     4: 'Upper secondary or post-secondary education',
     5: 'Upper secondary or post-secondary education',
     6: 'Tertiary education',
     7: 'Tertiary education',
     8: 'Tertiary education',
     9: 'Tertiary education',
     88: 'No education completed',
     99: 'No education completed'}, inplace=True)
filtered_data['Q11'].replace(
    {'1': 'long term',
     '2': 'short term',
     '3': 'short term',
     '4': 'short term'}, inplace=True
)

# -----------------------------------------
# Transpose raw data to 4 dimensional EIJQI

# Reverse scores
for l5 in ['Q61c', 'Q61i', 'Q61n', 'Q61a', 'Q63d', 'Q70e', 'Q61j', 'Q61h']:
    filtered_data[l5].replace({5: 1, 4: 2, 2: 4, 1: 5}, inplace=True)

# Build indexes
filtered_data['autonomy'] = filtered_data[['Q61c', 'Q61i', 'Q61n']].sum(axis=1)
filtered_data['interaction'] = filtered_data[['Q61a', 'Q63d', 'Q70e']].sum(axis=1)
filtered_data['intensity'] = filtered_data[['Q49a', 'Q49b']].sum(axis=1)
filtered_data['meaningful'] = filtered_data[['Q61j', 'Q61h']].sum(axis=1)

if remove_eijqi_factor:
    filtered_data = filtered_data.drop(['Q61c', 'Q61i', 'Q61n', 'Q61a', 'Q63d', 'Q70e', 'Q49a', 'Q49b', 'Q61j', 'Q61h'],
                                       axis=1)

filtered_data.to_csv("../../includes/ewcs2015_short_fr.csv")
