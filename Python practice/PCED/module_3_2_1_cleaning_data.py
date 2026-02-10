# Raw data from a file might look like this
raw_scores = [85, None, 92, "", 78, "N/A"]

# Using list comprehension to identify indices of missing data 
missing_indices = [i for i, val in enumerate(raw_scores) if val is None or val == ""]

print(f"Missing data found at indices: {missing_indices}")

cleaned_data = [val for val in raw_scores if val is not None and val != ""]

# Strategy 2: Replace missing values with 0 
imputed_data = [val if (val is not None and val != "") else 0 for val in raw_scores]

print(f"Cleaned: {cleaned_data}")
print(f"Imputed: {imputed_data}")

################

ages = [25, -5, "thirty", 40, 150]
valid_ages = []

for age in ages:
    # Check for invalid type 
    if not isinstance(age, int):
        print(f"Skipping invalid type: {age}")
        continue
    
    # Check for out-of-range values (Logical validation) 
    if age < 0 or age > 120:
        print(f"Skipping impossible age: {age}")
    else:
        valid_ages.append(age)

print(f"Final validated list: {valid_ages}")
