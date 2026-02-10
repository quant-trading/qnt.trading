import numpy as np

# Data: Hours Studied vs. Exam Score
hours = [2, 10, 5, 8, 1]
scores = [55, 95, 68, 88, 40]

# Compute correlation matrix
matrix = np.corrcoef(hours, scores)

# Extract the specific coefficient between the two variables
correlation = matrix[0, 1]

print(f"Correlation Coefficient: {correlation:.2f}")
# Output: ~0.99 (Strong positive correlation)

###################

# Dataset with one clear outlier (500)
data = [10, 12, 11, 13, 500, 12, 14]
arr = np.array(data)

# Method 1: Simple Threshold (e.g., anything over 100)
outliers_threshold = [x for x in data if x > 100]

# Method 2: Standard Deviation (Simple heuristic)
mean = np.mean(arr)
std = np.std(arr)
limit = mean + (2 * std)

outliers_std = arr[arr > limit]

print(f"Mean: {mean:.2f}, Threshold Limit: {limit:.2f}")
print(f"Detected Outliers: {outliers_std}")

# Create a mask of 'True' for values within the normal range
# Let's say we only want values less than 100
clean_data = arr[arr < 100]

print(f"Original Data: {arr}")
print(f"Cleaned Data: {clean_data}")
