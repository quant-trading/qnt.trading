results = [
    ("Alice", 85), 
    ("Bob", 70), 
    ("Alice", 92),  # Duplicate Alice (Higher)
    ("Charlie", 88), 
    ("Bob", 65)     # Duplicate Bob (Lower)
]

# Step 1: Get a unique list of names using a set
names = {name for name, score in results}

# Step 2: Use list comprehension with a conditional 'max' logic
# For every unique name, find the highest score associated with it
best_results = [
    (name, max(score for student, score in results if student == name))
    for name in names
]

print(f"Original Results: {results}")
print(f"Cleaned (Best) Results: {best_results}")


######

scores = [10, 20, 30, 40, 50]

# Calculate the bounds using built-ins [cite: 208]
min_val = min(scores)
max_val = max(scores)

# Apply formula using a list comprehension 
normalized_scores = [(x - min_val) / (max_val - min_val) for x in scores]

# Formatting the output with f-strings for precision [cite: 203]
formatted_scores = [f"{n:.2f}" for n in normalized_scores]
print(f"Normalized Data (0 to 1 scale): {formatted_scores}")
