# 1. CREATION
# Representing a single observation or record
record = {
    "emp_id": 101,
    "name": "Sarah",
    "department": "IT",
    "salary": 75000.0
}

# 2. ACCESSING DATA
# Method A: Direct access (raises KeyError if key is missing)
print(f"Employee Name: {record['name']}") 

# Method B: The .get() method (Safer - returns None or a default value)
bonus = record.get("bonus", 0) # Returns 0 because "bonus" isn't in the dict
print(f"Bonus: {bonus}")

# 3. MODIFYING DATA
# Updating an existing value
record["salary"] = 78000.0 

# Adding a new key-value pair
record["status"] = "Active"

# 4. VIEWING KEYS, VALUES, AND ITEMS
print(f"Keys: {record.keys()}")     # dict_keys(['emp_id', 'name', ...])
print(f"Values: {record.values()}") # dict_values([101, 'Sarah', ...])

# .items() returns a list of (Key, Value) tuples - excellent for loops
for key, value in record.items():
    print(f"{key}: {value}")

# 5. DATA AGGREGATION (Common Exam Scenario)
# Counting occurrences in a dataset
results = ["Pass", "Fail", "Pass", "Pass", "Absent", "Fail"]
summary = {}

for status in results:
    # If key exists, increment. If not, start at 0 and increment.
    summary[status] = summary.get(status, 0) + 1

print(f"Exam Summary: {summary}") # {'Pass': 3, 'Fail': 2, 'Absent': 1}

# 6. REMOVING DATA
# Remove a specific key and return its value
deleted_value = record.pop("status")
