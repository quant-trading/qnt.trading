# 1. CREATION
# A list of daily sales figures
sales = [120, 85, 200, 150, 45, 300, 150]

# 2. INDEXING & SLICING
print(f"First day sales: {sales[0]}")      # 120
print(f"Last day sales: {sales[-1]}")      # 150
print(f"Weekend sales (last 2): {sales[-2:]}") # [300, 150]
print(f"Mid-week sales: {sales[2:5]}")    # [200, 150, 45] (Index 2, 3, 4)

# 3. LIST METHODS (Manipulation)
# Add a new data point
sales.append(175) 

# Insert a missing value at index 1
sales.insert(1, 95) 

# Remove a specific value (removes the first occurrence)
sales.remove(150) 

# Remove and return an item by index
removed_item = sales.pop(4) # Removes 45

# Sort the data for analysis (ascending)
sales.sort()
print(f"Sorted Sales: {sales}")

# 4. DATA TRANSFORMATION (List Comprehension)
# Scenario: Apply a 10% tax to all sales figures
sales_with_tax = [round(amount * 1.1, 2) for amount in sales]

# Scenario: Filter for "High Value" sales (> 150)
high_value_sales = [amount for amount in sales if amount > 150]

print(f"Taxes applied: {sales_with_tax}")
print(f"High value entries: {high_value_sales}")

# 5. BASIC AGGREGATION
print(f"Total Sales Count: {len(sales)}")
print(f"Max Sale: {max(sales)}")
print(f"Min Sale: {min(sales)}")
