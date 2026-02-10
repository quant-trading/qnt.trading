# 1. CREATION
# Imagine two lists of Customer IDs from different months
jan_customers = [101, 102, 103, 101, 104, 102]
feb_customers = [103, 104, 105, 106, 104]

# Converting lists to sets automatically removes duplicates
set_jan = set(jan_customers)
set_feb = set(feb_customers)

print(f"Unique January Customers: {set_jan}") # {101, 102, 103, 104}
print(f"Unique February Customers: {set_feb}") # {103, 104, 105, 106}

# 2. SET OPERATIONS (The "Big Four")

# UNION: All unique customers from both months
all_customers = set_jan.union(set_feb)
# Alternatively: all_customers = set_jan | set_feb
print(f"Total unique customers (Union): {all_customers}")

# INTERSECTION: Customers who purchased in BOTH months (Returning customers)
returning_customers = set_jan.intersection(set_feb)
# Alternatively: returning_customers = set_jan & set_feb
print(f"Returning customers (Intersection): {returning_customers}")

# DIFFERENCE: Customers in Jan who did NOT return in Feb (Churned)
churned_customers = set_jan.difference(set_feb)
# Alternatively: churned_customers = set_jan - set_feb
print(f"Churned customers (Difference): {churned_customers}")

# SYMMETRIC DIFFERENCE: Customers in either month, but NOT both
unique_to_one_month = set_jan.symmetric_difference(set_feb)
print(f"One-time only customers: {unique_to_one_month}")

# 3. MANIPULATION
set_jan.add(107)      # Add a single item
set_jan.remove(101)   # Remove an item (raises error if not found)
set_jan.discard(999)  # Remove an item (does NOT raise error if missing)
