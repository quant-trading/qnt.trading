# List of transaction dictionaries
transactions = [
    {"id": 1, "amount": 150.50},
    {"id": 2, "amount": 45.00},
    {"id": 3, "amount": 300.25}
]

# Sort transactions by amount (Descending)
# 'key' tells Python which dictionary field to use for sorting
sorted_tx = sorted(transactions, key=lambda x: x['amount'], reverse=True)

print("Top Transaction:", sorted_tx[0])
# Output: Top Transaction: {'id': 3, 'amount': 300.25}

import numpy as np

amounts_list = [t['amount'] for t in transactions]

print(np.sort(amounts_list))

#####################

temperatures = [22.5, 31.0, 18.2, 35.5, 29.0, 40.1]

# Method 1: List Comprehension (Preferred for PCED)
hot_days = [t for t in temperatures if t > 30]

# Method 2: filter() function
def is_extreme(t):
    return t > 35

extreme_heat = list(filter(is_extreme, temperatures))

print(f"Hot days: {hot_days}")
print(f"Extreme heat: {extreme_heat}")
