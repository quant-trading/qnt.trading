# Raw input data
entry_value = "500"
threshold = 1000

# 1. Inspect type
print(f"Data type: {type(entry_value)}") # <class 'str'>

# 2. Check if it is a number before performing math
if isinstance(entry_value, (int, float)):
    result = entry_value + threshold
else:
    # 3. Handle string data types specifically
    result = entry_value + " (String Data)" 
    
print(f"Result: {result}") # Output: 500 (String Data)
