# 1. CREATION
# A tuple representing a single data record: (ID, "Category", Value)
record = (101, "Electronics", 299.99)

# Note: Even a single item needs a comma to be a tuple
single_item_tuple = (50,) 

# 2. ACCESSING DATA
# Like lists, tuples use zero-based indexing
print(f"Record ID: {record[0]}")      # 101
print(f"Category: {record[1]}")       # Electronics

# Slicing works the same way as lists
metadata = record[1:3]                # ("Electronics", 299.99)

# 3. IMMUTABILITY (The most important exam concept)
try:
    record[2] = 350.00  # This will raise a TypeError
except TypeError as e:
    print(f"Error: {e}") # 'tuple' object does not support item assignment

# 4. TUPLE UNPACKING
# This is a very common way to extract data from a record
id_num, cat, price = record

print(f"Unpacked Price: {price}")     # 299.99

# 5. CONVERSION
# If you MUST change a tuple, you convert it to a list, change it, and convert back
temp_list = list(record)
temp_list[2] = 350.00
record = tuple(temp_list)
print(f"Updated record: {record}")
