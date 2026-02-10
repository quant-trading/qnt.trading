import numpy as np

# A standard Python list
data_list = [10, 20, 30, 40, 50]

# Convert to a NumPy array
data_array = np.array(data_list)

print(f"Type: {type(data_array)}")
print(f"Array: {data_array}")

#########################

prices = np.array([19.99, 5.50, 10.00, 100.00, 45.00])

# NumPy statistical functions
total = np.sum(prices)
average = np.mean(prices)
midpoint = np.median(prices)
spread = np.std(prices)

print(f"Total: {total}, Mean: {average:.2f}, Median: {midpoint}, Std Dev: {spread:.2f}")


#########################

# Generate values from 0 to 10 with a step of 2
evens = np.arange(0, 11, 2) 
# Output: [ 0  2  4  6  8 10]

# Generate 5 values evenly spaced between 0 and 1
grid = np.linspace(0, 1, 5) 
# Output: [0.   0.25 0.5  0.75 1.  ]

print(f"Arange: {evens}")
print(f"Linspace: {grid}")
