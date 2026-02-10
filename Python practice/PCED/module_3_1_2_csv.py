import csv

csv_file = "sales_data.csv"
data_to_save = [
    ['Date', 'Product', 'Sales'],
    ['2025-01-01', 'Laptop', '1200'],
    ['2025-01-02', 'Mouse', '25']
]

# 1. Writing to a CSV file
with open(csv_file, 'w', newline='') as file:
    writer = csv.writer(file) # 
    writer.writerows(data_to_save)

# 2. Reading and processing the CSV
try:
    with open(csv_file, 'r') as file:
        reader = csv.reader(file) # 
        header = next(reader) # Skip the header row
        
        print(f"Report for: {header[1]} and {header[2]}")
        for row in reader:
            # Using f-strings for clean output [cite: 471]
            product = row[1]
            amount = float(row[2])
            print(f"- {product}: ${amount:.2f}")
except FileNotFoundError:
    print("The CSV file does not exist.")
