import os

filename = "data_notes.txt"

# 1. Writing to a file using 'with'
try:
    with open(filename, 'w') as file:
        file.write("Entry 1: Data analysis is fun.\n")
        file.write("Entry 2: Python makes it easier.")
    print(f"Successfully wrote to {filename}")
except Exception as e:
    print(f"An error occurred: {e}")

# 2. Checking if file exists and reading content
if os.path.exists(filename): # [cite: 464, 465]
    try:
        with open(filename, 'r') as file:
            content = file.readlines() # Returns a list of lines
            for line in content:
                print(f"Line read: {line.strip()}") # .strip() removes \n
    except FileNotFoundError:
        print("Error: The file was not found.") # 
