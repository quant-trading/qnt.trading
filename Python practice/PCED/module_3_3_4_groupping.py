# Raw data: List of dictionaries representing students
students = [
    {"name": "Alice", "class": "A", "score": 85},
    {"name": "Bob", "class": "B", "score": 72},
    {"name": "Charlie", "class": "A", "score": 90},
    {"name": "Diana", "class": "B", "score": 65}
]

# Step 1: Group scores by class
grouped_data = {}
for student in students:
    cls = student["class"]
    score = student["score"]
    
    if cls not in grouped_data:
        grouped_data[cls] = []
    grouped_data[cls].append(score)

# Step 2: Calculate average per class
for cls, scores in grouped_data.items():
    avg = sum(scores) / len(scores)
    print(f"Class {cls} Average: {avg}")

print(grouped_data)
