from collections import Counter

colors = ["red", "blue", "red", "green", "blue", "red"]
color_counts = Counter(colors)

print(color_counts) # Counter({'red': 3, 'blue': 2, 'green': 1})
# Get the single most common item
print(f"Most common: {color_counts.most_common(1)}") 
