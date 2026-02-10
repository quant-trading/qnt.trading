import matplotlib.pyplot as plt

# 1. BAR CHART: Comparing discrete categories (Sales by Department)
departments = ['Electronics', 'Clothing', 'Home', 'Toys']
sales_values = [15000, 12000, 18000, 9000]

plt.figure(figsize=(10, 4))
plt.subplot(1, 3, 1) # Position 1
plt.bar(departments, sales_values, color='skyblue')
plt.title('Sales by Department')
plt.ylabel('Revenue ($)')

# 2. LINE CHART: Trends over a continuous range (Monthly Growth)
months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun']
growth_index = [100, 112, 108, 125, 138, 145]

plt.subplot(1, 3, 2) # Position 2
plt.plot(months, growth_index, marker='o', linestyle='-', color='green')
plt.title('Monthly Growth Trend')
plt.grid(True, linestyle='--', alpha=0.6)

# 3. PIE CHART: Part-to-whole relationship (Market Share)
labels = ['Our Store', 'Competitor A', 'Competitor B', 'Other']
shares = [45, 25, 20, 10]

plt.subplot(1, 3, 3) # Position 3
plt.pie(shares, labels=labels, autopct='%1.1f%%', startangle=140)
plt.title('Market Share %')

plt.tight_layout()
plt.show()
