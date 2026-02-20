import random
import csv

# Simulating stock data for 10 days
tickers = ['AAPL', 'MSFT', 'GOOG', 'AMZN']
data = []

for _ in range(50):
    row = {
        'Date': f"2024-01-{random.randint(1, 31):02d}",
        'Ticker': random.choice(tickers),
        'Price': round(random.uniform(100, 2000), 2) if random.random() > 0.1 else None, # 10% missing data
        'Volume': random.randint(1000, 100000)
    }
    data.append(row)

# Save to CSV (Exam Block 3: File Handling)
with open('stocks.csv', 'w', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=['Date', 'Ticker', 'Price', 'Volume'])
    writer.writeheader()
    writer.writerows(data)
