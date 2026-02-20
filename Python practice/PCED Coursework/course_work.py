"""
Implementation Tasks (Your Coursework)
Data Cleaning: Read stocks.csv.
Count how many Price entries are missing (None or empty).
Replace missing prices with the average price of that specific ticker.

Analysis: Use a dictionary to group prices by Ticker.
Calculate the min, max, and mean price for each ticker using the statistics module.

Advanced Filtering: Use a list comprehension to create a list of all "High Volume" trades (where Volume > 50,000).

Reporting: Print a summary report.
For each ticker, state if the trend is "Stable" or "Volatile" (e.g., if the difference between max and min price is > 10% of the mean).
"""

import os.path
import csv
import math
import statistics
from collections import defaultdict


file_path = "c:/qnt.trading/Python practice/PCED Coursework/stocks.csv"

def get_average_price(data, ticker):
    counter = 0
    sum = 0
    for entry in data:
        if entry["Price"] == '' or entry["Price"] is None:
            continue
        if entry["Ticker"] != ticker:
            continue        
        sum = sum + float(entry["Price"])
        counter = counter + 1
        
    if counter > 0:
        return sum / counter
    else:
        return 0
        


if not os.path.exists(file_path):
    print("File doesn't exist!")

data = []
with open(file_path, mode = "r") as file:
    reader = csv.DictReader(file)
    headers = csv.list_dialects
    for row in reader:
        data.append(row)

# Count how many Price entries are missing (None or empty).
missing_counter = 0
for entry in data:
    if entry["Price"].strip() == "" or entry["Price"] is None:
        missing_counter = missing_counter +  1

print(f"Missing Price entries: {missing_counter}")

# Replace missing prices with the average price of that specific ticker.
tickers = set([d["Ticker"] for d in data if "Ticker" in d])

average_prices = {}
for ticker in tickers:
    average_prices[ticker] = get_average_price(data, ticker)


for entry in data:
    if entry["Price"].strip() == "" or entry["Price"] is None:
        entry["Price"] = average_prices[entry["Ticker"]]


# Analysis: Use a dictionary to group prices by Ticker.
ticker_grp = defaultdict(list)

for d in data:
    ticker_grp[d["Ticker"]].append(d)

# Calculate the min, max, and mean price for each ticker using the statistics module.
ticker_stat = {}

min_values = {
    key: min(float(item["Price"]) for item in items) for key, items in ticker_grp.items()
    }
print("Min Values:")
print(min_values)

max_values = {
    key: max(float(item["Price"]) for item in items) for key, items in ticker_grp.items()
    }
print("Max Values:")
print(max_values)


mean_values = {
    key: statistics.mean(float(item["Price"]) for item in items) for key, items in ticker_grp.items()
    }
print("Mean Values:")
print(mean_values)

ticker_stat = {
    "min_stat" : min_values,
    "max_stat" : max_values,
    "mean_stat" : mean_values
    }

# Advanced Filtering: Use a list comprehension to create a list of all "High Volume" trades (where Volume > 50,000).
high_volumes = [d for d in data if float(d["Volume"])>50000]


# Reporting: Print a summary report.
# For each ticker, state if the trend is "Stable" or "Volatile" (e.g., if the difference between max and min price is > 10% of the mean).
for ticker in tickers:
    trend = (ticker_stat["max_stat"][ticker] - ticker_stat["min_stat"][ticker]) / ticker_stat["mean_stat"][ticker]
    if(trend > 0.1):
        print(f"{ticker}: Volatile Trend")
    else:
        print(f"{ticker}: Stable Trend")

