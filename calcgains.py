import csv
from collections import defaultdict, deque
from decimal import Decimal

def calculate_gains(csv_file):
    trades = []
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            trades.append({
                'time': row['time'],
                'symbol': row['pair'],
                'side': row['type'].lower(),
                'quantity': Decimal(row['vol']),
                'price': Decimal(row['price']),
                'fee': Decimal(row.get('fee', '0'))
            })
    
    # Sort trades by time
    trades.sort(key=lambda x: x['time'])
    
    # Inventory for each symbol: queue of (quantity, price)
    inventories = defaultdict(deque)
    realized_gains = defaultdict(Decimal)
    
    for trade in trades:
        symbol = trade['symbol']
        if trade['side'] == 'buy':
            inventories[symbol].append((trade['quantity'], trade['price']))
        elif trade['side'] == 'sell':
            sell_qty = trade['quantity']
            sell_price = trade['price']
            fee = trade['fee']
            gain = Decimal(0)
            
            while sell_qty > 0 and inventories[symbol]:
                buy_qty, buy_price = inventories[symbol].popleft()
                qty_to_sell = min(sell_qty, buy_qty)
                profit = qty_to_sell * (sell_price - buy_price)
                gain += profit
                sell_qty -= qty_to_sell
                buy_qty -= qty_to_sell
                if buy_qty > 0:
                    inventories[symbol].appendleft((buy_qty, buy_price))
            
            # Subtract fee from gain
            gain -= fee
            realized_gains[symbol] += gain
    
    total_gains = sum(realized_gains.values())
    
    print("Realized Gains by Symbol:")
    for symbol, gain in realized_gains.items():
        print(f"{symbol}: {gain:.2f}")
    
    print(f"\nTotal Realized Gains: {total_gains:.2f}")

if __name__ == "__main__":
    calculate_gains('trades.csv')
