import csv
from collections import defaultdict, deque
from decimal import Decimal, InvalidOperation
from datetime import datetime
import statistics

def safe_decimal(value, default='0'):
    """Safely convert a value to Decimal, handling empty strings and invalid values"""
    if not value or value == '':
        return Decimal(default)
    try:
        return Decimal(value)
    except (InvalidOperation, ValueError):
        return Decimal(default)

def calculate_gains(csv_file):
    trades = []
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            trades.append({
                'time': row['time'],
                'symbol': row['pair'],
                'side': row['type'].lower(),
                'quantity': safe_decimal(row['vol']),
                'price': safe_decimal(row['price']),
                'fee': safe_decimal(row.get('fee', '0')),
                'ordertype': row.get('ordertype', ''),
                'cost': safe_decimal(row.get('cost', '0')),
                'costusd': safe_decimal(row.get('costusd', '0')),
                'net': safe_decimal(row.get('net', '0')),
                'margin': safe_decimal(row.get('margin', '0'))
            })
    
    # Sort trades by time
    trades.sort(key=lambda x: x['time'])
    
    # Inventory for each symbol: queue of (quantity, price)
    inventories = defaultdict(deque)
    realized_gains = defaultdict(Decimal)
    
    # Volume tracking
    volume_by_symbol = defaultdict(lambda: {'quantity': Decimal(0), 'usd_value': Decimal(0)})
    total_volume = {'quantity': Decimal(0), 'usd_value': Decimal(0)}
    
    # Trade counting for statistics
    trade_counts = defaultdict(int)
    total_trades = 0
    
    # Additional analytics
    buy_trades = defaultdict(list)
    sell_trades = defaultdict(list)
    trade_times = []
    fees_by_symbol = defaultdict(Decimal)
    total_fees = Decimal(0)
    
    # Order type analysis
    order_types = defaultdict(int)
    order_types_by_symbol = defaultdict(lambda: defaultdict(int))
    
    # Margin analysis
    margin_used = defaultdict(Decimal)
    total_margin = Decimal(0)
    
    # Cost analysis
    total_cost = Decimal(0)
    total_cost_usd = Decimal(0)
    total_net = Decimal(0)
    
    for trade in trades:
        symbol = trade['symbol']
        quantity = trade['quantity']
        price = trade['price']
        trade_value = quantity * price
        fee = trade['fee']
        ordertype = trade['ordertype']
        cost = trade['cost']
        costusd = trade['costusd']
        net = trade['net']
        margin = trade['margin']
        
        # Track volume for this trade
        volume_by_symbol[symbol]['quantity'] += quantity
        volume_by_symbol[symbol]['usd_value'] += trade_value
        total_volume['quantity'] += quantity
        total_volume['usd_value'] += trade_value
        
        # Count trades
        trade_counts[symbol] += 1
        total_trades += 1
        
        # Track fees
        fees_by_symbol[symbol] += fee
        total_fees += fee
        
        # Track order types
        order_types[ordertype] += 1
        order_types_by_symbol[symbol][ordertype] += 1
        
        # Track margin usage
        margin_used[symbol] += margin
        total_margin += margin
        
        # Track costs
        total_cost += cost
        total_cost_usd += costusd
        total_net += net
        
        # Track trade times for temporal analysis
        try:
            trade_time = datetime.fromisoformat(trade['time'].replace('Z', '+00:00'))
            trade_times.append(trade_time)
        except:
            pass
        
        # Track buy/sell trades separately
        if trade['side'] == 'buy':
            buy_trades[symbol].append({'price': price, 'quantity': quantity, 'time': trade['time']})
        elif trade['side'] == 'sell':
            sell_trades[symbol].append({'price': price, 'quantity': quantity, 'time': trade['time']})
        
        if trade['side'] == 'buy':
            inventories[symbol].append((trade['quantity'], trade['price']))
        elif trade['side'] == 'sell':
            sell_qty = trade['quantity']
            sell_price = trade['price']
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
    
    print("Asset Analysis:")
    print("Symbol".ljust(12) + "Gains".ljust(12) + "Volume(USD)".ljust(12) + "Gains/Vol%".ljust(12) + "Trades".ljust(8) + "Avg Gain")
    print("-" * 80)
    
    winning_assets = 0
    total_assets = len(volume_by_symbol)
    
    for symbol in sorted(volume_by_symbol.keys()):
        gain = realized_gains.get(symbol, Decimal(0))
        volume = volume_by_symbol[symbol]
        trades_count = trade_counts[symbol]
        
        # Calculate gains per volume percentage
        gains_per_volume_pct = (gain / volume['usd_value'] * 100) if volume['usd_value'] > 0 else Decimal(0)
        
        # Calculate average gain per trade
        avg_gain = gain / trades_count if trades_count > 0 else Decimal(0)
        
        # Count winning assets
        if gain > 0:
            winning_assets += 1
        
        print(f"{symbol.ljust(12)}{f'${gain:.2f}'.ljust(12)}{f'${volume['usd_value']:.0f}'.ljust(12)}{f'{gains_per_volume_pct:.2f}%'.ljust(12)}{str(trades_count).ljust(8)}{f'${avg_gain:.2f}'}")
    
    print("-" * 80)
    
    # Overall statistics
    total_gains_per_volume_pct = (total_gains / total_volume['usd_value'] * 100) if total_volume['usd_value'] > 0 else Decimal(0)
    avg_gain_per_trade = total_gains / total_trades if total_trades > 0 else Decimal(0)
    win_rate = (winning_assets / total_assets * 100) if total_assets > 0 else Decimal(0)
    
    print(f"{'TOTAL'.ljust(12)}{f'${total_gains:.2f}'.ljust(12)}{f'${total_volume['usd_value']:.0f}'.ljust(12)}{f'{total_gains_per_volume_pct:.2f}%'.ljust(12)}{str(total_trades).ljust(8)}{f'${avg_gain_per_trade:.2f}'}")
    
    print("\n" + "="*50)
    print("PORTFOLIO STATISTICS")
    print("="*50)
    print(f"Total Realized Gains: ${total_gains:.2f}")
    print(f"Total Volume Traded: ${total_volume['usd_value']:.2f}")
    print(f"Overall Return on Volume: {total_gains_per_volume_pct:.2f}%")
    print(f"Total Trades: {total_trades}")
    print(f"Average Gain per Trade: ${avg_gain_per_trade:.2f}")
    print(f"Assets Traded: {total_assets}")
    print(f"Winning Assets: {winning_assets}")
    print(f"Win Rate: {win_rate:.1f}%")
    print(f"Total Fees Paid: ${total_fees:.2f}")
    print(f"Fees as % of Volume: {(total_fees / total_volume['usd_value'] * 100):.2f}%")
    print(f"Total Margin Used: ${total_margin:.2f}")
    print(f"Total Cost: ${total_cost:.2f}")
    print(f"Total Cost USD: ${total_cost_usd:.2f}")
    print(f"Total Net: ${total_net:.2f}")
    
    # Trading activity analysis
    if trade_times:
        trading_duration = max(trade_times) - min(trade_times)
        print(f"Trading Period: {trading_duration.days} days")
        print(f"Average Trades per Day: {total_trades / max(trading_duration.days, 1):.1f}")
    
    # Order type analysis
    print(f"\nOrder Type Distribution:")
    for ordertype, count in sorted(order_types.items(), key=lambda x: x[1], reverse=True):
        pct = (count / total_trades * 100) if total_trades > 0 else 0
        print(f"{ordertype}: {count} trades ({pct:.1f}%)")
    
    # Top performers
    print(f"\nTop 5 Assets by Gains:")
    sorted_by_gains = sorted(realized_gains.items(), key=lambda x: x[1], reverse=True)
    for i, (symbol, gain) in enumerate(sorted_by_gains[:5], 1):
        print(f"{i}. {symbol}: ${gain:.2f}")
    
    # Worst performers
    print(f"\nBottom 5 Assets by Gains:")
    sorted_by_gains_asc = sorted(realized_gains.items(), key=lambda x: x[1])
    for i, (symbol, gain) in enumerate(sorted_by_gains_asc[:5], 1):
        print(f"{i}. {symbol}: ${gain:.2f}")
    
    # Fee analysis
    print(f"\nFee Analysis by Asset:")
    sorted_by_fees = sorted(fees_by_symbol.items(), key=lambda x: x[1], reverse=True)
    for symbol, fee in sorted_by_fees[:5]:
        fee_pct = (fee / volume_by_symbol[symbol]['usd_value'] * 100) if volume_by_symbol[symbol]['usd_value'] > 0 else 0
        print(f"{symbol}: ${fee:.2f} ({fee_pct:.2f}% of volume)")
    
    # Buy/Sell ratio analysis
    print(f"\nBuy/Sell Analysis:")
    for symbol in sorted(volume_by_symbol.keys()):
        buy_count = len(buy_trades[symbol])
        sell_count = len(sell_trades[symbol])
        if buy_count > 0 or sell_count > 0:
            ratio = buy_count / sell_count if sell_count > 0 else float('inf')
            print(f"{symbol}: {buy_count} buys, {sell_count} sells (ratio: {ratio:.2f})")
    
    # Order type analysis by symbol
    print(f"\nOrder Types by Asset:")
    for symbol in sorted(volume_by_symbol.keys()):
        symbol_orders = order_types_by_symbol[symbol]
        if symbol_orders:
            order_str = ", ".join([f"{ot}: {count}" for ot, count in symbol_orders.items()])
            print(f"{symbol}: {order_str}")
    
    # Most active trading periods (if we have time data)
    if trade_times:
        print(f"\nTrading Activity by Hour:")
        hour_counts = defaultdict(int)
        for trade_time in trade_times:
            hour_counts[trade_time.hour] += 1
        
        sorted_hours = sorted(hour_counts.items(), key=lambda x: x[1], reverse=True)
        for hour, count in sorted_hours[:5]:
            print(f"{hour:02d}:00 - {hour:02d}:59: {count} trades")

if __name__ == "__main__":
    calculate_gains('trades.csv')
