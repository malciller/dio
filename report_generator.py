import csv
from collections import defaultdict, deque
from decimal import Decimal, InvalidOperation
from datetime import datetime
import statistics
import argparse
from datetime import timedelta
import urllib.request
import urllib.parse
import json
import time

def safe_decimal(value, default='0'):
    if not value or value == '':
        return Decimal(default)
    try:
        return Decimal(value)
    except (InvalidOperation, ValueError):
        return Decimal(default)

def process_ledger_data(csv_file):
    """Process Kraken ledger CSV file and extract relevant data"""
    ledger_entries = []
    
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            entry = {
                'txid': row['txid'],
                'refid': row['refid'], 
                'time': row['time'],
                'type': row['type'],
                'subtype': row['subtype'],
                'aclass': row['aclass'],
                'asset': row['asset'],
                'wallet': row['wallet'],
                'amount': safe_decimal(row['amount']),
                'fee': safe_decimal(row['fee']),
                'balance': safe_decimal(row['balance']),
                'amountusd': safe_decimal(row['amountusd']) if row['amountusd'] else None
            }
            
            # Parse datetime
            try:
                dt = datetime.fromisoformat(entry['time'].replace('Z', '+00:00'))
                entry['dt'] = dt
                ledger_entries.append(entry)
            except:
                pass  # skip invalid dates
    
    return sorted(ledger_entries, key=lambda x: x['dt'])

def calculate_portfolio_balances(ledger_entries):
    """Calculate current portfolio balances from ledger entries across all wallets"""
    balances = defaultdict(Decimal)

    # Group entries by asset AND wallet to properly handle earn balances
    asset_wallet_entries = defaultdict(lambda: defaultdict(list))
    for entry in ledger_entries:
        asset_wallet_entries[entry['asset']][entry['wallet']].append(entry)

    # Exclude stock/ETF assets that are not part of the crypto portfolio
    exclude_assets = {'VTI', 'QQQ', 'SCHD'}

    # For each asset, sum balances across all wallets
    for asset, wallet_entries in asset_wallet_entries.items():
        # Skip excluded assets entirely
        if asset in exclude_assets:
            continue

        total_balance = Decimal(0)
        
        # Get the most recent balance for each wallet and sum them
        for wallet, entries in wallet_entries.items():
            if entries:
                # Sort entries by time to ensure proper order
                entries.sort(key=lambda x: x['dt'])
                most_recent_entry = entries[-1]
                wallet_balance = most_recent_entry['balance']
                total_balance += wallet_balance

        # Only include assets with non-zero total balances
        if abs(total_balance) > Decimal('0.00000001'):
            balances[asset] = total_balance

    return balances

def analyze_rewards_by_asset(ledger_entries):
    """Analyze reward/earning transactions by asset"""
    rewards_by_asset = defaultdict(lambda: {
        'total_amount': Decimal(0),
        'transaction_count': 0,
        'first_reward': None,
        'last_reward': None,
        'avg_reward': Decimal(0),
        'monthly_breakdown': defaultdict(Decimal)
    })

    for entry in ledger_entries:
        if entry['type'] == 'earn':
            asset = entry['asset']
            amount = entry['amount']
            dt = entry['dt']

            rewards_by_asset[asset]['total_amount'] += amount
            rewards_by_asset[asset]['transaction_count'] += 1

            if rewards_by_asset[asset]['first_reward'] is None or dt < rewards_by_asset[asset]['first_reward']:
                rewards_by_asset[asset]['first_reward'] = dt

            if rewards_by_asset[asset]['last_reward'] is None or dt > rewards_by_asset[asset]['last_reward']:
                rewards_by_asset[asset]['last_reward'] = dt

            # Monthly breakdown
            month_key = dt.strftime('%Y-%m')
            rewards_by_asset[asset]['monthly_breakdown'][month_key] += amount

    # Calculate averages
    for asset, data in rewards_by_asset.items():
        if data['transaction_count'] > 0:
            data['avg_reward'] = data['total_amount'] / data['transaction_count']

    return dict(rewards_by_asset)

def analyze_asset_balance_history(ledger_entries):
    """Track balance changes over time for each asset, excluding stock investments"""
    balance_history = defaultdict(list)
    current_balances = defaultdict(Decimal)

    # Exclude stock assets
    exclude_assets = {'VTI', 'QQQ', 'SCHD'}

    # Sort entries by time
    sorted_entries = sorted(ledger_entries, key=lambda x: x['dt'])

    for entry in sorted_entries:
        asset = entry['asset']

        # Skip stock assets
        if asset in exclude_assets:
            continue

        amount = entry['amount']
        dt = entry['dt']

        # Update current balance
        current_balances[asset] += amount

        # Record balance snapshot
        balance_history[asset].append({
            'date': dt,
            'balance': current_balances[asset],
            'amount_change': amount,
            'type': entry['type'],
            'txid': entry['txid']
        })

    return dict(balance_history)

def analyze_fees_by_asset(ledger_entries):
    """Analyze fee payments by asset and transaction type, excluding stock investments"""
    fees_by_asset = defaultdict(lambda: {
        'total_fees': Decimal(0),
        'fee_count': 0,
        'by_type': defaultdict(Decimal),
        'avg_fee': Decimal(0)
    })

    # Exclude stock assets
    exclude_assets = {'VTI', 'QQQ', 'SCHD'}

    for entry in ledger_entries:
        if entry['fee'] > 0:
            asset = entry['asset']

            # Skip stock assets and stock-related transactions
            if asset in exclude_assets:
                continue
            if entry['type'] == 'trade' and entry['subtype'] == 'tradeequities':
                continue

            fee = entry['fee']
            tx_type = entry['type']

            fees_by_asset[asset]['total_fees'] += fee
            fees_by_asset[asset]['fee_count'] += 1
            fees_by_asset[asset]['by_type'][tx_type] += fee

    # Calculate averages
    for asset, data in fees_by_asset.items():
        if data['fee_count'] > 0:
            data['avg_fee'] = data['total_fees'] / data['fee_count']

    return dict(fees_by_asset)

def analyze_deposit_withdrawal_patterns(ledger_entries):
    """Analyze deposit and withdrawal patterns, excluding stock investments"""
    capital_flow = {
        'deposits': defaultdict(lambda: {'total': Decimal(0), 'count': 0, 'monthly': defaultdict(Decimal)}),
        'withdrawals': defaultdict(lambda: {'total': Decimal(0), 'count': 0, 'monthly': defaultdict(Decimal)}),
        'net_flow': defaultdict(Decimal)
    }

    # Exclude stock assets and stock-related transactions
    exclude_assets = {'VTI', 'QQQ', 'SCHD'}

    for entry in ledger_entries:
        asset = entry['asset']

        # Skip stock assets entirely
        if asset in exclude_assets:
            continue

        if entry['type'] == 'deposit':
            amount = entry['amount']
            month_key = entry['dt'].strftime('%Y-%m')

            capital_flow['deposits'][asset]['total'] += amount
            capital_flow['deposits'][asset]['count'] += 1
            capital_flow['deposits'][asset]['monthly'][month_key] += amount
            capital_flow['net_flow'][asset] += amount

        elif entry['type'] == 'withdrawal':
            amount = abs(entry['amount'])  # withdrawals are negative, so we take absolute value
            month_key = entry['dt'].strftime('%Y-%m')

            capital_flow['withdrawals'][asset]['total'] += amount
            capital_flow['withdrawals'][asset]['count'] += 1
            capital_flow['withdrawals'][asset]['monthly'][month_key] += amount
            capital_flow['net_flow'][asset] -= amount

    # Subtract 1600 from USDC deposit amount for capital flow analysis
    if 'USDC' in capital_flow['deposits']:
        capital_flow['deposits']['USDC']['total'] -= Decimal('1600')
        capital_flow['net_flow']['USDC'] -= Decimal('1600')

    return capital_flow

def fetch_current_prices(assets):
    """Fetch current USD prices for given assets from Kraken API"""
    prices = {}

    # Kraken API endpoint for ticker data
    base_url = "https://api.kraken.com/0/public/Ticker"

    # Convert asset names to Kraken trading pair format
    # Using the correct Kraken pair naming conventions
    # Only including crypto assets that are available on Kraken
    asset_map = {
        'ADA': 'ADAUSD',
        'BTC': 'XXBTZUSD',
        'DOT': 'DOTUSD',
        'ETH': 'XETHZUSD',
        'SOL': 'SOLUSD',
        'TRX': 'TRXUSD',
        'USDC': 'USDCUSD',
        'USD': 'USDUSD',  # USD to USD - will be handled specially
        'XRP': 'XXRPZUSD',
        'INJ': 'INJUSD',
        'KSM': 'KSMUSD',
        'BABY': 'BABYUSD',
        'EIGEN': 'EIGENUSD',
        'USDG': 'USDGUSD',
        'USDR': 'USDRUSD'
        # Removed: QQQ, SCHD, VTI (stocks/ETFs not available as crypto trading pairs)
    }

    # Filter assets to only those we can get prices for
    valid_assets = [asset for asset in assets if asset in asset_map]

    if not valid_assets:
        print("No valid assets found for price fetching")
        return prices

    # Build the pair string for the API call
    pairs = ','.join([asset_map[asset] for asset in valid_assets])
    print(f"Requesting pairs: {pairs}")

    try:
        url = f"{base_url}?pair={pairs}"
        with urllib.request.urlopen(url, timeout=10) as response:
            data = json.loads(response.read().decode('utf-8'))

        if data['error']:
            print(f"Kraken API error: {data['error']}")
            # If we get unknown asset pair error, let's try individual requests
            if 'Unknown asset pair' in str(data['error']):
                print("Trying individual asset requests...")
                return fetch_prices_individually(valid_assets, asset_map, base_url)
            return prices

        # Parse the price data
        for asset in valid_assets:
            if asset == 'USD':
                # USD is the base currency, always $1.00
                prices[asset] = Decimal('1.00')
                print(f"Set {asset} price: ${prices[asset]:.4f}")
            elif asset_map[asset] in data['result']:
                # Get the last trade price (index 0 of the 'c' array)
                price_str = data['result'][asset_map[asset]]['c'][0]
                prices[asset] = Decimal(price_str)
                print(f"Fetched {asset} price: ${prices[asset]:.4f}")
            else:
                print(f"No price data available for {asset} ({asset_map[asset]})")

    except urllib.error.URLError as e:
        print(f"Error fetching prices from Kraken API: {e}")
        # Try alternative method if network error
        return fetch_prices_individually(valid_assets, asset_map, base_url)
    except (KeyError, ValueError) as e:
        print(f"Error parsing price data: {e}")
    except Exception as e:
        print(f"Unexpected error fetching prices: {e}")

    return prices

def fetch_prices_individually(assets, asset_map, base_url):
    """Fetch prices for assets individually to handle unknown pairs gracefully"""
    prices = {}

    for asset in assets:
        if asset in asset_map:
            pair = asset_map[asset]
            try:
                url = f"{base_url}?pair={pair}"
                with urllib.request.urlopen(url, timeout=10) as response:
                    data = json.loads(response.read().decode('utf-8'))

                if data['error']:
                    print(f"Skipping {asset} ({pair}): {data['error']}")
                    continue

                if asset == 'USD':
                    # USD is the base currency, always $1.00
                    prices[asset] = Decimal('1.00')
                    print(f"Set {asset} price: ${prices[asset]:.4f}")
                elif pair in data['result']:
                    price_str = data['result'][pair]['c'][0]
                    prices[asset] = Decimal(price_str)
                    print(f"Fetched {asset} price: ${prices[asset]:.4f}")
                else:
                    print(f"No price data available for {asset} ({pair})")

            except Exception as e:
                print(f"Error fetching price for {asset}: {e}")
                continue

    return prices

def extract_trades_from_ledger(ledger_entries):
    """Extract trading data from ledger entries"""
    trades_by_refid = defaultdict(list)
    
    # Group trade entries by refid (each trade generates 2+ ledger entries)
    for entry in ledger_entries:
        if entry['type'] == 'trade' and entry['subtype'] == 'tradespot':
            trades_by_refid[entry['refid']].append(entry)
    
    trades = []
    for refid, entries in trades_by_refid.items():
        if len(entries) == 2:  # Normal trade has exactly 2 entries
            # Determine which is the base and which is the quote
            entry1, entry2 = entries
            
            # The entry with negative amount is usually what we're selling (giving up)
            # The entry with positive amount is what we're buying (receiving)
            if entry1['amount'] < 0:
                sell_entry, buy_entry = entry1, entry2
            else:
                sell_entry, buy_entry = entry2, entry1
            
            # Create trade record
            if sell_entry['asset'] == 'USD' or sell_entry['asset'].endswith('USD'):
                # Buying crypto with USD
                trade = {
                    'time': buy_entry['time'],
                    'dt': buy_entry['dt'],
                    'symbol': f"{buy_entry['asset']}/USD",
                    'side': 'buy',
                    'quantity': buy_entry['amount'],
                    'price': abs(sell_entry['amount']) / buy_entry['amount'] if buy_entry['amount'] != 0 else Decimal(0),
                    'fee': sell_entry['fee'] + buy_entry['fee'],
                    'cost': abs(sell_entry['amount']),
                    'costusd': abs(sell_entry['amount']),
                    'net': buy_entry['amount'],
                    'margin': Decimal(0)
                }
            elif buy_entry['asset'] == 'USD' or buy_entry['asset'].endswith('USD'):
                # Selling crypto for USD
                trade = {
                    'time': sell_entry['time'],
                    'dt': sell_entry['dt'],
                    'symbol': f"{sell_entry['asset']}/USD",
                    'side': 'sell',
                    'quantity': abs(sell_entry['amount']),
                    'price': buy_entry['amount'] / abs(sell_entry['amount']) if sell_entry['amount'] != 0 else Decimal(0),
                    'fee': sell_entry['fee'] + buy_entry['fee'],
                    'cost': buy_entry['amount'],
                    'costusd': buy_entry['amount'],
                    'net': abs(sell_entry['amount']),
                    'margin': Decimal(0)
                }
            else:
                # Crypto-to-crypto trade - use USD values if available
                usd_value = None
                if sell_entry['amountusd'] and buy_entry['amountusd']:
                    usd_value = max(abs(sell_entry['amountusd']), abs(buy_entry['amountusd']))
                
                trade = {
                    'time': buy_entry['time'],
                    'dt': buy_entry['dt'],
                    'symbol': f"{buy_entry['asset']}/{sell_entry['asset']}",
                    'side': 'buy',
                    'quantity': buy_entry['amount'],
                    'price': abs(sell_entry['amount']) / buy_entry['amount'] if buy_entry['amount'] != 0 else Decimal(0),
                    'fee': sell_entry['fee'] + buy_entry['fee'],
                    'cost': abs(sell_entry['amount']),
                    'costusd': usd_value if usd_value else abs(sell_entry['amount']),
                    'net': buy_entry['amount'],
                    'margin': Decimal(0)
                }
            
            trades.append(trade)
    
    return trades

def compute_metrics(trades, period_name):
    inventories = defaultdict(deque)
    realized_gains = defaultdict(Decimal)
    
    volume_by_symbol = defaultdict(lambda: {'quantity': Decimal(0), 'usd_value': Decimal(0)})
    total_volume = {'quantity': Decimal(0), 'usd_value': Decimal(0)}
    
    trade_counts = defaultdict(int)
    total_trades = 0
    
    buy_trades = defaultdict(list)
    sell_trades = defaultdict(list)
    trade_times = []
    fees_by_symbol = defaultdict(Decimal)
    total_fees = Decimal(0)
    
    margin_used = defaultdict(Decimal)
    total_margin = Decimal(0)
    
    total_cost = Decimal(0)
    total_cost_usd = Decimal(0)
    total_net = Decimal(0)
    
    for trade in trades:
        symbol = trade['symbol']
        quantity = trade['quantity']
        price = trade['price']
        trade_value = quantity * price
        fee = trade['fee']
        cost = trade['cost']
        costusd = trade['costusd']
        net = trade['net']
        margin = trade['margin']
        
        volume_by_symbol[symbol]['quantity'] += quantity
        volume_by_symbol[symbol]['usd_value'] += trade_value
        total_volume['quantity'] += quantity
        total_volume['usd_value'] += trade_value
        
        trade_counts[symbol] += 1
        total_trades += 1
        
        fees_by_symbol[symbol] += fee
        total_fees += fee
        
        margin_used[symbol] += margin
        total_margin += margin
        
        total_cost += cost
        total_cost_usd += costusd
        total_net += net
        
        trade_times.append(trade['dt'])
        
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
            
            gain -= fee
            realized_gains[symbol] += gain
    
    total_gains = sum(realized_gains.values())
    
    lines = []
    if period_name != "All Time":
        lines.append(f"# {period_name}")
    lines.append("## All-Time Asset Analysis")
    lines.append("| Symbol | Gains | Volume(USD) | Gains/Vol% | Trades | Avg Gain |")
    
    sorted_symbols = sorted(volume_by_symbol.keys(), key=lambda s: realized_gains.get(s, Decimal(0)), reverse=True)
    winning_assets = 0
    total_assets = len(volume_by_symbol)
    
    for symbol in sorted_symbols:
        gain = realized_gains.get(symbol, Decimal(0))
        volume = volume_by_symbol[symbol]
        trades_count = trade_counts[symbol]
        
        gains_per_volume_pct = (gain / volume['usd_value'] * 100) if volume['usd_value'] > 0 else Decimal(0)
        
        avg_gain = gain / trades_count if trades_count > 0 else Decimal(0)
        
        if gain > 0:
            winning_assets += 1
        
        lines.append(f"| {symbol} | ${gain:.2f} | ${volume['usd_value']:.0f} | {gains_per_volume_pct:.2f}% | {trades_count} | ${avg_gain:.2f} |")
    
    total_gains_per_volume_pct = (total_gains / total_volume['usd_value'] * 100) if total_volume['usd_value'] > 0 else Decimal(0)
    avg_gain_per_trade = total_gains / total_trades if total_trades > 0 else Decimal(0)
    win_rate = (winning_assets / total_assets * 100) if total_assets > 0 else Decimal(0)
    
    lines.append(f"| <b>TOTAL</b> | <b>${total_gains:.2f}</b> | <b>${total_volume['usd_value']:.0f}</b> | <b>{total_gains_per_volume_pct:.2f}%</b> | <b>{total_trades}</b> | <b>${avg_gain_per_trade:.2f}</b> |")

    return '\n'.join(lines)

def calculate_gains(csv_file):
    # Process ledger data
    ledger_entries = process_ledger_data(csv_file)
    
    if not ledger_entries:
        print("No valid ledger entries with parsable dates.")
        return ""
    
    # Calculate current portfolio balances
    portfolio_balances = calculate_portfolio_balances(ledger_entries)
    
    # Extract trades from ledger
    trades = extract_trades_from_ledger(ledger_entries)

    # Analyze rewards by asset
    rewards_analysis = analyze_rewards_by_asset(ledger_entries)

    # Analyze balance history
    balance_history = analyze_asset_balance_history(ledger_entries)

    # Analyze fees by asset
    fees_analysis = analyze_fees_by_asset(ledger_entries)

    # Analyze deposit/withdrawal patterns
    capital_flow = analyze_deposit_withdrawal_patterns(ledger_entries)

    # Fetch current prices for all assets with balances or staking rewards
    assets_with_balance = [asset for asset, balance in portfolio_balances.items() if balance != 0]

    # Include assets that have staking rewards
    assets_with_rewards = set()
    if rewards_analysis:
        assets_with_rewards = set(rewards_analysis.keys())

    # Combine all assets that need prices
    all_assets_needing_prices = set(assets_with_balance) | assets_with_rewards

    # Handle USD specially - set price to $1.00
    current_prices = {}
    if 'USD' in all_assets_needing_prices:
        current_prices['USD'] = Decimal('1.00')
        all_assets_needing_prices.remove('USD')  # Remove USD from API fetching

    # Fetch prices for remaining assets
    if all_assets_needing_prices:
        api_prices = fetch_current_prices(list(all_assets_needing_prices))
        current_prices.update(api_prices)

    # Calculate current portfolio value
    portfolio_value = Decimal(0)
    asset_values = {}

    for asset, balance in portfolio_balances.items():
        if balance != 0 and asset in current_prices:
            value = balance * current_prices[asset]
            asset_values[asset] = value
            portfolio_value += value
        elif balance != 0:
            asset_values[asset] = Decimal(0)  # No price available

    # Create comprehensive report structure
    lines = []
    lines.append("# Kraken Portfolio Analysis Report")
    lines.append("")
    lines.append("This report analyzes Kraken Crypto trading activity based on ledger data. All analysis uses FIFO (First In, First Out) accounting for realized gains/losses calculations. Assets marked (inactive) have not been traded in the last 30 days.")
    lines.append("")

    # Portfolio Overview Section
    if portfolio_value > 0:
        lines.append(f"### Total Portfolio Value: ${portfolio_value:.2f}")
        lines.append("")

        lines.append("| Asset | Balance | Current Price | USD Value | % of Portfolio |")
        lines.append("|-------|---------|---------------|-----------|----------------|")

        # Separate assets into active and inactive groups
        active_assets = []
        inactive_assets = []

        for asset, balance in portfolio_balances.items():
            if balance != 0:  # Show all non-zero balances (stocks already excluded in balance calculation)
                # Check if asset is inactive (last activity > 30 days ago)
                is_inactive = False
                if asset in balance_history and balance_history[asset]:
                    last_activity = balance_history[asset][-1]['date']
                    days_since_activity = (datetime.now() - last_activity).days
                    is_inactive = days_since_activity > 30

                if is_inactive:
                    inactive_assets.append(asset)
                else:
                    active_assets.append(asset)

        # Sort each group alphabetically
        active_assets.sort()
        inactive_assets.sort()

        total_usd_value = Decimal(0)

        # Process active assets first
        for asset in active_assets:
            balance = portfolio_balances[asset]
            if asset in current_prices:
                price = current_prices[asset]
                value = asset_values[asset]
                pct_portfolio = (value / portfolio_value * 100) if portfolio_value > 0 else Decimal(0)
                lines.append(f"| {asset} | {balance:.8f} | ${price:.4f} | ${value:.2f} | {pct_portfolio:.1f}% |")
                total_usd_value += value
            else:
                lines.append(f"| {asset} | {balance:.8f} | N/A | N/A | N/A |")

        # Then process inactive assets
        for asset in inactive_assets:
            balance = portfolio_balances[asset]
            if asset in current_prices:
                price = current_prices[asset]
                value = asset_values[asset]
                pct_portfolio = (value / portfolio_value * 100) if portfolio_value > 0 else Decimal(0)
                # Add inactive indicator to asset name
                asset_display = f"{asset} (Inactive)"
                lines.append(f"| {asset_display} | {balance:.8f} | ${price:.4f} | ${value:.2f} | {pct_portfolio:.1f}% |")
                total_usd_value += value
            else:
                lines.append(f"| {asset} (Inactive) | {balance:.8f} | N/A | N/A | N/A |")

        # Add total row
        lines.append(f"| **TOTAL** | - | - | **${total_usd_value:.2f}** | **100.0%** |")
        lines.append("")

    # Rewards Analysis Section (simplified)
    if rewards_analysis:
        lines.append("### Staking Rewards")
        lines.append("| Asset | Total Rewards | Current Value | Transaction Count | Average Reward |")
        lines.append("|-------|---------------|---------------|-------------------|----------------|")

        total_rewards_value = Decimal(0)
        total_rewards_amount = Decimal(0)
        total_transaction_count = 0

        # Separate assets into active and inactive groups based on balance history
        active_assets = []
        inactive_assets = []

        for asset in rewards_analysis.keys():
            # Check if asset is inactive (last activity > 30 days ago)
            is_inactive = False
            if asset in balance_history and balance_history[asset]:
                last_activity = balance_history[asset][-1]['date']
                days_since_activity = (datetime.now() - last_activity).days
                is_inactive = days_since_activity > 30

            if is_inactive:
                inactive_assets.append(asset)
            else:
                active_assets.append(asset)

        # Sort each group alphabetically
        active_assets.sort()
        inactive_assets.sort()

        # Process active assets first
        for asset in active_assets:
            data = rewards_analysis[asset]
            # Calculate current value of rewards
            if asset in current_prices:
                current_value = data['total_amount'] * current_prices[asset]
                current_value_str = f"${current_value:.2f}"
                total_rewards_value += current_value
            else:
                current_value_str = "N/A"
            total_rewards_amount += data['total_amount']
            total_transaction_count += data['transaction_count']
            lines.append(f"| {asset} | {data['total_amount']:.8f} | {current_value_str} | {data['transaction_count']} | ${data['avg_reward']:.4f} |")

        # Then process inactive assets
        for asset in inactive_assets:
            data = rewards_analysis[asset]
            # Calculate current value of rewards
            if asset in current_prices:
                current_value = data['total_amount'] * current_prices[asset]
                current_value_str = f"${current_value:.2f}"
                total_rewards_value += current_value
            else:
                current_value_str = "N/A"
            total_rewards_amount += data['total_amount']
            total_transaction_count += data['transaction_count']

            # Add inactive indicator to asset name
            asset_display = f"{asset} (Inactive)"
            lines.append(f"| {asset_display} | {data['total_amount']:.8f} | {current_value_str} | {data['transaction_count']} | ${data['avg_reward']:.4f} |")

        # Add total row
        avg_reward_total = total_rewards_amount / total_transaction_count if total_transaction_count > 0 else Decimal(0)
        total_value_str = f"${total_rewards_value:.2f}" if total_rewards_value > 0 else "N/A"
        lines.append(f"| **TOTAL** | **{total_rewards_amount:.8f}** | **{total_value_str}** | **{total_transaction_count}** | **${avg_reward_total:.4f}** |")
        lines.append("")

    # Capital Flow Analysis
    if capital_flow['deposits'] or capital_flow['withdrawals']:
        lines.append("### Capital Flow")
        lines.append("| Asset | Deposits | Withdrawals | Net Flow |")
        lines.append("|-------|----------|-------------|----------|")

        # Get all assets that have either deposits or withdrawals
        all_assets = set(capital_flow['deposits'].keys()) | set(capital_flow['withdrawals'].keys())

        if all_assets:
            # Separate assets into active and inactive groups
            active_assets = []
            inactive_assets = []

            for asset in all_assets:
                # Check if asset is inactive (last activity > 30 days ago)
                is_inactive = False
                if asset in balance_history and balance_history[asset]:
                    last_activity = balance_history[asset][-1]['date']
                    days_since_activity = (datetime.now() - last_activity).days
                    is_inactive = days_since_activity > 30

                if is_inactive:
                    inactive_assets.append(asset)
                else:
                    active_assets.append(asset)

            # Sort each group alphabetically
            active_assets.sort()
            inactive_assets.sort()

            # Process active assets first
            for asset in active_assets:
                deposits = capital_flow['deposits'].get(asset, {'total': Decimal(0), 'count': 0})
                withdrawals = capital_flow['withdrawals'].get(asset, {'total': Decimal(0), 'count': 0})
                net_flow = capital_flow['net_flow'].get(asset, Decimal(0))

                lines.append(f"| {asset} | {deposits['total']:.8f} | {withdrawals['total']:.8f} | {net_flow:.8f} |")

            # Then process inactive assets
            for asset in inactive_assets:
                deposits = capital_flow['deposits'].get(asset, {'total': Decimal(0), 'count': 0})
                withdrawals = capital_flow['withdrawals'].get(asset, {'total': Decimal(0), 'count': 0})
                net_flow = capital_flow['net_flow'].get(asset, Decimal(0))

                # Add inactive indicator to asset name
                asset_display = f"{asset} (Inactive)"
                lines.append(f"| {asset_display} | {deposits['total']:.8f} | {withdrawals['total']:.8f} | {net_flow:.8f} |")

        lines.append("")

    # Asset Balance History Summary
    if balance_history:
        lines.append("### Balance History")
        lines.append("| Asset | Current Balance | Peak Balance | Transactions | First Activity | Last Activity |")
        lines.append("|-------|-----------------|--------------|--------------|----------------|---------------|")

        # Separate assets into active and inactive groups
        active_assets = []
        inactive_assets = []

        for asset in balance_history.keys():
            history = balance_history[asset]
            if not history:
                continue

            # Check if asset is inactive (last activity > 30 days ago)
            is_inactive = False
            if history:
                last_activity = history[-1]['date']
                days_since_activity = (datetime.now() - last_activity).days
                is_inactive = days_since_activity > 30

            if is_inactive:
                inactive_assets.append(asset)
            else:
                active_assets.append(asset)

        # Sort each group alphabetically
        active_assets.sort()
        inactive_assets.sort()

        # Process active assets first
        for asset in active_assets:
            history = balance_history[asset]
            # Use actual current balance from portfolio_balances instead of historical data
            current_balance = portfolio_balances.get(asset, Decimal(0))
            peak_balance = max((entry['balance'] for entry in history), default=Decimal(0))
            transaction_count = len(history)
            first_date = history[0]['date'].strftime('%Y-%m-%d') if history else 'N/A'
            last_date = history[-1]['date'].strftime('%Y-%m-%d') if history else 'N/A'

            lines.append(f"| {asset} | {current_balance:.8f} | {peak_balance:.8f} | {transaction_count} | {first_date} | {last_date} |")

        # Then process inactive assets
        for asset in inactive_assets:
            history = balance_history[asset]
            # Use actual current balance from portfolio_balances instead of historical data
            current_balance = portfolio_balances.get(asset, Decimal(0))
            peak_balance = max((entry['balance'] for entry in history), default=Decimal(0))
            transaction_count = len(history)
            first_date = history[0]['date'].strftime('%Y-%m-%d') if history else 'N/A'
            last_date = history[-1]['date'].strftime('%Y-%m-%d') if history else 'N/A'

            # Add inactive indicator to asset name
            asset_display = f"{asset} (Inactive)"

            lines.append(f"| {asset_display} | {current_balance:.8f} | {peak_balance:.8f} | {transaction_count} | {first_date} | {last_date} |")

        lines.append("")

    # Fees Analysis Section
    if fees_analysis:
        lines.append("### Trading Fees")
        lines.append("| Asset | Total Fees | Transaction Count | Average Fee |")
        lines.append("|-------|------------|-------------------|-------------|")

        # Separate assets into active and inactive groups
        active_assets = []
        inactive_assets = []

        for asset in fees_analysis.keys():
            # Check if asset is inactive (last activity > 30 days ago)
            is_inactive = False
            if asset in balance_history and balance_history[asset]:
                last_activity = balance_history[asset][-1]['date']
                days_since_activity = (datetime.now() - last_activity).days
                is_inactive = days_since_activity > 30

            if is_inactive:
                inactive_assets.append(asset)
            else:
                active_assets.append(asset)

        # Sort each group alphabetically
        active_assets.sort()
        inactive_assets.sort()

        # Process active assets first
        for asset in active_assets:
            data = fees_analysis[asset]
            lines.append(f"| {asset} | ${data['total_fees']:.4f} | {data['fee_count']} | ${data['avg_fee']:.4f} |")

        # Then process inactive assets
        for asset in inactive_assets:
            data = fees_analysis[asset]
            # Add inactive indicator to asset name
            asset_display = f"{asset} (Inactive)"
            lines.append(f"| {asset_display} | ${data['total_fees']:.4f} | {data['fee_count']} | ${data['avg_fee']:.4f} |")

        lines.append("")

    # Trading Analysis (if we have trades)
    if trades:
        # Group trades by month
        monthly_trades = defaultdict(list)
        for trade in trades:
            month_key = trade['dt'].strftime('%Y-%m')
            monthly_trades[month_key].append(trade)
        
        sorted_months = sorted(monthly_trades.keys())

        # Capital contributions - easily editable list
        # To add new contributions: add tuples in format ('YYYY-MM-DD', Decimal('amount'))
        capital_contributions = [
            # Format: ('YYYY-MM-DD', amount)
            ('2024-09-01', Decimal('2600')),  # Initial capital through start of data
            ('2025-02-06', Decimal('100')),
            ('2025-02-12', Decimal('300')),
            ('2025-02-24', Decimal('500')),
            ('2025-02-28', Decimal('100')),
            ('2025-03-05', Decimal('100')),
            ('2025-03-09', Decimal('100')),
            ('2025-03-19', Decimal('100')),
            ('2025-03-26', Decimal('100')),
            ('2025-03-29', Decimal('100')),
            ('2025-03-30', Decimal('100')),
            ('2025-04-02', Decimal('100')),
            ('2025-04-16', Decimal('100')),
            ('2025-07-10', Decimal('100')),
            
        ]

        # Trading Performance
        lines.append("## Monthly Performance")
        lines.append("| Month | Gains | Volume USD | Return % | ROC % | Trades | Avg Gain | Fees | Fees % | Capital | Margin | Cost USD |")
        lines.append("|-------|-------|------------|----------|-------|--------|----------|------|--------|---------|--------|----------|")
        
        cumulative_trades = []
        cumulative_metrics = {
            'gains': Decimal(0),
            'volume_usd': Decimal(0),
            'trades': 0,
            'fees': Decimal(0),
            'margin': Decimal(0),
            'cost_usd': Decimal(0),
            'net': Decimal(0)
        }

        # Calculate cumulative capital for each month
        monthly_capital = {}

        for month in sorted_months:
            # Calculate capital available at start of this month
            month_start = datetime.strptime(f"{month}-01", "%Y-%m-%d")
            capital_available = Decimal('0')

            for contrib_date_str, amount in capital_contributions:
                contrib_date = datetime.strptime(contrib_date_str, "%Y-%m-%d")
                if contrib_date <= month_start:
                    capital_available += amount

            monthly_capital[month] = capital_available

        # Process each month cumulatively
        for month in sorted_months:
            month_trades = monthly_trades[month]

            # Build inventory from all previous trades (without calculating gains)
            temp_inventories = defaultdict(deque)
            for trade in cumulative_trades:
                if trade['side'] == 'buy':
                    temp_inventories[trade['symbol']].append((trade['quantity'], trade['price']))
                elif trade['side'] == 'sell':
                    sell_qty = trade['quantity']
                    while sell_qty > 0 and temp_inventories[trade['symbol']]:
                        buy_qty, buy_price = temp_inventories[trade['symbol']].popleft()
                        qty_to_sell = min(sell_qty, buy_qty)
                        sell_qty -= qty_to_sell
                        buy_qty -= qty_to_sell
                        if buy_qty > 0:
                            temp_inventories[trade['symbol']].appendleft((buy_qty, buy_price))

            # Calculate monthly metrics for this month only
            monthly_metrics = {
                'gains': Decimal(0),
                'volume_usd': Decimal(0),
                'trades': 0,
                'fees': Decimal(0),
                'margin': Decimal(0),
                'cost_usd': Decimal(0),
                'net': Decimal(0)
            }

            # Now calculate gains from this month's trades only
            monthly_gains_this_month = Decimal(0)
            for trade in month_trades:
                if trade['side'] == 'buy':
                    temp_inventories[trade['symbol']].append((trade['quantity'], trade['price']))
                elif trade['side'] == 'sell':
                    sell_qty = trade['quantity']
                    sell_price = trade['price']
                    gain = Decimal(0)

                    while sell_qty > 0 and temp_inventories[trade['symbol']]:
                        buy_qty, buy_price = temp_inventories[trade['symbol']].popleft()
                        qty_to_sell = min(sell_qty, buy_qty)
                        profit = qty_to_sell * (sell_price - buy_price)
                        gain += profit
                        sell_qty -= qty_to_sell
                        buy_qty -= qty_to_sell
                        if buy_qty > 0:
                            temp_inventories[trade['symbol']].appendleft((buy_qty, buy_price))

                    gain -= trade['fee']
                    # Attribute the gain to the month when the sell occurred
                    monthly_gains_this_month += gain

                # Track monthly metrics
                quantity = trade['quantity']
                price = trade['price']
                trade_value = quantity * price
                fee = trade['fee']
                costusd = trade['costusd']
                net = trade['net']
                margin = trade['margin']

                monthly_metrics['volume_usd'] += trade_value
                monthly_metrics['trades'] += 1
                monthly_metrics['fees'] += fee
                monthly_metrics['margin'] += margin
                monthly_metrics['cost_usd'] += costusd
                monthly_metrics['net'] += net

            monthly_metrics['gains'] = monthly_gains_this_month

            # Add this month's trades to cumulative
            cumulative_trades.extend(month_trades)

            # Add this month's metrics to cumulative totals
            for trade in month_trades:
                quantity = trade['quantity']
                price = trade['price']
                trade_value = quantity * price
                fee = trade['fee']
                costusd = trade['costusd']
                net = trade['net']
                margin = trade['margin']

                cumulative_metrics['volume_usd'] += trade_value
                cumulative_metrics['trades'] += 1
                cumulative_metrics['fees'] += fee
                cumulative_metrics['margin'] += margin
                cumulative_metrics['cost_usd'] += costusd
                cumulative_metrics['net'] += net

            # Update cumulative gains
            cumulative_metrics['gains'] += monthly_gains_this_month

            # Calculate percentages based on MONTHLY metrics
            return_pct = (monthly_metrics['gains'] / monthly_metrics['volume_usd'] * 100) if monthly_metrics['volume_usd'] > 0 else Decimal(0)
            avg_gain = monthly_metrics['gains'] / monthly_metrics['trades'] if monthly_metrics['trades'] > 0 else Decimal(0)
            fees_pct = (monthly_metrics['fees'] / monthly_metrics['volume_usd'] * 100) if monthly_metrics['volume_usd'] > 0 else Decimal(0)

            # Calculate Return on Capital
            capital = monthly_capital[month]
            roc_pct = (monthly_metrics['gains'] / capital * 100) if capital > 0 else Decimal(0)

            lines.append(f"| {month} | ${monthly_metrics['gains']:.2f} | ${monthly_metrics['volume_usd']:.0f} | {return_pct:.2f}% | {roc_pct:.2f}% | {monthly_metrics['trades']} | ${avg_gain:.2f} | ${monthly_metrics['fees']:.2f} | {fees_pct:.2f}% | ${capital:.0f} | ${monthly_metrics['margin']:.2f} | ${monthly_metrics['cost_usd']:.2f} |")
        
        # All Time totals (use final cumulative metrics)
        total_metrics = cumulative_metrics.copy()
        total_capital = sum(amount for _, amount in capital_contributions)

        total_return_pct = (total_metrics['gains'] / total_metrics['volume_usd'] * 100) if total_metrics['volume_usd'] > 0 else Decimal(0)
        total_avg_gain = total_metrics['gains'] / total_metrics['trades'] if total_metrics['trades'] > 0 else Decimal(0)
        total_fees_pct = (total_metrics['fees'] / total_metrics['volume_usd'] * 100) if total_metrics['volume_usd'] > 0 else Decimal(0)
        total_roc_pct = (total_metrics['gains'] / total_capital * 100) if total_capital > 0 else Decimal(0)

        lines.append(f"| **All Time** | **${total_metrics['gains']:.2f}** | **${total_metrics['volume_usd']:.0f}** | **{total_return_pct:.2f}%** | **{total_roc_pct:.2f}%** | **{total_metrics['trades']}** | **${total_avg_gain:.2f}** | **${total_metrics['fees']:.2f}** | **{total_fees_pct:.2f}%** | **${total_capital:.0f}** | **${total_metrics['margin']:.2f}** | **${total_metrics['cost_usd']:.2f}** |")
        
        lines.append("")

        # Asset Performance Details
        lines.append("### Realized Gains by Asset")

        # Calculate per-asset per-month metrics using proper FIFO accounting
        asset_monthly_data = defaultdict(lambda: defaultdict(lambda: {
            'gains': Decimal(0),
            'volume_usd': Decimal(0),
            'trades': 0,
            'fees': Decimal(0),
            'margin': Decimal(0),
            'cost_usd': Decimal(0)
        }))

        # Group all trades by asset
        asset_all_trades = defaultdict(list)
        for trade in trades:
            asset_all_trades[trade['symbol']].append(trade)

        # For each asset, process ALL its trades chronologically to get correct FIFO gains
        for symbol, asset_trades in asset_all_trades.items():
            # Sort trades chronologically for this asset
            asset_trades.sort(key=lambda x: x['dt'])

            inventory = deque()
            monthly_gains_tracker = defaultdict(Decimal)

            for trade in asset_trades:
                trade_month = trade['dt'].strftime('%Y-%m')
                trade_value = trade['quantity'] * trade['price']

                # Track volume and other metrics for the month
                asset_monthly_data[symbol][trade_month]['volume_usd'] += trade_value
                asset_monthly_data[symbol][trade_month]['trades'] += 1
                asset_monthly_data[symbol][trade_month]['fees'] += trade['fee']
                asset_monthly_data[symbol][trade_month]['margin'] += trade['margin']
                asset_monthly_data[symbol][trade_month]['cost_usd'] += trade['costusd']

                if trade['side'] == 'buy':
                    inventory.append((trade['quantity'], trade['price']))
                elif trade['side'] == 'sell':
                    sell_qty = trade['quantity']
                    sell_price = trade['price']
                    gain = Decimal(0)

                    while sell_qty > 0 and inventory:
                        buy_qty, buy_price = inventory.popleft()
                        qty_to_sell = min(sell_qty, buy_qty)
                        profit = qty_to_sell * (sell_price - buy_price)
                        gain += profit
                        sell_qty -= qty_to_sell
                        buy_qty -= qty_to_sell
                        if buy_qty > 0:
                            inventory.appendleft((buy_qty, buy_price))

                    gain -= trade['fee']
                    # Attribute the gain to the month when the sell occurred
                    monthly_gains_tracker[trade_month] += gain

            # Store the calculated monthly gains
            for month, gains in monthly_gains_tracker.items():
                asset_monthly_data[symbol][month]['gains'] = gains

        # Build detailed tables for each metric
        all_assets = sorted(asset_monthly_data.keys())

        # Table 1: Gains
        lines.append("| Asset | " + " | ".join(sorted_months) + " | Total |")
        lines.append("| " + " | ".join(["---"] * (len(sorted_months) + 2)) + " |")

        for symbol in all_assets:
            row_data = [f"**{symbol}**"]
            for month in sorted_months:
                if month in asset_monthly_data[symbol]:
                    gains = asset_monthly_data[symbol][month]['gains']
                    row_data.append(f"${gains:.2f}")
                else:
                    row_data.append("-")
            # Total gains for this asset
            total_gains = sum(asset_monthly_data[symbol][m]['gains'] for m in asset_monthly_data[symbol])
            row_data.append(f"**${total_gains:.2f}**")
            lines.append("| " + " | ".join(row_data) + " |")

        # Total row for gains
        lines.append("| **TOTAL** | " + " | ".join([
            f"**${sum(asset_monthly_data[symbol][month]['gains'] for symbol in all_assets if month in asset_monthly_data[symbol]):.2f}**"
            for month in sorted_months
        ]) + f" | **${total_metrics['gains']:.2f}** |")

        lines.append("")
        lines.append("### Trading Volume by Asset")

        # Table 2: Volume
        lines.append("| Asset | " + " | ".join(sorted_months) + " | Total |")
        lines.append("| " + " | ".join(["---"] * (len(sorted_months) + 2)) + " |")

        for symbol in all_assets:
            row_data = [f"**{symbol}**"]
            for month in sorted_months:
                if month in asset_monthly_data[symbol]:
                    volume = asset_monthly_data[symbol][month]['volume_usd']
                    row_data.append(f"${volume:.0f}")
                else:
                    row_data.append("-")
            # Total volume for this asset
            total_volume = sum(asset_monthly_data[symbol][m]['volume_usd'] for m in asset_monthly_data[symbol])
            row_data.append(f"**${total_volume:.0f}**")
            lines.append("| " + " | ".join(row_data) + " |")

        # Total row for volume
        lines.append("| **TOTAL** | " + " | ".join([
            f"**${sum(asset_monthly_data[symbol][month]['volume_usd'] for symbol in all_assets if month in asset_monthly_data[symbol]):.0f}**"
            for month in sorted_months
        ]) + f" | **${total_metrics['volume_usd']:.0f}** |")

        lines.append("")
        lines.append("### Return Percentage by Asset")

        # Table 3: Return %
        lines.append("| Asset | " + " | ".join(sorted_months) + " | Total |")
        lines.append("| " + " | ".join(["---"] * (len(sorted_months) + 2)) + " |")

        for symbol in all_assets:
            row_data = [f"**{symbol}**"]
            for month in sorted_months:
                if month in asset_monthly_data[symbol]:
                    gains = asset_monthly_data[symbol][month]['gains']
                    volume = asset_monthly_data[symbol][month]['volume_usd']
                    return_pct = (gains / volume * 100) if volume > 0 else Decimal(0)
                    row_data.append(f"{return_pct:.2f}%")
                else:
                    row_data.append("-")
            # Total return % for this asset
            total_gains = sum(asset_monthly_data[symbol][m]['gains'] for m in asset_monthly_data[symbol])
            total_volume = sum(asset_monthly_data[symbol][m]['volume_usd'] for m in asset_monthly_data[symbol])
            total_return_pct = (total_gains / total_volume * 100) if total_volume > 0 else Decimal(0)
            row_data.append(f"**{total_return_pct:.2f}%**")
            lines.append("| " + " | ".join(row_data) + " |")

        # Total row for return %
        lines.append("| **TOTAL** | " + " | ".join([
            f"**{(sum(asset_monthly_data[symbol][month]['gains'] for symbol in all_assets if month in asset_monthly_data[symbol]) / sum(asset_monthly_data[symbol][month]['volume_usd'] for symbol in all_assets if month in asset_monthly_data[symbol]) * 100):.2f}%**"
            if sum(asset_monthly_data[symbol][month]['volume_usd'] for symbol in all_assets if month in asset_monthly_data[symbol]) > 0 else "**-**"
            for month in sorted_months
        ]) + f" | **{(total_metrics['gains'] / total_metrics['volume_usd'] * 100):.2f}%** |")

        lines.append("")
        lines.append("### Trade Count by Asset")

        # Table 4: Trades
        lines.append("| Asset | " + " | ".join(sorted_months) + " | Total |")
        lines.append("| " + " | ".join(["---"] * (len(sorted_months) + 2)) + " |")

        for symbol in all_assets:
            row_data = [f"**{symbol}**"]
            for month in sorted_months:
                if month in asset_monthly_data[symbol]:
                    trade_count = asset_monthly_data[symbol][month]['trades']
                    row_data.append(f"{trade_count}")
                else:
                    row_data.append("-")
            # Total trades for this asset
            total_trades = sum(asset_monthly_data[symbol][m]['trades'] for m in asset_monthly_data[symbol])
            row_data.append(f"**{total_trades}**")
            lines.append("| " + " | ".join(row_data) + " |")

        # Total row for trades
        lines.append("| **TOTAL** | " + " | ".join([
            f"**{sum(asset_monthly_data[symbol][month]['trades'] for symbol in all_assets if month in asset_monthly_data[symbol])}**"
            for month in sorted_months
        ]) + f" | **{total_metrics['trades']}** |")

        lines.append("")

        detailed_all_time = compute_metrics(trades, 'All Time')
        lines.append(detailed_all_time)
    
    return '\n'.join(lines)

def markdown_to_html(markdown_text):
    """Convert markdown tables and basic formatting to HTML."""
    import re

    # Convert headers
    html = re.sub(r'^### (.*)$', r'<h3>\1</h3>', markdown_text, flags=re.MULTILINE)
    html = re.sub(r'^## (.*)$', r'<h2>\1</h2>', html, flags=re.MULTILINE)
    html = re.sub(r'^# (.*)$', r'<h1>\1</h1>', html, flags=re.MULTILINE)

    # Convert markdown tables to HTML tables
    lines = html.split('\n')
    html_lines = []
    in_table = False
    table_data = []
    current_table_type = None

    for line in lines:
        # Check if this is a header that indicates table type
        if line.startswith('## Trades by Asset and Month'):
            current_table_type = 'trades'
        elif line.startswith('## Return % by Asset and Month'):
            current_table_type = 'return_pct'
        elif line.startswith('## Gains by Asset and Month'): # Add detection for Gains table
            current_table_type = 'gains'
        elif line.startswith('## Volume by Asset and Month'): # Add detection for Volume table
            current_table_type = 'volume'
        elif line.startswith('# Monthly Portfolio Metrics'):
            current_table_type = 'monthly_metrics'
        elif line.startswith('All-Time Asset Analysis'):
            current_table_type = 'asset_analysis'
        elif line.startswith('#') and not line.startswith('## '):
            current_table_type = None

        # Check if this is a table row (contains | )
        if '|' in line and not line.strip().startswith('#'):
            # Remove leading/trailing | and split
            cells = [cell.strip() for cell in line.split('|')[1:-1]]
            if cells and any(cell for cell in cells):  # Skip empty rows
                table_data.append(cells)
                if not in_table:
                    in_table = True
        else:
            # End of table
            if in_table and table_data:
                html_lines.append('<table>')
                for i, row in enumerate(table_data):
                    if i == 0:
                        html_lines.append('<thead><tr>')
                        for cell in row:
                            html_lines.append(f'<th>{cell}</th>')
                        html_lines.append('</tr></thead><tbody>')
                    elif i == 1 and all(cell.replace('-', '').strip() == '' for cell in row):
                        continue  # Skip separator row
                    else:
                        # Check if this is a total row (contains **)
                        is_total = any('**' in cell for cell in row)
                        row_class = ' class="total-row"' if is_total else ''
                        html_lines.append(f'<tr{row_class}>')
                        for cell in row:
                            # Convert markdown formatting to HTML
                            cell = re.sub(r'\*\*(.*?)\*\*', r'<strong>\1</strong>', cell)
                            cell = cell.replace('*', '')

                            # Add color classes based on table type
                            if current_table_type == 'return_pct':
                                # For Return % table, color code all numeric values (with or without %)
                                cell_stripped = cell.strip()
                                if cell_stripped and cell_stripped != '-' and cell_stripped != '':
                                    # Check if this looks like a numeric value (contains digits and optionally %)
                                    has_digit = any(char.isdigit() for char in cell_stripped)
                                    has_percent = '%' in cell_stripped
                                    # Relax this check to allow for percentage signs and other common numeric formatting
                                    is_numeric_or_percentage = has_digit and (has_percent or all(char in '0123456789.%-+,' or char.isspace() for char in cell_stripped))

                                    if is_numeric_or_percentage:
                                        try:
                                            # Extract numeric value (remove % and $ for parsing)
                                            clean_cell = cell_stripped.replace('%', '').replace('$', '').replace(',', '').strip()
                                            value = float(clean_cell)
                                            if value > 0:
                                                cell = f'<span class="positive">{cell}</span>'
                                            elif value < 0:
                                                cell = f'<span class="negative">{cell}</span>'
                                            else:
                                                cell = f'<span class="neutral">{cell}</span>'
                                        except Exception as e:
                                            pass
                            elif current_table_type != 'trades':
                                # For all other tables (not trades table), keep existing color coding
                                if cell.replace('$', '').replace(',', '').replace('.', '').replace('-', '').replace('+', '').strip():
                                    if not any(char.isalpha() for char in cell) and not '%' in cell:  # Only numeric cells without %
                                        try:
                                            value = float(cell.replace('$', '').replace(',', ''))
                                            if value > 0:
                                                cell = f'<span class="positive">{cell}</span>'
                                            elif value < 0:
                                                cell = f'<span class="negative">{cell}</span>'
                                            else:
                                                cell = f'<span class="neutral">{cell}</span>'
                                        except:
                                            pass

                            html_lines.append(f'<td>{cell}</td>')
                        html_lines.append('</tr>')
                html_lines.append('</tbody></table>')
                table_data = []
                in_table = False

            # Add non-table content with better formatting for explanations
            if line.strip():
                # Handle explanation sections
                if '**What this shows:**' in line:
                    # Close any existing explanation box first
                    # Count open vs closed divs to detect nesting
                    open_divs = sum(1 for line in html_lines if line == '<div class="explanation-box">')
                    close_divs = sum(1 for line in html_lines if line == '</div>')

                    if open_divs > close_divs:
                        html_lines.append('</div>')

                    html_lines.append('<div class="explanation-box">')
                    html_lines.append('<p class="explanation-header"><strong>What this shows:</strong></p>')
                    # Extract the explanation text from the same line
                    explanation_text = line.split('**What this shows:**', 1)[1].strip()
                    if explanation_text:
                        html_lines.append(f'<p class="explanation-content">{explanation_text}</p>')
                    continue
                elif '**Transaction Type Explanations:**' in line:
                    # Close any existing explanation box first
                    # Count open vs closed divs to detect nesting
                    open_divs = sum(1 for line in html_lines if line == '<div class="explanation-box">')
                    close_divs = sum(1 for line in html_lines if line == '</div>')

                    if open_divs > close_divs:
                        html_lines.append('</div>')

                    html_lines.append('<div class="explanation-box">')
                    html_lines.append('<p class="explanation-header"><strong>Transaction Type Explanations:</strong></p>')
                    html_lines.append('<ul class="explanation-list">')
                    continue
                elif '**Key Metrics Explained:**' in line:
                    # Close any existing explanation box first
                    # Count open vs closed divs to detect nesting
                    open_divs = sum(1 for line in html_lines if line == '<div class="explanation-box">')
                    close_divs = sum(1 for line in html_lines if line == '</div>')

                    if open_divs > close_divs:
                        html_lines.append('</div>')

                    html_lines.append('<div class="explanation-box">')
                    html_lines.append('<p class="explanation-header"><strong>Key Metrics Explained:</strong></p>')
                    html_lines.append('<ul class="explanation-list">')
                    continue
                elif line.strip().startswith('- **') and '**:' in line:
                    # Bullet point with bold text
                    clean_line = re.sub(r'^\s*-\s*\*\*(.*?)\*\*:\s*(.*)$', r'<li><strong>\1:</strong> \2</li>', line.strip())
                    html_lines.append(clean_line)
                elif line.strip() == 'This report analyzes Kraken Crypto trading activity based on ledger data. All analysis uses FIFO (First In, First Out) accounting for realized gains/losses calculations. Assets marked (inactive) have not been traded in the last 30 days.':
                    html_lines.append('<div class="intro-text">')
                    html_lines.append('<p>' + line.strip() + '</p>')
                    html_lines.append('</div>')
                elif line.strip().startswith('**') and line.strip().endswith('**'):
                    # Close previous explanation box if we're starting a new section
                    if html_lines and html_lines[-1].endswith('</li>'):
                        html_lines.append('</ul>')
                        html_lines.append('</div>')
                    # Regular bold text
                    clean_line = re.sub(r'\*\*(.*?)\*\*', r'<strong>\1</strong>', line)
                    html_lines.append(clean_line)
                elif line.startswith('## ') and ('📊' in line or '📈' in line or '💰' in line or '🔢' in line):
                    # Close any open explanation boxes before new section
                    if html_lines and html_lines[-1] == '</div>':
                        pass  # Already closed
                    elif html_lines and (html_lines[-1].endswith('</p>') or '</li>' in html_lines[-1]) and '</div>' in html_lines[-2:]:
                        # Close explanation box if it's open
                        if not html_lines[-1].endswith('</div>'):
                            html_lines.append('</div>')
                    html_lines.append(line)
                elif '|' in line and not line.strip().startswith('#'):
                    # Close explanation box before table
                    if html_lines and len(html_lines) >= 3:
                        # Check if we have an open explanation box
                        recent_lines = html_lines[-5:]  # Look at last 5 lines
                        has_open_explanation = any('<div class="explanation-box">' in line for line in recent_lines)
                        has_closing_div = any(line == '</div>' for line in recent_lines)

                        if has_open_explanation and not has_closing_div:
                            html_lines.append('</div>')
                    html_lines.append(line)
                else:
                    # Regular line - check if we need to close explanation boxes
                    if html_lines and '</li>' in html_lines[-1] and (line.startswith('## ') or line.startswith('# ') or '|' in line):
                        html_lines.append('</ul>')
                        html_lines.append('</div>')
                    html_lines.append(line)
            else:
                # Close any open explanation boxes on empty lines
                if html_lines and '</li>' in html_lines[-1]:
                    html_lines.append('</ul>')
                    html_lines.append('</div>')
                html_lines.append('<br>')

    # Handle any remaining table
    if in_table and table_data:
        html_lines.append('<table>')
        headers = table_data[0]
        for i, row in enumerate(table_data):
            if i == 0:
                html_lines.append('<thead><tr>')
                for cell in row:
                    html_lines.append(f'<th>{cell}</th>')
                html_lines.append('</tr></thead><tbody>')
            elif i == 1 and all(cell.replace('-', '').strip() == '' for cell in row):
                continue
            else:
                is_total = any('**' in cell for cell in row)
                row_class = ' class="total-row"' if is_total else ''
                html_lines.append(f'<tr{row_class}>')
                for j, cell in enumerate(row):
                    cell = re.sub(r'\*\*(.*?)\*\*', r'<strong>\1</strong>', cell)
                    cell = cell.replace('*', '')
                    cell_stripped = cell.strip()
                    if cell_stripped and cell_stripped != '-' and cell_stripped != '':
                        clean_cell = cell_stripped.replace('%', '').replace('$', '').replace(',', '').replace('<strong>', '').replace('</strong>', '').strip()
                        try:
                            value = float(clean_cell)
                            color_class = None
                            if value < 0:
                                color_class = "negative"
                            elif value > 0:
                                col_name = headers[j].strip() if j < len(headers) else ''
                                if current_table_type in ['gains', 'return_pct'] or any(kw in col_name for kw in ['Gains', 'Return %', 'ROC %', 'Gains/Vol%', 'Avg Gain']):
                                    color_class = "positive"
                                else:
                                    color_class = "neutral"
                            else:
                                color_class = "neutral"
                            if color_class:
                                cell = f'<span class="{color_class}">{cell}</span>'
                        except ValueError:
                            pass
                    html_lines.append(f'<td>{cell}</td>')
                html_lines.append('</tr>')
        html_lines.append('</tbody></table>')
        table_data = []
        in_table = False
        current_table_type = None

    return '\n'.join(html_lines)

def split_report_content(markdown_content):
    """Split the markdown content into three sections for tabs."""
    lines = markdown_content.split('\n')
    sections = {
        'portfolio': [],
        'trading': [],
        'analysis': []
    }

    current_section = 'portfolio'

    for line in lines:
        # Check for section transitions
        if line.startswith('## Monthly Performance'):
            current_section = 'trading'
        elif line.startswith('## All-Time Asset Analysis'):
            current_section = 'analysis'

        sections[current_section].append(line)

    return sections

def generate_html_report(markdown_content, output_file='portfolio_report.html'):
    """Generate an HTML version of the portfolio report with tabs."""
    # Split content into sections
    sections = split_report_content(markdown_content)

    # Convert each section to HTML
    portfolio_content = markdown_to_html('\n'.join(sections['portfolio']))
    trading_content = markdown_to_html('\n'.join(sections['trading']))
    analysis_content = markdown_to_html('\n'.join(sections['analysis']))

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Portfolio Analysis Report</title>
    <style>
        body {{
            font-family: Arial, sans-serif;
            line-height: 1.4;
            color: #333;
            max-width: 1200px;
            margin: 0 auto;
            padding: 15px;
            background: white;
        }}
        .container {{
            background: white;
            padding: 15px;
            margin: 10px 0;
        }}
        h1 {{
            color: #000;
            border-bottom: 2px solid #666;
            padding-bottom: 5px;
            margin-bottom: 20px;
            font-size: 2em;
            font-weight: bold;
        }}
        h2 {{
            color: #333;
            border-left: 3px solid #666;
            padding-left: 10px;
            margin-top: 25px;
            margin-bottom: 10px;
            font-size: 1.4em;
            font-weight: bold;
            clear: both;
        }}
        h3 {{
            color: #666;
            margin-top: 20px;
            margin-bottom: 8px;
            font-size: 1.2em;
            font-weight: bold;
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
            margin: 15px 0;
            font-size: 12px;
            border: 1px solid #ddd;
            background: white;
        }}
        th, td {{
            padding: 8px 12px;
            text-align: left;
            border: 1px solid #ddd;
            vertical-align: top;
        }}
        th {{
            background: #f5f5f5;
            color: #333;
            font-weight: bold;
            font-size: 11px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }}
        tr:nth-child(even) {{
            background: #fafafa;
        }}
        tr:hover {{
            background: #f0f0f0;
        }}
        .positive {{
            color: #006600;
            font-weight: bold;
        }}
        .negative {{
            color: #cc0000;
            font-weight: bold;
        }}
        .neutral {{
            color: #666666;
        }}
        .total-row {{
            background: #e8e8e8;
            font-weight: bold;
            border-top: 2px solid #bbb;
        }}
        .explanation-box {{
            background: #f8f8f8;
            padding: 10px 15px;
            margin: 10px 0;
            clear: both;
        }}
        .explanation-header {{
            color: #333;
            font-size: 14px;
            font-weight: bold;
            margin-bottom: 8px;
            margin-top: 0;
        }}
        .explanation-content {{
            color: #555;
            line-height: 1.4;
            margin-bottom: 0;
            font-size: 13px;
        }}
        .explanation-list {{
            margin: 0;
            padding-left: 15px;
        }}
        .explanation-list li {{
            margin-bottom: 5px;
            line-height: 1.3;
            color: #555;
            font-size: 12px;
        }}
        .explanation-list li strong {{
            color: #333;
            font-weight: bold;
        }}
        .intro-text {{
            background: #f0f0f0;
            color: #333;
            padding: 15px;
            margin: 15px 0;
            text-align: center;
            font-size: 14px;
            line-height: 1.4;
        }}
        .intro-text p {{
            margin: 0;
            font-weight: normal;
        }}
        br {{
            display: none;
        }}

        /* Tab Styles */
        .tab-container {{
            margin: 20px 0;
        }}
        .tab-buttons {{
            display: flex;
            background: #f5f5f5;
            border-radius: 5px 5px 0 0;
            overflow: hidden;
        }}
        .tab-button {{
            flex: 1;
            padding: 12px 20px;
            background: #f5f5f5;
            border: none;
            border-right: 1px solid #ddd;
            cursor: pointer;
            font-size: 14px;
            font-weight: bold;
            color: #666;
            transition: all 0.3s ease;
        }}
        .tab-button:last-child {{
            border-right: none;
        }}
        .tab-button:hover {{
            background: #e8e8e8;
        }}
        .tab-button.active {{
            background: white;
            color: #333;
            border-bottom: 3px solid #007cba;
        }}
        .tab-content {{
            display: none;
            padding: 20px;
            background: white;
            border: 1px solid #ddd;
            border-top: none;
            border-radius: 0 0 5px 5px;
        }}
        .tab-content.active {{
            display: block;
        }}
    </style>
    <script>
        function openTab(tabName) {{
            // Hide all tab contents
            const tabContents = document.querySelectorAll('.tab-content');
            tabContents.forEach(content => content.classList.remove('active'));

            // Remove active class from all buttons
            const tabButtons = document.querySelectorAll('.tab-button');
            tabButtons.forEach(button => button.classList.remove('active'));

            // Show the selected tab content
            document.getElementById(tabName).classList.add('active');

            // Add active class to the clicked button
            event.target.classList.add('active');
        }}

        // Set default active tab
        document.addEventListener('DOMContentLoaded', function() {{
            document.querySelector('.tab-button').click();
        }});
    </script>
</head>
<body>
    <div class="container">
        <div class="tab-container">
            <div class="tab-buttons">
                <button class="tab-button active" onclick="openTab('portfolio')">Portfolio Overview</button>
                <button class="tab-button" onclick="openTab('trading')">Trading Performance</button>
                <button class="tab-button" onclick="openTab('analysis')">Asset Analysis</button>
            </div>
            <div id="portfolio" class="tab-content active">
                {portfolio_content}
            </div>
            <div id="trading" class="tab-content">
                {trading_content}
            </div>
            <div id="analysis" class="tab-content">
                {analysis_content}
            </div>
        </div>
    </div>
</body>
</html>"""

    with open(output_file, 'w') as f:
        f.write(html)
    print(f"HTML report generated: {output_file}")
    print(f"Open {output_file} in your web browser to view the beautiful report!")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate portfolio report from Kraken ledger CSV.")
    parser.add_argument('csv_file', nargs='?', default='ledgers.csv', help='Input CSV file (Kraken ledger format)')
    parser.add_argument('--output', '-o', default='portfolio_report.md', help='Output file (supports .md, .html extensions)')
    parser.add_argument('--format', '-f', choices=['md', 'html'], default='md', help='Output format')
    args = parser.parse_args()

    report = calculate_gains(args.csv_file)
    if report:
        if args.format == 'html':
            # Generate HTML version
            output_file = args.output if args.output.endswith('.html') else args.output.replace('.md', '.html')
            generate_html_report(report, output_file)
        else:
            # Generate markdown version
            with open(args.output, 'w') as f:
                f.write(report)
            print(f"Report written to {args.output}")
    else:
        print("No report generated.")
