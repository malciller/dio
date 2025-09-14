# Dio

[![OCaml](https://img.shields.io/badge/Language-OCaml-blue.svg)](https://ocaml.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A high-performance, real-time cryptocurrency trading engine built in OCaml. Dio connects to the Kraken exchange via WebSocket and REST APIs to execute automated trading strategies with live market data. 

## Features

- **Real-time Trading**: WebSocket connections for live price feeds and instant order execution
- **Multiple Strategies**: Grid trading, top-level orderbook market making, and price arbitrage
- **Terminal Dashboard**: Real-time CLI interface with price visualization and order tracking
- **Discord Notification**: Real-time order fulfillment notifications via discord webhook
- **Modular Architecture**: Clean separation between strategies, routing, and data feeds
- **Configuration-Driven**: JSON-based configuration for easy strategy deployment
- **Robust Infrastructure**: Comprehensive logging, error handling, and retry mechanisms
- **Kraken Integration**: Full API integration with authentication and order management

## Requirements

- **OCaml** 4.14+ with opam package manager
- **Kraken API** credentials (API key and secret)
- **Unix-like OS** (macOS, Linux, or WSL)

## Quick Start

### 1. Clone & Setup
```bash
git clone https://github.com/malciller/dio.git
cd dio
opam install . --deps-only
dune build
```

### 2. Environment Configuration
Create a `.env` file:
```bash
KRAKEN_API_KEY=your_kraken_api_key
KRAKEN_API_SECRET=your_kraken_api_secret
DISCORD_WEBHOOK_URL=your_discord_webhook_url 
```

### 3. Trading Configuration
Edit `_config.json`:
```json
{
  "assets": [
    {
      "symbol": "BTC/USD",
      "qty": "0.001",
      "grid_interval": "1.0",
      "sell_mult": "0.999",
      "strategy": "Grid"
    },    
    {
      "symbol": "USDT/USD",
      "qty": "100.0",
      "min_usd_balance": "500.0",
      "strategy": "Orderbook"
    }
  ],
  "debounce_ms": 1000,
  "queues_cap": 1000
}
```

### 4. Launch
```bash
# With terminal dashboard
./_build/default/bin/dio.exe --dashboard

# Or headless mode
./_build/default/bin/dio.exe
```

## Trading Strategies

### Grid Strategy
Creates a dynamic grid of buy/sell orders around current market price:
- **Adaptive**: Automatically adjusts order spacing based on market conditions
- **Profit-Taking**: Configurable multipliers for sell orders
- **Risk Management**: Position sizing based on portfolio value
- **Ideal for**: Ranging markets and volatility harvesting

### Orderbook Strategy
Market making at the top of the orderbook:
- **High Frequency**: Maintains orders at best bid/ask prices
- **Tight Spreads**: Minimizes slippage with optimal positioning
- **Volume Management**: Dynamic quantity adjustment
- **Ideal for**: High-liquidity pairs with competitive fees

### Arbitrage Strategy
Detects and executes multi-leg arbitrage opportunities across different trading pairs. This strategy runs automatically on all assets defined in `_config.json` and does not need to be assigned to a specific asset.
- **Graph-Based Detection**: Builds a real-time graph of all available assets to find profitable cycles.
- **Bellman-Ford Algorithm**: Uses an optimized Bellman-Ford algorithm to efficiently identify arbitrage opportunities from price discrepancies.
- **Liquidity Aware**: Calculates trade sizes based on available order book depth and current balances to minimize slippage and ensure execution.
- **Automated Execution**: Submits a rapid sequence of orders to execute all legs of a profitable cycle.
- **Ideal for**: Exploiting short-lived, cross-pair mispricings in real-time.

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   CLI Dashboard │    │     Engine      │    │     Router      │
│                 │    │                 │    │                 │
│ • Real-time UI  │◄──►│ • Orchestration │◄──►│ • Order Mgmt    │
│ • Price Display │    │ • Buffer Mgmt   │    │ • Kraken API    │
│ • Order Status  │    │ • Strategy Exec │    │ • REST/WS       │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                    ┌─────────────────┐
                    │      Feed       │
                    │                 │
                    │ • Market Data   │
                    │ • WebSocket     │
                    │ • Price Updates │
                    └─────────────────┘
```

### Core Components

- **Engine**: Central orchestration managing strategy execution and data flow
- **Strategies**: Pluggable trading logic implementations
- **Router**: Order execution and Kraken API communication layer
- **Feed**: Real-time market data ingestion via WebSocket
- **Dashboard**: Terminal-based monitoring and control interface

## Configuration Reference

### Asset Configuration
```json
{
  "symbol": "BTC/USD",       // Trading pair
  "qty": "0.001",            // Base order quantity
  "grid_interval": "1.0",    // Grid spacing (%)
  "sell_mult": "0.999",      // Sell multiplier (< 1.0 = profit)
  "strategy": "Grid"         // Strategy name
},
{
  "symbol": "USDT/USD",      // Trading pair
  "qty": "100.0",             // Base order quantity
  "min_usd_balance": "500.0",  // Minimum USD balance threshold for strategy to run
  "strategy": "Orderbook"     // Strategy name
}
```

### System Configuration
```json
{
  "debounce_ms": 1000,       // Min time between operations
  "queues_cap": 1000         // Buffer capacity
}
```

## Dashboard Interface

The terminal dashboard provides real-time monitoring:

```
┌─ Dio Trading Engine ──────────────────────────────┐
│ Runtime: 02h:15m:30s  │ Status: Connected         │
├───────────────────────────────────────────────────┤
│ BTC/USD: $43,250.00  ▲0.5%                       │
│ ├── Buy Orders: 3     │ Sell Orders: 2           │
│ ├── Best Bid: $43,245 │ Best Ask: $43,255        │
│ └─────────────────────────────────────────────────┘
│ ETH/USD: $2,650.00   ▼0.2%                       │
│ └── [•••••••••••••••••••••••••••••••••••••] ◇ [••] │
└───────────────────────────────────────────────────┘
Commands: q=quit | Press any key to refresh
```

**Key Features:**
- Real-time price ladders with order visualization
- Order status tracking and execution monitoring
- System logs and performance metrics

## Development

### Building
```bash
# Full build
dune build

# With tests
dune build @all

# Install locally
dune install
```

### Testing
```bash
# Run test suite
dune test

# Run specific tests
dune test src/dio_engine/test/
```

### Code Quality
```bash
# Format code
dune fmt

# Generate docs
dune build @doc
```

## Monitoring & Logging

Dio provides comprehensive logging at multiple levels:

- **Debug**: Detailed execution tracing
- **Info**: Strategy decisions and order executions
- **Warning**: Recoverable errors and rate limit hits
- **Error**: Critical failures requiring attention

Logs are timestamped and categorized by component for easy debugging.

## Risk Management

**CRITICAL**: This software is for educational and research purposes only.

### Important Warnings
- **Financial Risk**: Cryptocurrency trading involves substantial risk of loss
- **API Limits**: Respect exchange rate limits to avoid account restrictions
- **Testing**: Always test strategies with minimal capital first
- **Monitoring**: Never run unattended without proper monitoring
- **Backup**: Maintain separate emergency funds

### Best Practices
- Start with small position sizes
- Implement proper stop-loss mechanisms
- Monitor API usage and account balances
- Keep multiple backup configurations

## Contributing

We welcome contributions! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines
- Add tests for new functionality
- Update documentation
- Ensure CI passes

## Acknowledgments

- Built with [OCaml](https://ocaml.org/) and modern functional programming
- Powered by [LWT](https://github.com/ocsigen/lwt) for concurrency

## Support & Contact

- **Issues**: [GitHub Issues](https://github.com/malciller/dio/issues)
- **Discussions**: [GitHub Discussions](https://github.com/malciller/dio/discussions)

## License
- This project is licensed under the MIT license. Please see LICENSE file for details.

**Legal Disclaimer**: This software is provided "as is" without warranty. The authors are not responsible for any financial losses, damages, or other liabilities arising from its use. Always consult with financial professionals before engaging in cryptocurrency trading.