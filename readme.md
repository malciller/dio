# Dio - Cryptocurrency Trading Engine

[![OCaml](https://img.shields.io/badge/Language-OCaml-blue.svg)](https://ocaml.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A high-performance, real-time cryptocurrency trading engine built in OCaml. Dio connects to the Kraken exchange via WebSocket and REST APIs to execute automated trading strategies with live market data. 

## Features

- **Real-time Trading**: WebSocket connections for live price feeds and instant order execution
- **Multiple Example Strategies**: Grid trading and top-level orderbook market making
- **Terminal Dashboard**: Real-time CLI interface with price visualization and order tracking
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
      "grid_interval": "0.01",
      "sell_mult": "1.0",
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
  "symbol": "BTC/USD",        // Trading pair
  "qty": "0.001",            // Base order quantity
  "grid_interval": "1.0",    // Grid spacing (%)
  "sell_mult": "0.999",      // Sell multiplier (< 1.0 = profit)
  "strategy": "Grid"         // "Grid" or "Orderbook"
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
- Inspired by high-frequency trading systems and algorithmic strategies
- Thanks to the OCaml ecosystem for excellent libraries

## Support & Contact

- **Issues**: [GitHub Issues](https://github.com/malciller/dio/issues)
- **Discussions**: [GitHub Discussions](https://github.com/malciller/dio/discussions)
- **Documentation**: Check `_docs/workflow.md` for detailed architecture

## License
- This project is licensed under the MIT license. Please see LICENSE file for details.

**Legal Disclaimer**: This software is provided "as is" without warranty. The authors are not responsible for any financial losses, damages, or other liabilities arising from its use. Always consult with financial professionals before engaging in cryptocurrency trading.


## Example Performance - $4,500 Capital

# Monthly Portfolio Metrics
| Month | Gains | Volume USD | Return % | ROC % | Trades | Avg Gain | Fees | Fees % | Capital | Margin | Cost USD |
|-------|-------|------------|----------|-------|--------|----------|------|--------|---------|--------|----------|
| 2024-09 | $26.00 | $12353 | 0.21% | 1.00% | 1853 | $0.01 | $30.42 | 0.25% | $2600 | $0.00 | $12352.88 |
| 2024-10 | $8.06 | $5239 | 0.15% | 0.31% | 849 | $0.01 | $12.64 | 0.24% | $2600 | $0.00 | $5239.08 |
| 2024-11 | $59.23 | $12643 | 0.47% | 2.28% | 1485 | $0.04 | $28.51 | 0.23% | $2600 | $0.00 | $12642.76 |
| 2024-12 | $-17.00 | $14706 | -0.12% | -0.65% | 1009 | $-0.02 | $29.90 | 0.20% | $2600 | $0.00 | $14705.59 |
| 2025-01 | $221.93 | $39997 | 0.55% | 8.54% | 1348 | $0.16 | $79.99 | 0.20% | $2600 | $0.00 | $39996.69 |
| 2025-02 | $-224.69 | $25550 | -0.88% | -8.64% | 797 | $-0.28 | $51.67 | 0.20% | $2600 | $0.00 | $25549.97 |
| 2025-03 | $-132.05 | $27963 | -0.47% | -3.67% | 498 | $-0.27 | $56.25 | 0.20% | $3600 | $0.00 | $27962.75 |
| 2025-04 | $142.26 | $16316 | 0.87% | 3.47% | 296 | $0.48 | $32.78 | 0.20% | $4100 | $0.00 | $16316.33 |
| 2025-05 | $451.15 | $28717 | 1.57% | 10.49% | 550 | $0.82 | $57.60 | 0.20% | $4300 | $0.00 | $28716.97 |
| 2025-06 | $-37.07 | $17496 | -0.21% | -0.86% | 282 | $-0.13 | $35.28 | 0.20% | $4300 | $0.00 | $17496.37 |
| 2025-07 | $471.93 | $793436 | 0.06% | 10.98% | 12995 | $0.04 | $40.27 | 0.01% | $4300 | $0.00 | $793435.55 |
| 2025-08 | $162.47 | $561932 | 0.03% | 3.69% | 10739 | $0.02 | $44.03 | 0.01% | $4400 | $0.00 | $561931.89 |
| 2025-09 | $-48.93 | $110705 | -0.04% | -1.11% | 2230 | $-0.02 | $8.53 | 0.01% | $4400 | $0.00 | $110705.43 |
| **All Time** | **$1083.30** | **$1667052** | **0.06%** | **24.62%** | **34931** | **$0.03** | **$507.88** | **0.03%** | **$4400** | **$0.00** | **$1667052.28** |

# Detailed Monthly Asset Performance

## Gains by Asset and Month

| Asset | 2024-09 | 2024-10 | 2024-11 | 2024-12 | 2025-01 | 2025-02 | 2025-03 | 2025-04 | 2025-05 | 2025-06 | 2025-07 | 2025-08 | 2025-09 | **Total** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **ADA/USD** | $4.50 | - | $2.13 | - | $5.29 | $-11.90 | $53.23 | $-45.77 | $25.85 | $-32.44 | $75.67 | $63.85 | $-14.57 | **$125.82** |
| **AVAX/USD** | $2.23 | $1.24 | $1.35 | - | $-5.48 | $-62.93 | - | - | - | - | - | - | - | **$-63.59** |
| **BTC/USD** | $4.74 | $3.31 | $11.95 | $-9.35 | $99.38 | $-74.32 | $-131.53 | $266.89 | $244.12 | $2.77 | $142.66 | $-52.28 | $-22.63 | **$485.73** |
| **DOT/USD** | - | - | - | - | - | $5.95 | $-1.55 | $-8.77 | $16.76 | $0.31 | - | - | - | **$12.68** |
| **ETH/USD** | $6.16 | $2.36 | $9.54 | $-1.38 | $3.65 | $-20.43 | $-3.18 | $-50.52 | $52.82 | $4.39 | $117.95 | $58.54 | $-0.54 | **$179.36** |
| **EURR/USD** | - | - | - | - | - | - | - | - | - | - | $0.88 | - | - | **$0.88** |
| **FET/USD** | $-0.89 | $-1.73 | $6.67 | - | - | - | - | - | - | - | - | - | - | **$4.05** |
| **INJ/USD** | - | - | - | - | - | $2.21 | $-9.29 | $0.00 | - | $19.16 | - | - | - | **$12.08** |
| **KSM/USD** | - | - | - | - | - | $12.12 | $0.38 | $-7.82 | $32.95 | - | - | - | - | **$37.64** |
| **LINK/USD** | - | - | - | - | $15.57 | $-30.48 | - | - | - | - | - | - | - | **$-14.91** |
| **QQQ/USD** | - | - | - | - | - | - | - | - | - | - | $0.00 | - | - | **$0.00** |
| **SCHD/USD** | - | - | - | - | - | - | - | - | - | - | $0.00 | - | - | **$0.00** |
| **SOL/USD** | $6.34 | $7.91 | $19.53 | $-24.55 | $60.38 | $-27.95 | $-38.19 | $-10.00 | $62.38 | $-34.39 | $80.15 | $56.35 | $-2.61 | **$155.36** |
| **SUI/USD** | - | - | - | - | $4.32 | $-7.31 | - | - | - | - | - | - | - | **$-2.99** |
| **TRX/USD** | $0.03 | - | - | - | $5.32 | $3.63 | $0.32 | $3.19 | $10.36 | $3.13 | $11.65 | $6.84 | $-13.87 | **$30.59** |
| **USDC/USD** | $0.00 | $0.00 | $0.00 | $-1.33 | $-3.20 | $-2.00 | $-1.20 | $-0.20 | - | - | $-3.60 | $0.00 | - | **$-11.52** |
| **USDG/USD** | - | - | - | - | - | - | - | - | - | - | $46.59 | $29.09 | $5.28 | **$80.97** |
| **USDG/USDC** | - | - | - | - | - | - | - | - | - | - | $-0.01 | - | - | **$-0.01** |
| **VTI/USD** | - | - | - | - | - | - | - | - | - | - | $0.00 | $0.07 | - | **$0.07** |
| **XLM/USD** | - | - | - | - | $2.01 | $-9.53 | - | - | - | - | - | - | - | **$-7.52** |
| **XMR/USD** | $-2.22 | $-0.16 | $0.28 | $6.31 | - | - | - | - | - | - | - | - | - | **$4.20** |
| **XRP/USD** | $5.11 | $-4.86 | $7.78 | $13.31 | $34.70 | $-1.75 | $-1.05 | $-4.73 | $5.92 | - | - | - | - | **$54.42** |
| **TOTAL** | **$26.00** | **$8.06** | **$59.23** | **$-17.00** | **$221.93** | **$-224.69** | **$-132.05** | **$142.26** | **$451.15** | **$-37.07** | **$471.93** | **$162.47** | **$-48.93** | **$1083.30** |

## Volume by Asset and Month

| Asset | 2024-09 | 2024-10 | 2024-11 | 2024-12 | 2025-01 | 2025-02 | 2025-03 | 2025-04 | 2025-05 | 2025-06 | 2025-07 | 2025-08 | 2025-09 | **Total** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **ADA/USD** | $2280 | - | $89 | - | $1697 | $680 | $1421 | $373 | $1780 | $447 | $2181 | $3216 | $325 | **$14491** |
| **AVAX/USD** | $1300 | $105 | $36 | - | $1216 | $214 | - | - | - | - | - | - | - | **$2872** |
| **BTC/USD** | $1678 | $1250 | $4006 | $7171 | $27277 | $17925 | $24539 | $14496 | $23384 | $14652 | $13068 | $12966 | $3092 | **$165505** |
| **DOT/USD** | - | - | - | - | - | $598 | $67 | $81 | $67 | $11 | - | - | - | **$824** |
| **ETH/USD** | $1341 | $668 | $1608 | $2011 | $847 | $324 | $90 | $202 | $1491 | $707 | $1478 | $2212 | $296 | **$13275** |
| **EURR/USD** | - | - | - | - | - | - | - | - | - | - | $1278 | - | - | **$1278** |
| **FET/USD** | $332 | $160 | $50 | - | - | - | - | - | - | - | - | - | - | **$542** |
| **INJ/USD** | - | - | - | - | - | $685 | $88 | $49 | - | $179 | - | - | - | **$1001** |
| **KSM/USD** | - | - | - | - | - | $1119 | $106 | $78 | $147 | - | - | - | - | **$1451** |
| **LINK/USD** | - | - | - | - | $1378 | $283 | - | - | - | - | - | - | - | **$1661** |
| **QQQ/USD** | - | - | - | - | - | - | - | - | - | - | $340 | - | - | **$340** |
| **SCHD/USD** | - | - | - | - | - | - | - | - | - | - | $1019 | - | - | **$1019** |
| **SOL/USD** | $2779 | $2793 | $6738 | $1243 | $2454 | $1091 | $460 | $652 | $1368 | $724 | $1113 | $3029 | $284 | **$24727** |
| **SUI/USD** | - | - | - | - | $666 | $13 | - | - | - | - | - | - | - | **$679** |
| **TRX/USD** | $12 | - | - | - | $605 | $397 | $26 | $147 | $400 | $777 | $488 | $491 | $269 | **$3613** |
| **USDC/USD** | $15 | $15 | $55 | $735 | $1600 | $999 | $600 | $100 | - | - | $1799 | $100 | - | **$6017** |
| **USDG/USD** | - | - | - | - | - | - | - | - | - | - | $769396 | $539818 | $106439 | **$1415653** |
| **USDG/USDC** | - | - | - | - | - | - | - | - | - | - | $935 | - | - | **$935** |
| **VTI/USD** | - | - | - | - | - | - | - | - | - | - | $340 | $100 | - | **$440** |
| **XLM/USD** | - | - | - | - | $238 | $17 | - | - | - | - | - | - | - | **$255** |
| **XMR/USD** | $423 | $19 | $10 | $55 | - | - | - | - | - | - | - | - | - | **$507** |
| **XRP/USD** | $2192 | $229 | $51 | $3491 | $2020 | $1203 | $564 | $138 | $80 | - | - | - | - | **$9968** |
| **TOTAL** | **$12353** | **$5239** | **$12643** | **$14706** | **$39997** | **$25550** | **$27963** | **$16316** | **$28717** | **$17496** | **$793436** | **$561932** | **$110705** | **$1667052** |

## Return % by Asset and Month

| Asset | 2024-09 | 2024-10 | 2024-11 | 2024-12 | 2025-01 | 2025-02 | 2025-03 | 2025-04 | 2025-05 | 2025-06 | 2025-07 | 2025-08 | 2025-09 | **Total** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **ADA/USD** | 0.20% | - | 2.40% | - | 0.31% | -1.75% | 3.74% | -12.27% | 1.45% | -7.25% | 3.47% | 1.99% | -4.48% | **0.87%** |
| **AVAX/USD** | 0.17% | 1.18% | 3.71% | - | -0.45% | -29.38% | - | - | - | - | - | - | - | **-2.21%** |
| **BTC/USD** | 0.28% | 0.26% | 0.30% | -0.13% | 0.36% | -0.41% | -0.54% | 1.84% | 1.04% | 0.02% | 1.09% | -0.40% | -0.73% | **0.29%** |
| **DOT/USD** | - | - | - | - | - | 0.99% | -2.30% | -10.89% | 24.87% | 2.77% | - | - | - | **1.54%** |
| **ETH/USD** | 0.46% | 0.35% | 0.59% | -0.07% | 0.43% | -6.30% | -3.52% | -24.97% | 3.54% | 0.62% | 7.98% | 2.65% | -0.18% | **1.35%** |
| **EURR/USD** | - | - | - | - | - | - | - | - | - | - | 0.07% | - | - | **0.07%** |
| **FET/USD** | -0.27% | -1.08% | 13.40% | - | - | - | - | - | - | - | - | - | - | **0.75%** |
| **INJ/USD** | - | - | - | - | - | 0.32% | -10.59% | 0.00% | - | 10.73% | - | - | - | **1.21%** |
| **KSM/USD** | - | - | - | - | - | 1.08% | 0.36% | -10.02% | 22.41% | - | - | - | - | **2.59%** |
| **LINK/USD** | - | - | - | - | 1.13% | -10.77% | - | - | - | - | - | - | - | **-0.90%** |
| **QQQ/USD** | - | - | - | - | - | - | - | - | - | - | 0.00% | - | - | **0.00%** |
| **SCHD/USD** | - | - | - | - | - | - | - | - | - | - | 0.00% | - | - | **0.00%** |
| **SOL/USD** | 0.23% | 0.28% | 0.29% | -1.98% | 2.46% | -2.56% | -8.30% | -1.53% | 4.56% | -4.75% | 7.20% | 1.86% | -0.92% | **0.63%** |
| **SUI/USD** | - | - | - | - | 0.65% | -54.31% | - | - | - | - | - | - | - | **-0.44%** |
| **TRX/USD** | 0.24% | - | - | - | 0.88% | 0.92% | 1.20% | 2.17% | 2.59% | 0.40% | 2.39% | 1.39% | -5.15% | **0.85%** |
| **USDC/USD** | 0.00% | 0.00% | 0.00% | -0.18% | -0.20% | -0.20% | -0.20% | -0.20% | - | - | -0.20% | 0.00% | - | **-0.19%** |
| **USDG/USD** | - | - | - | - | - | - | - | - | - | - | 0.01% | 0.01% | 0.00% | **0.01%** |
| **USDG/USDC** | - | - | - | - | - | - | - | - | - | - | -0.00% | - | - | **-0.00%** |
| **VTI/USD** | - | - | - | - | - | - | - | - | - | - | 0.00% | 0.07% | - | **0.02%** |
| **XLM/USD** | - | - | - | - | 0.85% | -56.29% | - | - | - | - | - | - | - | **-2.95%** |
| **XMR/USD** | -0.53% | -0.82% | 2.77% | 11.56% | - | - | - | - | - | - | - | - | - | **0.83%** |
| **XRP/USD** | 0.23% | -2.13% | 15.23% | 0.38% | 1.72% | -0.15% | -0.19% | -3.42% | 7.40% | - | - | - | - | **0.55%** |
| **TOTAL** | **0.21%** | **0.15%** | **0.47%** | **-0.12%** | **0.55%** | **-0.88%** | **-0.47%** | **0.87%** | **1.57%** | **-0.21%** | **0.06%** | **0.03%** | **-0.04%** | **0.06%** |

## Trades by Asset and Month

| Asset | 2024-09 | 2024-10 | 2024-11 | 2024-12 | 2025-01 | 2025-02 | 2025-03 | 2025-04 | 2025-05 | 2025-06 | 2025-07 | 2025-08 | 2025-09 | **Total** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **ADA/USD** | 259 | - | 13 | - | 150 | 56 | 82 | 32 | 138 | 31 | 98 | 109 | 10 | **978** |
| **AVAX/USD** | 286 | 18 | 6 | - | 111 | 8 | - | - | - | - | - | - | - | **429** |
| **BTC/USD** | 200 | 181 | 445 | 359 | 427 | 244 | 294 | 154 | 184 | 111 | 90 | 86 | 19 | **2794** |
| **DOT/USD** | - | - | - | - | - | 53 | 6 | 7 | 1 | 1 | - | - | - | **68** |
| **ETH/USD** | 226 | 126 | 230 | 181 | 76 | 28 | 9 | 17 | 97 | 40 | 60 | 65 | 9 | **1164** |
| **EURR/USD** | - | - | - | - | - | - | - | - | - | - | 17 | - | - | **17** |
| **FET/USD** | 81 | 44 | 5 | - | - | - | - | - | - | - | - | - | - | **130** |
| **INJ/USD** | - | - | - | - | - | 58 | 8 | 4 | - | 1 | - | - | - | **71** |
| **KSM/USD** | - | - | - | - | - | 97 | 9 | 7 | 2 | - | - | - | - | **115** |
| **LINK/USD** | - | - | - | - | 123 | 21 | - | - | - | - | - | - | - | **144** |
| **QQQ/USD** | - | - | - | - | - | - | - | - | - | - | 1 | - | - | **1** |
| **SCHD/USD** | - | - | - | - | - | - | - | - | - | - | 1 | - | - | **1** |
| **SOL/USD** | 410 | 435 | 766 | 122 | 184 | 97 | 38 | 49 | 92 | 49 | 49 | 94 | 7 | **2392** |
| **SUI/USD** | - | - | - | - | 60 | 1 | - | - | - | - | - | - | - | **61** |
| **TRX/USD** | 2 | - | - | - | 54 | 34 | 2 | 11 | 35 | 49 | 25 | 18 | 9 | **239** |
| **USDC/USD** | 3 | 3 | 10 | 12 | 3 | 4 | 6 | 1 | - | - | 1 | 1 | - | **44** |
| **USDG/USD** | - | - | - | - | - | - | - | - | - | - | 12639 | 10365 | 2176 | **25180** |
| **USDG/USDC** | - | - | - | - | - | - | - | - | - | - | 13 | - | - | **13** |
| **VTI/USD** | - | - | - | - | - | - | - | - | - | - | 1 | 1 | - | **2** |
| **XLM/USD** | - | - | - | - | 18 | 1 | - | - | - | - | - | - | - | **19** |
| **XMR/USD** | 84 | 4 | 2 | 1 | - | - | - | - | - | - | - | - | - | **91** |
| **XRP/USD** | 302 | 38 | 8 | 334 | 142 | 95 | 44 | 14 | 1 | - | - | - | - | **978** |
| **TOTAL** | **1853** | **849** | **1485** | **1009** | **1348** | **797** | **498** | **296** | **550** | **282** | **12995** | **10739** | **2230** | **34931** |

## Asset Analysis

| Symbol | Gains | Volume(USD) | Gains/Vol% | Trades | Avg Gain |
|--------|--------|-------------|------------|--------|----------|
| BTC/USD | $485.73 | $165505 | 0.29% | 2794 | $0.17 |
| BTC/USD | $485.73 | $165505 | 0.29% | 2794 | $0.17 |
| ETH/USD | $179.36 | $13275 | 1.35% | 1164 | $0.15 |
| SOL/USD | $155.36 | $24727 | 0.63% | 2392 | $0.06 |
| ADA/USD | $125.82 | $14491 | 0.87% | 978 | $0.13 |
| USDG/USD | $80.97 | $1415653 | 0.01% | 25180 | $0.00 |
| XRP/USD | $54.42 | $9968 | 0.55% | 978 | $0.06 |
| KSM/USD | $37.64 | $1451 | 2.59% | 115 | $0.33 |
| TRX/USD | $30.59 | $3613 | 0.85% | 239 | $0.13 |
| DOT/USD | $12.68 | $824 | 1.54% | 68 | $0.19 |
| INJ/USD | $12.08 | $1001 | 1.21% | 71 | $0.17 |
| XMR/USD | $4.20 | $507 | 0.83% | 91 | $0.05 |
| FET/USD | $4.05 | $542 | 0.75% | 130 | $0.03 |
| EURR/USD | $0.88 | $1278 | 0.07% | 17 | $0.05 |
| VTI/USD | $0.07 | $440 | 0.02% | 2 | $0.04 |
| SCHD/USD | $0.00 | $1019 | 0.00% | 1 | $0.00 |
| QQQ/USD | $0.00 | $340 | 0.00% | 1 | $0.00 |
| USDG/USDC | $-0.01 | $935 | -0.00% | 13 | $-0.00 |
| SUI/USD | $-2.99 | $679 | -0.44% | 61 | $-0.05 |
| XLM/USD | $-7.52 | $255 | -2.95% | 19 | $-0.40 |
| USDC/USD | $-11.52 | $6017 | -0.19% | 44 | $-0.26 |
| LINK/USD | $-14.91 | $1661 | -0.90% | 144 | $-0.10 |
| AVAX/USD | $-63.59 | $2872 | -2.21% | 429 | $-0.15 |
| **TOTAL** | **$1083.30** | **$1667052** | **0.06%** | **34931** | **$0.03** |