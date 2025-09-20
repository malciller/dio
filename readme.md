# Dio

[![OCaml](https://img.shields.io/badge/Language-OCaml-blue.svg)](https://ocaml.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

OCaml trading engine for Kraken. Uses WebSocket and REST to run automated strategies with a terminal dashboard and optional Discord notifications.

## Requirements

- OCaml 4.14+ (opam)
- Kraken API key/secret
- macOS/Linux/WSL

## Quick Start

### Install
```bash
git clone https://github.com/malciller/dio.git
cd dio
opam install . --deps-only
dune build
```

### Configure
Create `.env`:
```bash
KRAKEN_API_KEY=your_kraken_api_key
KRAKEN_API_SECRET=your_kraken_api_secret
DISCORD_WEBHOOK_URL=your_discord_webhook_url
```

Edit `_config.json` (example):
```json
{
  "assets": [
    { "symbol": "BTC/USD",  // Pair to Trade
      "qty": "0.001",  // Base asset amount per trade
      "grid_interval": "1.0",  // Distance between buy and sell orders (profit spread)
      "sell_mult": "0.999", // Base asset amount sold multiplier (qty * sell_mult = sell order qty)
      "strategy": "Grid"  // Strategy Name (Grid/MM)
    },
    { "symbol": "USDT/USD",  // Pair to Trade
      "qty": "100.0",  // Base asset amount per trade
      "min_usd_balance": "500.0",  // Minimum USD balance for this pair and strategy to run
      "strategy": "MM"  // Strategy Name (Grid/MM)
     }
  ],
  "queues_cap": 1000,  // Maximum queue size allocated
  "profit_threshold_pct": 0.0010  // Arbitrage strategy profit threshold (percentage)
}
```

### Run
```bash
./_build/default/bin/dio.exe --dashboard   # with UI
./_build/default/bin/dio.exe               # headless
```

## Strategies

- GRID: Maintains buy/sell ladders around price with configurable spacing and size.
- MM: Quotes at top-of-book using fixed sizes.
- ARB: **Strategy runs automatically, does not require a config.** Finds triangle/longer cycles and single-pair spreads; sizes by liquidity and balances, runs for all assets active with other strategies.

## Architecture

- Engine: Orchestrates strategies and data flow.
- Router: Executes orders via Kraken (REST/WS) and manages auth.
- Feed: Streams market data via WebSocket.
- Dashboard: CLI for prices, orders, and logs.

## Dashboard

![Dio Dashboard](.github/readme/image.png) 
## Notification

### Balance Updates
![Balances](.github/readme/balances.png)

### Order Execution Alerts
![Buy Alert](.github/readme/buy.png)
![Sell Alert](.github/readme/sell.png)

## Development

```bash
# Build
dune build

# Tests
dune test

# Format / Docs
dune fmt
dune build @doc
```

## Logging

- Debug, Info, Warning, Error — timestamped by component.

## Contributing

1. Fork
2. Create branch (`git checkout -b feature/xyz`)
    - Trading logic found in src/dio_engine/trade_strategies/
3. Commit (`git commit -m "..."`)
4. Push (`git push origin feature/xyz`)
5. Open PR

Guidelines: add tests, update docs, keep CI green.

## License

MIT — see `LICENSE`.

Legal: Provided “as is”, without warranty. Trading involves risk.