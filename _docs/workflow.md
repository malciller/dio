<!-- Go to https://mermaid.live/edit and paste below comment into viewer -->

graph TD
    A[bin/dio.ml] --> A1[main]
    A1 --> A2[Arg.parse]
    A2 -->|set mode_dash| A3[setup_logging]
    A2 -->|mode_dash=true| A4[dashboard_mode]
    A2 -->|mode_dash=false| A5[start_engine_logic]
    A3 --> A3a[create default_logger]
    A3a -->|mode_dash=true| A3b[Dashboard logger with timestamps]
    A3a -->|mode_dash=false| A3c[Stdout logger]
    A3 --> A3d[add logging rules: engine.*, kraken_ws_exec]
    A5 --> A6[read_config]
    A6 -->|kraken_grid_config.json| A7[runtime_cfg: assets, debounce_ms, queues_cap]
    A6 -->|kraken_grid_config.json| A8[engine_config: ws_host, ws_port, ws_path, symbols, auth_token, kraken_api_key, kraken_api_secret]
    A6 --> A9[Sys.getenv KRAKEN_API_KEY, KRAKEN_API_SECRET]
    A5 --> A10[Dotenv.export]
    A10 -->|.env| A11[Kraken.Token.get_token]
    A11 --> A11a[load_env_file]
    A11 --> A11b[get_api_credentials]
    A11 --> A11c[sign request]
    A11 --> A11d[POST /0/private/GetWebSocketsToken]
    A11 -->|auth_token| A8
    A5 --> A12[create grid_strategy]
    A5 --> A12a[create orderbook_strategy]
    A5 --> A13[create router]
    A5 --> A14[Engine.run]
    A7 -->|runtime_cfg| A14
    A8 -->|engine_config| A14
    A12 -->|grid_strategy| A14
    A12a -->|orderbook_strategy| A14
    A13 -->|router| A14
    A4 --> A4a[Dashboard.start]
    A4a --> A4a1[create_term]
    A4a --> A4a2[tick_loop]
    A4a --> A4a3[input_loop]
    A4a2 --> A4a4[render]
    A4a3 -->|quit| A4d[on_quit callback]
    A4a4 --> A4a5[stats_data]
    A4a4 --> KY
    A4 --> A4b[quit_promise]
    A4 --> A4c[Lwt_unix.on_signal SIGINT]
    A4d --> A4b
    A4c --> A4b
    A4b --> A4e[cleanup]
    A4e --> A4f[Lwt.cancel engine_promise]
    A4e --> A4g[Notty_lwt.Term.release]
    A4 --> A5

    A14 --> B[Engine.run]
    B --> B1[create buffers]
    B1 --> G[tick_buffer: Ringbuffer.t]
    B1 --> H[cmd_buffer: Ringbuffer.t]
    B1 --> I[exec_buffer: Ringbuffer.t]
    B --> B2[Supervisor.start]
    B2 -->|feed_initializer_fn| B3[start_feed]
    B3 --> B4[Feed.Prod.start]
    B3 --> B5[Feed.Prod.start_executions]
    B4 -->|on_tick| B6[push_tick_to_buffer]
    B5 -->|on_execution| B7[push_execs_to_buffer]
    B6 -->|tick_event| G
    B7 -->|market_event| I
    B2 -->|runtime_cfg| C
    B2 -->|engine_config| C
    B2 -->|grid_strategy| C
    B2 -->|orderbook_strategy| C
    B2 -->|router| C
    B2 -->|tick_buffer| C
    B2 -->|exec_buffer| C
    B2 -->|cmd_buffer| C

    subgraph "Supervisor"
        C --> C1[feed_fut]
        C --> C2[grid_strat_fut]
        C --> C2a[orderbook_strat_fut]
        C --> C3[router_fut]
        C1 --> D[Feed]
        C2 --> E1[Grid Strategy]
        C2a --> E2[Orderbook Strategy]
        C3 --> F[Router]
        C -->|runtime_cfg| C2
        C -->|runtime_cfg| C2a
        C -->|engine_config| C1
        C -->|engine_config| C2
        C -->|engine_config| C2a
        C -->|engine_config| C3
        C -->|tick_buffer| C2
        C -->|tick_buffer| C2a
        C -->|exec_buffer| C2
        C -->|exec_buffer| C2a
        C -->|cmd_buffer| C2
        C -->|cmd_buffer| C2a
        C -->|cmd_buffer| C3
        C -->|exec_buffer| C3
        C --> C4[Lwt.join]
        C1 --> C4
        C2 --> C4
        C2a --> C4
        C3 --> C4
    end

    subgraph "Ring Buffers"
        G[tick_buffer: Ringbuffer.t]
        H[cmd_buffer: Ringbuffer.t]
        I[exec_buffer: Ringbuffer.t]
    end

    subgraph "Feed Component"
        D --> B4
        D --> B5
        B4 --> B6
        B5 --> B7
        B6 -->|tick_event| G
        B7 -->|market_event| I
        B4 --> J1[Feed.Prod.start]
        B5 --> J2[Feed.Prod.start_executions]
        J1 --> K[Public Feed]
        J2 --> J3[check auth_token]
        J3 -->|Some| L[Auth Feed]
        J3 -->|None| J4[log warning]
        K -->|tick_event| B6
        L -->|market_event| B7

        subgraph "Public Feed"
            K --> K1[retry_loop]
            K1 --> K2[Lwt.catch]
            K2 --> K3[Kraken.Ws_feed.start]
            K2 --> K4[log error]
            K4 --> K5[Lwt_unix.sleep 5s]
            K5 --> K1
            K3 --> K6[connect]
            K3 --> K7[subscribe Ticker, Instrument]
            K3 --> K8[handle_public_frame]
            K6 --> K7
            K7 --> K8
            K8 -->|tick_event| B6
            K8 -->|instrument_precisions| KY
            K8 -->|instruments_loaded| KZ2
        end

        subgraph "Auth Feed"
            L --> L1[retry_loop]
            L1 --> L2[Lwt.catch]
            L2 --> L3[Kraken.Ws_feed.start_executions]
            L2 --> L4[log error]
            L4 --> L5[Lwt_unix.sleep 5s]
            L5 --> L1
            L3 --> L6[connect]
            L3 --> L7[subscribe Executions]
            L3 --> L8[handle_auth_frame]
            L6 --> L7
            L7 --> L8
            L8 -->|market_event| B7
            L8 -->|open_buy_orders| LV
            L8 -->|pending_orders| LW
            L8 -->|snapshot_processed| LZ2
        end
    end

    subgraph "Strategy Components"
        E1 --> M[Suicide Grid Strategy]
        E2 --> M20[Top Level Orderbook MM Strategy]
        
        subgraph "Suicide Grid (strategy/suicide_grid.ml)"
            M --> M1[Suicide_grid.start]
            M1 --> M2[wait_for_snapshot]
            M1 --> M3[wait_for_instruments]
            M1 --> M4[initialize_orders]
            M1 --> M5[loop]
            M2 -->|snapshot_processed| M5
            M3 -->|instruments_loaded| M5
            M4 -->|engine_config| M5
            M5 --> M6[process exec_buffer]
            M5 --> M7[process tick_buffer]
            M6 --> M8[handle_execution]
            M7 --> M9[check price changed]
            M9 -->|changed| M10[update_price]
            M9 -->|changed| M11[sync_open_orders]
            M9 -->|changed| M12[check_and_adjust_orders]
            M9 -->|changed| M13[create_initial_orders]
            M9 -->|changed| M16[verify_grid_spacing]
            M9 -->|unchanged| M5
            M8 --> M12
            M8 --> M13
            M8 --> M14[open_orders]
            M10 --> M15[price_info]
            M11 --> M14
            M11 --> M12
            M12 --> M14
            M12 -->|amend_cmds| H
            M13 --> M14
            M13 -->|add_cmds| H
            M13 --> M17[initialized_symbols]
            M13 --> KY
            M16 -->|amend_cmds| H
            G -- ticks --> M7
            I -- execs --> M6
            M4 --> M14
            M4 --> M17
            M11 --> LV
            M12 --> LV
            M16 --> LV
        end
        
        subgraph "Top Level Orderbook MM (strategy/top_level_orderbook_mm.ml)"
            M20 --> M21[Top_level_orderbook_mm.start]
            M21 --> M22[initialize_orderbook_state]
            M21 --> M23[main_loop]
            M22 --> M23
            M23 --> M24[process tick_buffer]
            M23 --> M25[process exec_buffer]
            M24 --> M26[analyze_top_level_book]
            M24 --> M27[update_market_state]
            M25 --> M28[handle_fill_events]
            M26 -->|top_level_changed| M29[adjust_orders]
            M27 --> M30[orderbook_state]
            M28 --> M29
            M29 -->|order_cmds| H
            M29 --> M30
            G -- ticks --> M24
            I -- execs --> M25
            M22 --> M30
            M28 --> M30
        end
    end

    subgraph "Router Component"
        F --> N[Router]
        N --> N1[Router.start]
        N1 --> N2[cmd_loop]
        N2 --> N3[OrderCache.cleanup]
        N2 --> N4[OrderCache.is_duplicate]
        N2 --> N5[log command]
        N2 --> N6[Kraken.handle_order]
        N4 -->|duplicate| N7[log warning]
        N4 -->|not duplicate| N5
        N5 --> N6
        N6 --> N8[Kraken.Kraken_exec.handle_router_command]
        N8 --> N10[send_order_command]
        N10 --> N11[REST POST]
        N10 --> N12[on_event]
        N3 --> N9[recent_orders]
        N4 --> N9
        N10 --> KY
        N12 -->|market_event| I
        H -- cmds --> N2
    end

    style A fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style A1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A5 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A7 fill:#a5f3fc,stroke:#000000,stroke-width:2px,color:#000000
    style A8 fill:#a5f3fc,stroke:#000000,stroke-width:2px,color:#000000
    style A14 fill:#1e3a8a,stroke:#000000,stroke-width:2px,color:#d1d5db
    style B fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style C fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style G fill:#4b5563,stroke:#000000,stroke-width:2px,color:#ffffff
    style H fill:#4b5563,stroke:#000000,stroke-width:2px,color:#ffffff
    style I fill:#4b5563,stroke:#000000,stroke-width:2px,color:#ffffff
    style J1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style J2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style J3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style J4 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style K fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style LV fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style LW fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style KY fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style KZ2 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style LZ2 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style M fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style M20 fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style M15 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style M14 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style M17 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style M30 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style N fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style N9 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style N10 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N11 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N12 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4a fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4a1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4a2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4a3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4a4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style A4a5 fill:#a5f3fc,stroke:#000000,stroke-width:1px,color:#000000
    style A12a fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style C2a fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style D fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style E1 fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style E2 fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style F fill:#333333,stroke:#000000,stroke-width:2px,color:#ffffff
    style B1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style B2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style B3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style B4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style B5 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style B6 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style B7 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style C1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style C2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style C3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style C4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K5 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K6 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K7 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style K8 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L5 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L6 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L7 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style L8 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M5 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M6 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M7 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M8 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M9 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M10 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M11 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M12 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M13 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M16 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M21 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M22 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M23 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M24 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M25 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M26 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M27 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M28 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style M29 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N1 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N2 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N3 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N4 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N5 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N6 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N7 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db
    style N8 fill:#1e3a8a,stroke:#000000,stroke-width:1px,color:#d1d5db