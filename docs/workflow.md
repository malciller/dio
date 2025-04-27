<!-- Mermaid.js Diagram - https://mermaid.live ; copy from below the comment, and paste into viewer. -->

graph TD
    A[bin/dio.ml] --> A1[main]
    A1 --> A2[setup_logging]
    A1 --> A3[read_config]
    A1 --> A4[Dotenv.export]
    A1 --> A5[Kraken.Token.get_token]
    A5 --> A5a[load_env_file]
    A5 --> A5b[get_api_credentials]
    A5 --> A5c[sign request]
    A5 --> A5d[POST /0/private/GetWebSocketsToken]
    A1 --> A6[create strategy]
    A1 --> A7[create router]
    A1 --> B[Engine.run]
    A3 -->|kraken_grid_config.json| A8[runtime_cfg: assets, debounce_ms, queues_cap]
    A3 -->|kraken_grid_config.json| A9[core_cfg: ws_host, ws_port, ws_path, symbols, auth_token]
    A4 -->|.env| A5
    A5 -->|auth_token| A9
    A6 -->|strategy| B
    A7 -->|router| B
    A8 -->|runtime_cfg| B
    A9 -->|core_cfg| B
    B --> B1[create buffers with queues_cap]
    B1 --> G[tick_buffer: Ringbuffer.t]
    B1 --> H[cmd_buffer: Ringbuffer.t]
    B1 --> I[exec_buffer: Ringbuffer.t]
    B --> B2[start_feed]
    B --> C[Supervisor.start]
    
    subgraph "Supervisor"
        C --> C1[feed_fut]
        C --> C2[strat_fut]
        C --> C3[router_fut]
        C1 --> D[Feed]
        C2 --> E[Strategy]
        C3 --> F[Router]
        C -->|runtime_cfg| C2
        C -->|core_cfg| C1
        C -->|core_cfg| C2
        C -->|core_cfg| C3
        C -->|tick_buffer| C2
        C -->|exec_buffer| C2
        C -->|cmd_buffer| C2
        C -->|cmd_buffer| C3
        C -->|exec_buffer| C3
        C --> C4[Lwt.join]
        C1 --> C4
        C2 --> C4
        C3 --> C4
    end

    subgraph "Ring Buffers"
        G[tick_buffer: Ringbuffer.t]
        H[cmd_buffer: Ringbuffer.t]
        I[exec_buffer: Ringbuffer.t]
    end

    subgraph "Feed Component"
        D --> B2
        B2 --> B3[push_tick_to_buffer]
        B2 --> B4[push_execs_to_buffer]
        B2 --> J1[Feed.Prod.start]
        B2 --> J2[Feed.Prod.start_executions]
        J1 --> K[Public Feed]
        J2 --> J3[check auth_token]
        J3 -->|Some| L[Auth Feed]
        J3 -->|None| J4[log warning]
        K -->|tick_event| B3
        L -->|market_event| B4
        B3 -->|tick_event| G
        B4 -->|market_event| I

        subgraph "Public Feed"
            K --> KP[connect]
            KP -->|wss://ws.kraken.com/v2| KQ[start]
            KQ -->|subscribe ticker| KR[make_subscribe_message]
            KQ -->|subscribe instrument| KR
            KQ --> KS[handle_public_frame]
            KR -->|Frame| KS
            KS -->|ticker_data| KT[on_tick]
            KS -->|instrument_data| KX[update instrument_precisions]
            KS -->|heartbeat| KU[pong]
            KS -->|status| KV[log_status]
            KS -->|subscription_response| KZ[log_subscription]
            KT -->|tick_event| B3
            KX --> KY[instrument_precisions]
            KX --> KZ2[resolve instruments_loaded]
        end

        subgraph "Auth Feed"
            L --> LP[connect]
            LP -->|wss://ws-auth.kraken.com/v2| LQ[start_executions]
            LQ -->|subscribe executions| LR[make_subscribe_message]
            LQ --> LS[handle_auth_frame]
            LR -->|Frame| LS
            LS -->|execution_report snapshot| LT1[update orders]
            LS -->|execution_report update| LT2[execution_report_to_market_event]
            LT1 --> LV[open_buy_orders]
            LT1 --> LW[pending_orders]
            LT1 --> LZ2[resolve snapshot_processed]
            LT2 --> LU[on_execution]
            LU -->|market_event| B4
            LS -->|heartbeat| LX[pong]
            LS -->|status| LY[log_status]
            LS -->|subscription_response| LZ[log_subscription]
        end
    end

    subgraph "Strategy Component"
        E --> M[Grid Strategy]
        M --> M1[Strategy.start]
        M1 --> M2[wait_for_snapshot]
        M1 --> M3[wait_for_instruments]
        M1 --> M4[initialize_orders]
        M1 --> M5[loop]
        M2 -->|snapshot_processed| M5
        M3 -->|instruments_loaded| M5
        M4 -->|core_cfg| M5
        M5 --> M6[process exec_buffer]
        M5 --> M7[process tick_buffer]
        M6 --> M8[handle_execution]
        M7 --> M9[update_price]
        M7 --> M10[sync_open_orders]
        M7 --> M11[check_and_adjust_orders]
        M7 --> M12[create_initial_orders]
        M8 --> M11
        M8 --> M12
        M9 --> M13[price_info]
        M10 --> LV
        M11 --> LV
        M12 --> M14[open_orders]
        M12 --> M15[initialized_symbols]
        M12 --> KY
        G -- ticks --> M7
        I -- execs --> M6
        M8 -- order_cmds --> H
        M11 -- order_cmds --> H
        M12 -- order_cmds --> H
        M8 --> M14
        M10 --> M14
    end

    subgraph "Router Component"
        F --> N[Router]
        N --> N1[Router.start]
        N1 --> N2[cmd_loop]
        N2 --> N3[OrderCache.cleanup]
        N2 --> N4[OrderCache.is_duplicate]
        N2 --> N5[Kraken.handle_order]
        N4 --> N6[recent_orders]
        N5 --> N7[Kraken.Ws_exec.handle_router_command]
        N7 --> N8[connect]
        N7 --> N9[enqueue cmd]
        N7 --> N10[start_loop]
        N8 -->|wss://ws-auth.kraken.com/v2| N10
        N9 --> N11[cmd_queue]
        N10 --> N12[send_order_command]
        N10 --> N13[handle_message]
        N12 --> KY
        N13 -->|Ack| I
        H -- cmds --> N2
        N9 --> N11
        N12 --> N11
        N13 --> N11
    end

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style A1 fill:#f9f,stroke:#333,stroke-width:1px
    style A8 fill:#ffb,stroke:#333,stroke-width:2px
    style A9 fill:#ffb,stroke:#333,stroke-width:2px
    style B fill:#bbf,stroke:#333,stroke-width:2px
    style C fill:#bbf,stroke:#333,stroke-width:2px
    style G fill:#dfd,stroke:#333,stroke-width:2px
    style H fill:#dfd,stroke:#333,stroke-width:2px
    style I fill:#dfd,stroke:#333,stroke-width:2px
    style J1 fill:#bbf,stroke:#333,stroke-width:1px
    style J2 fill:#bbf,stroke:#333,stroke-width:1px
    style J3 fill:#bbf,stroke:#333,stroke-width:1px
    style J4 fill:#ffb,stroke:#333,stroke-width:1px
    style K fill:#bbf,stroke:#333,stroke-width:1px
    style L fill:#bbf,stroke:#333,stroke-width:1px
    style LV fill:#ffb,stroke:#333,stroke-width:1px
    style LW fill:#ffb,stroke:#333,stroke-width:1px
    style KY fill:#ffb,stroke:#333,stroke-width:1px
    style M fill:#bbf,stroke:#333,stroke-width:2px
    style M13 fill:#ffb,stroke:#333,stroke-width:1px
    style M14 fill:#ffb,stroke:#333,stroke-width:1px
    style M15 fill:#ffb,stroke:#333,stroke-width:1px
    style N fill:#bbf,stroke:#333,stroke-width:2px
    style N6 fill:#ffb,stroke:#333,stroke-width:1px
    style N11 fill:#ffb,stroke:#333,stroke-width:1px