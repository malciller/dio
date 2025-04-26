graph TD
    A[bin/dio.ml] --> A1[main]
    A1 --> A2[setup_logging]
    A1 --> A3[read_config]
    A1 --> A4[Dotenv.export]
    A1 --> A5[Kraken.Token.get_token]
    A1 --> B[Engine.run]
    A3 -->|config.json| A8[config]
    A4 -->|.env| A8
    A5 -->|auth_token| A8
    A6 -->|strategy| B
    A8 -->|config| B
    B --> B1[create buffers]
    B1 --> G[tick_buffer]
    B1 --> H[cmd_buffer]
    B1 --> I[exec_buffer]
    B --> B2[start_feed]
    B --> C[Supervisor.start]
    
    subgraph "Supervisor"
        C --> C1[feed_fut]
        C --> C2[strat_fut]
        C --> C3[router_fut]
        C --> C5[monitor exec_buffer]
        C1 --> D[Feed]
        C2 --> E[Strategy]
        C3 --> F[Router]
        C -->|config| D
        C -->|config| E
        C -->|config| F
        C -->|tick_buffer| E
        C -->|exec_buffer| E
        C -->|cmd_buffer| E
        C -->|cmd_buffer| F
        C -->|exec_buffer| C5
        C --> C4[Lwt.join]
        C1 --> C4
        C2 --> C4
        C3 --> C4
        %% Note: Router initialized here via router_fut, not in dio.ml
    end

    subgraph "Ring Buffers"
        G[tick_buffer]
        H[cmd_buffer]
        I[exec_buffer]
    end

    subgraph "Feed Component"
        D --> B2
        B2 --> J[Kraken ws_feed]
        B2 --> B3[push_tick_to_buffer]
        B2 --> B4[push_execs_to_buffer]
        J --> K[Public Feed]
        J --> L[Auth Feed]
        K -->|tick_event| B3
        L -->|market_event| B4
        B3 -->|tick_event| G
        B4 -->|market_event| I

        subgraph "Public Feed"
            K --> KP[connect]
            KP -->|wss://ws.kraken.com/v2| KQ[start]
            KQ -->|subscribe ticker| KR[make_subscribe_message]
            KQ --> KS[handle_public_frame]
            KR -->|Frame| KS
            KS -->|ticker_data| KT[on_tick]
            KT -->|tick_event| B3
            KS -->|heartbeat| KU[pong]
            KS -->|status| KV[log_status]
        end

        subgraph "Auth Feed"
            L --> LP[connect]
            LP -->|wss://ws-auth.kraken.com/v2| LQ[start_executions]
            LQ -->|subscribe executions| LR[make_subscribe_message]
            LQ --> LS[handle_auth_frame]
            LR -->|Frame| LS
            LS -->|execution_report| LT[execution_report_to_market_event]
            LT -->|market_event| LU[on_execution]
            LU -->|market_event| B4
            LS -->|update open_buy_orders| LV[open_buy_orders]
            LS -->|update pending_orders| LW[pending_orders]
            LS -->|heartbeat| LX[pong]
            LS -->|status| LY[log_status]
            LS -->|subscription_response| LZ[log_subscription]
        end
    end

    subgraph "Strategy Component"
        E --> M[Grid Strategy]
        M --> M1[Strategy.start]
        G -- ticks --> M1
        I -- execs --> M1
        M1 -- order_cmds --> H
    end

    subgraph "Router Component"
        F --> N[Router]
        N --> N1[Router.start]
        N1 --> O[ws_exec]
        H -- cmds --> N1
    end

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style A1 fill:#f9f,stroke:#333,stroke-width:1px
    style A8 fill:#ffb,stroke:#333,stroke-width:2px
    style B fill:#bbf,stroke:#333,stroke-width:2px
    style C fill:#bbf,stroke:#333,stroke-width:2px
    style G fill:#dfd,stroke:#333,stroke-width:2px
    style H fill:#dfd,stroke:#333,stroke-width:2px
    style I fill:#dfd,stroke:#333,stroke-width:2px
    style J fill:#bbf,stroke:#333,stroke-width:2px
    style K fill:#bbf,stroke:#333,stroke-width:1px
    style L fill:#bbf,stroke:#333,stroke-width:1px
    style LV fill:#ffb,stroke:#333,stroke-width:1px
    style LW fill:#ffb,stroke:#333,stroke-width:1px
    style M fill:#bbf,stroke:#333,stroke-width:2px
    style N fill:#bbf,stroke:#333,stroke-width:2px