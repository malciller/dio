graph TD
    A[bin/dio.ml] --> B[Engine.run]
    B --> C[Supervisor.start]
    
    subgraph "Supervisor"
        C --> D[Feed]
        C --> E[Strategy]
        C --> F[Router]
    end

    subgraph "Ring Buffers"
        G[tick_buffer]
        H[cmd_buffer]
        I[exec_buffer]
    end

    subgraph "Feed Component"
        D --> J[Kraken ws_feed]
        J --> K[Public Feed]
        J --> L[Auth Feed]

        subgraph "Public Feed"
            K --> KP[connect]
            KP -->|wss://ws.kraken.com/v2| KQ[start]
            KQ -->|subscribe ticker| KR[make_subscribe_message]
            KQ --> KS[handle_public_frame]
            KR -->|Frame| KS
            KS -->|ticker_data| KT[on_tick]
            KT -->|tick_event| G
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
            LU -->|market_event| I
            LS -->|update open_buy_orders| LV[open_buy_orders]
            LS -->|update pending_orders| LW[pending_orders]
            LS -->|heartbeat| LX[pong]
            LS -->|status| LY[log_status]
            LS -->|subscription_response| LZ[log_subscription]
        end
    end

    subgraph "Strategy Component"
        E --> M[Grid Strategy]
        G -- ticks --> M
        I -- execs --> M
        M -- order_cmds --> H
    end

    subgraph "Router Component"
        F --> N[Router]
        N --> O[ws_exec]
        H -- cmds --> N
    end

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style G fill:#dfd,stroke:#333,stroke-width:2px
    style H fill:#dfd,stroke:#333,stroke-width:2px
    style I fill:#dfd,stroke:#333,stroke-width:2px
    style J fill:#bbf,stroke:#333,stroke-width:2px
    style K fill:#bbf,stroke:#333,stroke-width:1px
    style L fill:#bbf,stroke:#333,stroke-width:1px
    style LV fill:#ffb,stroke:#333,stroke-width:1px
    style LW fill:#ffb,stroke:#333,stroke-width:1px