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
        K -- on_tick --> G
        L -- on_execution --> I
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
