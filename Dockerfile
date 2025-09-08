# ────────────────────────────────────────────────────────────────────────────────
# Dio – Dockerfile
# Ubuntu 22.04 base image + OCaml 4.14.2 + full native build
# Fixes "unknown scheme" by shipping /etc/services (via netbase package)
# ────────────────────────────────────────────────────────────────────────────────

# 1.  Base image
FROM ubuntu:22.04
ENV QEMU_CPU=host

# 2.  System dependencies
#     • netbase   → provides /etc/services so Conduit can resolve "https"
#     • libsqlite3-dev for caqti-driver-sqlite3
#     • libpq-dev for caqti-driver-postgresql
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    m4 \
    pkg-config \
    libffi-dev \
    libgmp-dev \
    libpcre3-dev \
    libssl-dev \
    libsqlite3-dev \
    libpq-dev \
    zlib1g-dev \
    make \
    g++ \
    git \
    curl \
    opam \
    ca-certificates \
    netbase \
 && rm -rf /var/lib/apt/lists/*

# 3.  OPAM + OCaml switch (4.14.2)
RUN opam init --disable-sandboxing --reinit -y \
 && opam switch create 4.14.0 ocaml-base-compiler.4.14.0 \
 && eval $(opam env --switch=4.14.0)

# 4.  Workdir inside the container
WORKDIR /app

# 5.  Copy project descriptors first (layer-cache friendly)
COPY --chown=root:root dio.opam dune-project ./

# 6.  Install OCaml dependencies
RUN eval $(opam env --switch=4.14.0) \
 && opam install -y . --deps-only --with-test

# 7.  Copy the rest of the source tree
COPY --chown=root:root . .

# 8.  Build native executable
RUN eval $(opam env --switch=4.14.0) \
 && dune build --profile=release bin/dio.exe \
 && cp _build/default/bin/dio.exe /usr/local/bin/dio

# 9.  Runtime PATH (opam binaries + app)
ENV PATH="/usr/local/bin:/root/.opam/4.14.0/bin:${PATH}"

# 10. Default command
CMD ["dio"]