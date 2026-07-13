# syntax=docker/dockerfile:1.4
#
# Multistage build:
#   alire-base       OS packages (cached)
#   alire-toolchain  Alire + GNAT toolchain (cached, heavy)
#   build            compile AdaRaft with alr exec / gprbuild
#   runtime          minimal image with binary and GNAT runtime libs

# ---------------------------------------------------------------------------
# Base OS layer shared by toolchain and build stages.
# ---------------------------------------------------------------------------
FROM ubuntu:24.04 AS alire-base

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       ca-certificates curl unzip git build-essential \
    && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------
# Install Alire and the GNAT toolchain (slow; kept in its own stage).
# ---------------------------------------------------------------------------
FROM alire-base AS alire-toolchain

ARG ALIRE_VERSION=2.0.2

RUN curl -fsSL \
      "https://github.com/alire-project/alire/releases/download/v${ALIRE_VERSION}/alr-${ALIRE_VERSION}-bin-x86_64-linux.zip" \
      -o /tmp/alr.zip \
    && unzip -q /tmp/alr.zip -d /tmp/alr-extract \
    && install -m 0755 /tmp/alr-extract/bin/alr /usr/local/bin/alr \
    && rm -rf /tmp/alr.zip /tmp/alr-extract

ENV PATH="/usr/local/bin:${PATH}"

# Alire 2.x: download and select indexed toolchains (no --install flag).
RUN --mount=type=cache,target=/root/.cache/alire \
    alr -n toolchain --select --disable-assistant gnat_native \
    && alr -n toolchain --select --disable-assistant gprbuild

# ---------------------------------------------------------------------------
# Build the library and raft_server executable.
# ---------------------------------------------------------------------------
FROM alire-toolchain AS build

WORKDIR /app

# Manifests and generated Alire config (config/ is gitignored locally).
COPY alire.toml raft.gpr ./
COPY examples/docker/build-config/ config/

COPY src/ src/
COPY examples/ examples/

# Invoke gprbuild from the Alire toolchain directly (avoids alr exec syncing
# gnatprove from alire.toml).
RUN --mount=type=cache,target=/root/.cache/alire \
    bash -c 'set -euo pipefail; \
      export PATH="$(find /root/.local/share/alire/toolchains -type d -name bin \
        | tr "\n" ":")${PATH}"; \
      gprbuild -P raft.gpr -j"$(nproc)"; \
      gprbuild -P examples/raft_examples.gpr -j"$(nproc)" raft_server'

# Collect runtime shared libraries required by the binary.
RUN mkdir -p /runtime-libs \
    && ldd /app/examples/bin/raft_server \
         | awk '/=> \// {print $3}' \
         | sort -u \
         | xargs -r cp --parents -t /runtime-libs/

# ---------------------------------------------------------------------------
# Minimal runtime image.
# ---------------------------------------------------------------------------
FROM ubuntu:24.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       ca-certificates libgcc-s1 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /runtime-libs/ /
COPY --from=build /app/examples/bin/raft_server /usr/local/bin/raft_server
COPY examples/cluster.docker.toml /etc/raft/cluster.toml

WORKDIR /app

ENTRYPOINT ["raft_server"]
CMD ["--config", "/etc/raft/cluster.toml", "--server-id", "1"]
