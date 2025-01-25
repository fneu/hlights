# ─────────────────────────────────────────────────────────────────────────────
#  1. Builder Stage
# ─────────────────────────────────────────────────────────────────────────────
FROM haskell:9.6.5 AS builder

# Create and set working directory
WORKDIR /usr/src/hlights

# Copy the cabal files first for dependency caching
COPY hlights.cabal cabal.project ./
# (Optionally also copy cabal.project.freeze if you create it for reproducible builds)
COPY cabal.project.freeze ./

# Update package list and install all dependencies
RUN cabal update
RUN cabal build --only-dependencies

# Now copy the rest of the source code
COPY . .

# Build and install the `hlights` executable into /usr/local/bin
RUN cabal install exe:hlights --installdir=/usr/local/bin --install-method=copy

# ─────────────────────────────────────────────────────────────────────────────
#  2. Runtime Stage
# ─────────────────────────────────────────────────────────────────────────────
FROM debian:bullseye-slim

# We need libgmp for Haskell binaries
RUN apt-get update && \
    apt-get install -y --no-install-recommends libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

# Create a directory for your app (or static files, logs, etc. if needed)
WORKDIR /opt/hlights

# Copy over the `hlights` executable from the builder
COPY --from=builder /usr/local/bin/hlights /usr/local/bin/hlights

# If you have static assets that must be included at runtime, copy them too:
COPY static ./static

# Expose whichever port hlights listens on (if needed)
EXPOSE 3000

# By default, start the hlights application
CMD ["hlights"]
