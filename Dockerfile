FROM haskell:9.6.5

# Create and set working directory
WORKDIR /usr/src/hlights

# Copy the cabal files first for dependency caching
COPY hlights.cabal cabal.project ./
COPY cabal.project.freeze ./

# Update package list and install all dependencies
RUN cabal update
RUN cabal build --only-dependencies

# Now copy the rest of the source code
COPY . .

# Build and install the `hlights` executable into /usr/local/bin
RUN cabal install exe:hlights --installdir=/usr/local/bin --install-method=copy

# (Optional) If you rely on system tzdata or other packages, install them here:
# RUN apt-get update && apt-get install -y --no-install-recommends tzdata

# If you have static assets, you can keep them in /usr/src/hlights/static
# or move them somewhere else as needed. By default, we stay in /usr/src/hlights
# so the binary can find them if that's how your code is structured.

EXPOSE 3000

CMD ["hlights"]
