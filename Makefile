
.PHONY: all
all: build-client run-server

.PHONY: build-client
build-client:
	clear && \
	stack build -j8 --install-ghc

.PHONY: build-server
build-server:
	(cd server && stack install -j8 --install-ghc)

.PHONY: run-server
run-server: build-server
	~/.local/bin/lubeck-server &
