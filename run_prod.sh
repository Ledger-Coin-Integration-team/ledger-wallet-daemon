#!/usr/bin/env bash
# Run driver (disable admin port and run from stage)
./target/universal/stage/bin/wallet-daemon -http.port=':9200' -admin.port=':0'
