#!/bin/bash
export CONNECT_VERSION=$(cat CONNECT_VERSION)
docker-compose -f ./test-connect.yml down
docker-compose -f ./test-connect.yml up -d
