#!/bin/bash
export CONNECT_VERSION=1.7.8-7
docker-compose -f ./test-connect.yml up -d
