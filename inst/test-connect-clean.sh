#!/bin/bash
docker-compose -f ./test-connect.yml down
docker-compose -f ./test-connect.yml up -d
