#!/bin/bash
export RSC_VERSION=$(cat RSC_VERSION)
docker-compose -f ./test-connect.yml up -d
