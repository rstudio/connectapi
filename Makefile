#!make
PWD := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

PROJECT=connectapi
NETWORK=${PROJECT}_default
RSC_VERSION=1.8.0.1-14

#---------------------------------------------
# Helpers
#---------------------------------------------
mail-up:
	NETWORK=${NETWORK} \
	docker-compose -f inst/ci/mail.yml -f inst/ci/make-network.yml up -d

mail-down:
	NETWORK=${NETWORK} \
	docker-compose -f inst/ci/mail.yml -f inst/ci/make-network.yml down
	
connect-up:
	NETWORK=${NETWORK} \
	RSC_LICENSE=$(RSC_LICENSE) \
	RSC_VERSION=$(RSC_VERSION) \
	docker-compose -f inst/ci/test-connect.yml -f inst/ci/make-network.yml up -d

connect-down:
	NETWORK=${NETWORK} \
	docker-compose -f inst/ci/test-connect.yml -f inst/ci/make-network.yml down
	
connect-file-up:
	NETWORK=${NETWORK} \
	RSC_LICENSE=$(RSC_LICENSE) \
	RSC_VERSION=$(RSC_VERSION) \
	docker-compose -f inst/ci/test-connect-lic.yml -f inst/ci/make-network.yml up -d

connect-file-down:
	NETWORK=${NETWORK} \
	docker-compose -f inst/ci/test-connect-lic.yml -f inst/ci/make-network.yml down
