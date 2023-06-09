# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

SHENYU_HOME :=  "."
VERSION ?= "2.6.0-SNAPSHOT"

REGISTRY ?= "docker.io"
REPOSITORY_PREF ?= "apache/shenyu"
ADMIN_REPOSITORY ?= "${REPOSITORY_PREF}-admin"
BOOTSTRAP_REPOSITORY ?= "${REPOSITORY_PREF}-bootstrap"

TAG ?= latest
COMMIT_ID := $(shell git rev-parse HEAD)

default: build-all-image

build-all: build-admin build-bootstrap
	@echo "build all"

build-admin:
	@echo "build admin"
	@${SHENYU_HOME}/mvnw -am \
		-pl shenyu-dist/shenyu-admin-dist \
		-Dmaven.javadoc.skip=true \
		-Drat.skip=true \
		-Djacoco.skip=true \
		-DskipTests \
		-Prelease \
		clean package

build-bootstrap:
	@echo "build bootstrap"
	@${SHENYU_HOME}/mvnw -am \
		-pl shenyu-dist/shenyu-bootstrap-dist \
		-Dmaven.javadoc.skip=true \
		-Drat.skip=true \
		-Djacoco.skip=true \
		-DskipTests \
		-Prelease \
		clean package

build-all-image: build-admin-image build-bootstrap-image ## build images on local, does not support multi platforms.
	@echo "build images"

build-admin-image: build-admin ## build admin image for local
	@echo "build admin image"
	@docker buildx build --load \
		-t ${REGISTRY}/${ADMIN_REPOSITORY}:${TAG} \
		-f ${SHENYU_HOME}/shenyu-dist/shenyu-admin-dist/docker/Dockerfile \
		--build-arg APP_NAME=apache-shenyu-${VERSION}-admin-bin \
		--label "commit.id=${COMMIT_ID}" \
		${SHENYU_HOME}/shenyu-dist/shenyu-admin-dist

build-bootstrap-image: build-bootstrap ## build bootstrap image for local
	@echo "build bootstrap image"
	@docker buildx build --load \
		-t ${REGISTRY}/${BOOTSTRAP_REPOSITORY}:${TAG} \
		-f ${SHENYU_HOME}/shenyu-dist/shenyu-bootstrap-dist/docker/Dockerfile \
		--build-arg APP_NAME=apache-shenyu-${VERSION}-bootstrap-bin \
		--label "commit.id=${COMMIT_ID}" \
		${SHENYU_HOME}/shenyu-dist/shenyu-bootstrap-dist

publish-all-images: init publish-admin-image publish-bootstrap-image ## build and publish images on buildx, for producing multi platform images
	@docker buildx rm shenyu

.PHONY: init
init:
	@docker buildx create --name shenyu
	@docker buildx use shenyu

publish-admin-image: build-admin
	@echo "build and push admin image"
	@docker buildx build --push \
		--platform=linux/arm64,linux/amd64 \
		-t ${REGISTRY}/${ADMIN_REPOSITORY}:latest \
		-t ${REGISTRY}/${ADMIN_REPOSITORY}:${VERSION} \
		--build-arg APP_NAME=apache-shenyu-${VERSION}-admin-bin \
		--label "commit.id=${COMMIT_ID}" \
		-f ${SHENYU_HOME}/shenyu-dist/shenyu-admin-dist/docker/Dockerfile \
		${SHENYU_HOME}/shenyu-dist/shenyu-admin-dist

publish-bootstrap-image: build-bootstrap
	@echo "build and push bootstrap image"
	@docker buildx build --push \
		--platform=linux/arm64,linux/amd64 \
		-t ${REGISTRY}/${BOOSTRAP_REPOSITORY}:latest \
		-t ${REGISTRY}/${BOOSTRAP_REPOSITORY}:${VERSION} \
		--build-arg APP_NAME=apache-shenyu-${VERSION}-bootstrap-bin \
		--label "commit.id=${COMMIT_ID}" \
		-f ${SHENYU_HOME}/shenyu-dist/shenyu-bootstrap-dist/docker/Dockerfile \
		${SHENYU_HOME}/shenyu-dist/shenyu-bootstrap-dist
