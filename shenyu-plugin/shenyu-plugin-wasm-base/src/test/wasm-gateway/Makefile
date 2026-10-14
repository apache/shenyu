#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

WAT2WASM ?= wat2wasm
RESOURCES := ../resources
PREFIX := org.apache.shenyu.plugin.wasm.base.gateway.

.PHONY: build
build:
	$(WAT2WASM) reference-plugin.wat -o $(RESOURCES)/$(PREFIX)ReferenceWasmPlugin.wasm
	$(WAT2WASM) configuration-handler.wat -o $(RESOURCES)/$(PREFIX)ReferenceWasmPluginDataHandler.wasm
	$(WAT2WASM) missing-hooks.wat -o '$(RESOURCES)/$(PREFIX)WasmGatewayTest$$MissingExecutePlugin.wasm'
	$(WAT2WASM) missing-hooks.wat -o '$(RESOURCES)/$(PREFIX)WasmGatewayTest$$MissingHooksHandler.wasm'
	$(WAT2WASM) trap.wat -o '$(RESOURCES)/$(PREFIX)WasmGatewayTest$$TrappingPlugin.wasm'
	$(WAT2WASM) invalid-memory.wat -o '$(RESOURCES)/$(PREFIX)WasmGatewayTest$$InvalidMemoryPlugin.wasm'
	$(WAT2WASM) missing-hooks.wat -o '$(RESOURCES)/$(PREFIX)WasmLifecycleContractTest$$MissingMetadataHandler.wasm'
	$(WAT2WASM) missing-hooks.wat -o '$(RESOURCES)/$(PREFIX)WasmLifecycleContractTest$$MissingDiscoveryHandler.wasm'
	$(WAT2WASM) invalid-hook.wat -o '$(RESOURCES)/$(PREFIX)WasmLifecycleContractTest$$InvalidHookLoader.wasm'
	$(WAT2WASM) invalid-initializer.wat -o '$(RESOURCES)/$(PREFIX)WasmLifecycleContractTest$$InvalidInitializerLoader.wasm'
	$(WAT2WASM) incompatible-result.wat -o '$(RESOURCES)/$(PREFIX)WasmGatewayTest$$IncompatibleResultPlugin.wasm'
