/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.wasm.base.gateway;

import com.dylibso.chicory.wasm.InvalidException;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmInitException;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmDiscoveryHandler;
import org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmMetaDataHandler;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Verifies optional and required lifecycle exports against actual Wasm modules.
 */
public final class WasmLifecycleContractTest {

    @Test
    public void absentMetadataOperationsAreRequiredButRefreshIsOptional() {
        try (MissingMetadataHandler handler = new MissingMetadataHandler()) {
            assertThrows(ShenyuWasmInitException.class, () -> handler.handle(new MetaData()));
            assertThrows(ShenyuWasmInitException.class, () -> handler.remove(new MetaData()));
            handler.refresh();
            assertFalse(handler.getWasmExtern("_initialize").isPresent());
        }
    }

    @Test
    public void absentDiscoveryOperationIsRequired() {
        try (MissingDiscoveryHandler handler = new MissingDiscoveryHandler()) {
            assertThrows(ShenyuWasmInitException.class, () -> handler.handlerDiscoveryUpstreamData(new DiscoverySyncData()));
        }
    }

    @Test
    public void exportOfTheWrongKindIsNotAnAbsentOptionalFunction() {
        try (InvalidHookLoader loader = new InvalidHookLoader()) {
            assertThrows(InvalidException.class, () -> loader.getWasmExtern("before"));
        }
    }

    @Test
    public void invalidInitializerIsNotIgnored() {
        assertThrows(InvalidException.class, InvalidInitializerLoader::new);
    }

    /** Memory-only metadata fixture. */
    static final class MissingMetadataHandler extends AbstractWasmMetaDataHandler {
        @Override
        public String rpcType() {
            return "http";
        }

        @Override
        protected Long getArgumentId(final MetaData data) {
            return 1L;
        }
    }

    /** Memory-only discovery fixture. */
    static final class MissingDiscoveryHandler extends AbstractWasmDiscoveryHandler {
        @Override
        public String pluginName() {
            return ReferenceWasmPlugin.NAME;
        }

        @Override
        protected Long getArgumentId(final DiscoverySyncData data) {
            return 1L;
        }
    }

    /** Fixture exporting a global named before. */
    static final class InvalidHookLoader extends WasmLoader {
    }

    /** Fixture exporting a global named _initialize. */
    static final class InvalidInitializerLoader extends WasmLoader {
    }
}
