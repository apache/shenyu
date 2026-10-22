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

package org.apache.shenyu.plugin.wasm.api;

import com.dylibso.chicory.runtime.HostFunction;
import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Machine;
import com.dylibso.chicory.runtime.Store;
import com.dylibso.chicory.wasm.WasmModule;
import com.dylibso.chicory.wasm.types.FunctionType;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real compiler/interpreter guests verify deadlines, cancellation, memory and recovery.
 */
class WasmExecutionLimitsTest {

    private static final String PREFIX = "shenyu.wasm." + Guest.class.getName() + ".";

    private static final String INIT_PREFIX = "shenyu.wasm." + InitializationGuest.class.getName() + ".";

    private static final String MEMORY_FREE_PREFIX = "shenyu.wasm." + MemoryFreeGuest.class.getName() + ".";

    @BeforeEach
    void configure() {
        System.setProperty(PREFIX + "execution-timeout-millis", "300");
        System.setProperty(PREFIX + "initialization-timeout-millis", "10000");
        System.setProperty(PREFIX + "max-memory-pages", "2");
    }

    @AfterEach
    void clear() {
        for (String prefix : List.of(PREFIX, INIT_PREFIX, MEMORY_FREE_PREFIX)) {
            for (String setting : List.of("execution-timeout-millis", "initialization-timeout-millis", "max-memory-pages")) {
                System.clearProperty(prefix + setting);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void infiniteGuestStopsAndItsInstanceIsReplaced(final boolean interpreted) {
        try (WasmLoader loader = loader(interpreted, () -> { })) {
            ShenyuWasmException timeout = assertThrows(ShenyuWasmException.class,
                    () -> loader.getWasmExtern("spin").orElseThrow().apply());
            assertTrue(timeout.getMessage().contains("timed out"));
            assertEquals(7, loader.getWasmExtern("value").orElseThrow().apply(7)[0]);
            assertEquals(0, loader.getWasmExtern("read").orElseThrow().apply()[0],
                    "the poisoned instance's dirty memory must not be reused");
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void memoryGrowthCannotExceedTheConfiguredBudget(final boolean interpreted) {
        try (WasmLoader loader = loader(interpreted, () -> { })) {
            assertEquals(1, loader.getWasmExtern("grow").orElseThrow().apply(1)[0]);
            assertEquals(-1, loader.getWasmExtern("grow").orElseThrow().apply(1)[0]);
            assertEquals(9, loader.getWasmExtern("value").orElseThrow().apply(9)[0]);
        }
    }

    @Test
    void interruptionCancelsTheActualGuestWorker() throws Exception {
        final CountDownLatch entered = new CountDownLatch(1);
        final CountDownLatch exited = new CountDownLatch(1);
        ExecutorService caller = Executors.newSingleThreadExecutor();
        try (WasmLoader loader = loader(true, () -> {
            entered.countDown();
            try {
                new CountDownLatch(1).await();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                exited.countDown();
            }
        })) {
            Future<?> call = caller.submit(() -> loader.getWasmExtern("block").orElseThrow().apply());
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            call.cancel(true);
            assertTrue(exited.await(5, TimeUnit.SECONDS), "cancelling only the caller is insufficient");
            assertEquals(11, loader.getWasmExtern("value").orElseThrow().apply(11)[0]);
        } finally {
            caller.shutdownNow();
        }
    }

    @Test
    void interruptionIgnoringHostCannotCreateReplacementWorkerCapacity() throws Exception {
        final CountDownLatch release = new CountDownLatch(1);
        final CountDownLatch exited = new CountDownLatch(1);
        final AtomicInteger calls = new AtomicInteger();
        try (WasmLoader loader = loader(true, () -> {
            calls.incrementAndGet();
            try {
                while (release.getCount() != 0) {
                    awaitIgnoringInterruption(release);
                }
            } finally {
                exited.countDown();
            }
        })) {
            assertThrows(ShenyuWasmException.class, () -> loader.getWasmExtern("block").orElseThrow().apply());
            assertThrows(ShenyuWasmException.class, () -> loader.getWasmExtern("block").orElseThrow().apply());
            assertEquals(1, calls.get(), "the still-running host must retain its worker capacity");
            release.countDown();
            assertTrue(exited.await(5, TimeUnit.SECONDS));
            assertEquals(13, loader.getWasmExtern("value").orElseThrow().apply(13)[0]);
        } finally {
            release.countDown();
        }
    }

    @Test
    void guestInitializationHasItsOwnDeadline() {
        System.setProperty(INIT_PREFIX + "execution-timeout-millis", "300");
        System.setProperty(INIT_PREFIX + "initialization-timeout-millis", "100");
        ShenyuWasmException timeout = assertThrows(ShenyuWasmException.class,
                () -> new InterpretedLoader(InitializationGuest.class, null));
        assertTrue(timeout.getMessage().contains("timed out"));
    }

    @Test
    void initialMemoryAboveBudgetFailsConstruction() {
        String property = "shenyu.wasm." + LargeGuest.class.getName() + ".max-memory-pages";
        System.setProperty(property, "2");
        try {
            RuntimeException failure = assertThrows(RuntimeException.class, () -> new WasmLoader(LargeGuest.class, null, 1));
            assertTrue(failure.getMessage().contains("memory budget"));
        } finally {
            System.clearProperty(property);
        }
    }

    @Test
    void invalidLimitsFailBeforeConstruction() {
        System.setProperty(PREFIX + "execution-timeout-millis", "-1");
        assertThrows(IllegalArgumentException.class, () -> loader(true, () -> { }));
    }

    @ParameterizedTest
    @ValueSource(strings = {"execution-timeout-millis", "initialization-timeout-millis", "max-memory-pages"})
    void malformedPropertyNamesItsKeyAndValue(final String name) {
        System.setProperty(PREFIX + name, "8m");
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, () -> loader(true, () -> { }));
        assertTrue(failure.getMessage().contains(PREFIX + name));
        assertTrue(failure.getMessage().contains("8m"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void memoryFreeModuleRunsWithOrWithoutABudget(final boolean interpreted) {
        for (int pages : List.of(0, 2)) {
            System.setProperty(MEMORY_FREE_PREFIX + "max-memory-pages", Integer.toString(pages));
            AtomicReference<WasmLoader> binding = new AtomicReference<>();
            Consumer<Store> initializer = store -> store.addFunction(new HostFunction("test", "memory",
                    FunctionType.of(List.of(), List.of()), (instance, args) -> {
                        binding.get().getMemory();
                        return new long[0];
                    }));
            try (WasmLoader loader = interpreted ? new InterpretedLoader(MemoryFreeGuest.class, initializer)
                    : new WasmLoader(MemoryFreeGuest.class, initializer, 1)) {
                binding.set(loader);
                assertEquals(7, loader.getWasmExtern("value").orElseThrow().apply(7)[0]);
                assertThrows(IllegalStateException.class, loader::getMemory);
                RuntimeException failure = assertThrows(RuntimeException.class, () -> loader.getWasmExtern("memory").orElseThrow().apply());
                assertTrue(failure.getMessage().contains("memory not available"));
                assertEquals(9, loader.getWasmExtern("value").orElseThrow().apply(9)[0]);
            }
        }
    }

    private void awaitIgnoringInterruption(final CountDownLatch release) {
        try {
            release.await();
        } catch (InterruptedException ignored) {
            // Deliberately model a host operation that does not honor interruption.
        }
    }

    private WasmLoader loader(final boolean interpreted, final Runnable callback) {
        Consumer<Store> initializer = store -> store.addFunction(new HostFunction("test", "block",
                FunctionType.of(List.of(), List.of()), (instance, args) -> {
                    callback.run();
                    return new long[0];
                }));
        return interpreted ? new InterpretedLoader(Guest.class, initializer) : new WasmLoader(Guest.class, initializer, 1);
    }

    private static final class Guest {
    }

    private static final class InitializationGuest {
    }

    private static final class LargeGuest {
    }

    private static final class MemoryFreeGuest {
    }

    private static final class InterpretedLoader extends WasmLoader {
        private InterpretedLoader(final Class<?> guest, final Consumer<Store> initializer) {
            super(guest, initializer, 1);
        }

        @Override
        protected Function<Instance, Machine> compileMachineFactory(final WasmModule module) {
            return null;
        }
    }
}

