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

import com.dylibso.chicory.runtime.ExportFunction;
import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Machine;
import com.dylibso.chicory.wasm.WasmModule;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.function.LongFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression tests for issue #7558: pooled instances must isolate concurrent calls
 * while still allowing executions to overlap.
 */
public class WasmLoaderConcurrentTest {

    private final String expectedResult = "go result";

    private final Map<Long, String> results = new ConcurrentHashMap<>();

    private final AtomicLong ids = new AtomicLong();

    @Test
    public void concurrentExportsAreIsolatedTest() throws Exception {
        assertConcurrentCallsAreIsolated(new TestLoader(4, 0L, null, null), argId -> expectedResult);
    }

    @Test
    public void concurrentExportsAreIsolatedOnInterpreterTest() throws Exception {
        assertConcurrentCallsAreIsolated(new InterpretedTestLoader(4), argId -> expectedResult);
    }

    @Test
    public void concurrentEchoPayloadsAreIsolatedTest() throws Exception {
        // the echo guest returns exactly its own input, so input-side swaps between calls are visible
        assertConcurrentCallsAreIsolated(new EchoTestLoader(4), argId -> "hello from java " + argId);
    }

    @Test
    public void pooledInstancesExecuteInParallelTest() throws Exception {
        final long blockedArgId = -1L;
        final CountDownLatch blockEntered = new CountDownLatch(1);
        final CountDownLatch releaseBlock = new CountDownLatch(1);
        try (WasmLoader loader = new TestLoader(2, blockedArgId, blockEntered, releaseBlock)) {
            final ExportFunction execute = loader.getWasmExtern("execute")
                    .orElseThrow(() -> new IllegalStateException("execute export not found"));
            final ExecutorService pool = Executors.newFixedThreadPool(2);
            try {
                final Future<?> blockedCall = pool.submit(() -> execute.apply(blockedArgId));
                assertTrue(blockEntered.await(10, TimeUnit.SECONDS), "first call never reached its host callback");

                // must complete on a free instance, not queue behind the parked one
                final Future<String> secondCall = pool.submit(() -> {
                    final long argumentId = ids.incrementAndGet();
                    execute.apply(argumentId);
                    return results.remove(argumentId);
                });
                assertEquals(expectedResult, secondCall.get(10, TimeUnit.SECONDS),
                        "second call should complete on a free pooled instance, not queue behind the parked one");

                releaseBlock.countDown();
                blockedCall.get(10, TimeUnit.SECONDS);
                assertEquals(expectedResult, results.remove(blockedArgId));
                assertTrue(results.isEmpty(), "stray results indicate cross-call payload leakage");
            } finally {
                releaseBlock.countDown();
                pool.shutdownNow();
            }
        }
    }

    @Test
    public void nestedExportsUseDistinctInstancesTest() throws Exception {
        try (WasmLoader loader = new RecursiveTestLoader(3, 3)) {
            final ExportFunction execute = loader.getWasmExtern("execute")
                    .orElseThrow(() -> new IllegalStateException("execute export not found"));
            execute.apply(-1L);
            for (long argId = -1L; argId >= -3L; argId--) {
                assertEquals(expectedResult, results.remove(argId), "nested call " + argId + " lost its payload");
            }
            assertTrue(results.isEmpty(), "stray results indicate cross-call payload leakage");
        }
    }

    @Test
    public void nestingBeyondPoolSizeFailsExplicitlyTest() throws Exception {
        try (WasmLoader loader = new RecursiveTestLoader(2, 3)) {
            final ExportFunction execute = loader.getWasmExtern("execute")
                    .orElseThrow(() -> new IllegalStateException("execute export not found"));
            final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> execute.apply(-1L));
            assertTrue(ex.getMessage().contains("Nested export calls exceed the WASM instance pool size"),
                    "unexpected message: " + ex.getMessage());
            assertTrue(results.isEmpty(), "failed nesting must not leave stray payloads");
        }
    }

    @Test
    public void poolExhaustionTimesOutInsteadOfHangingTest() throws Exception {
        final long blockedArgId = -1L;
        final CountDownLatch blockEntered = new CountDownLatch(1);
        final CountDownLatch releaseBlock = new CountDownLatch(1);
        try (WasmLoader loader = new TimeoutTestLoader(1, blockedArgId, blockEntered, releaseBlock)) {
            final ExportFunction execute = loader.getWasmExtern("execute")
                    .orElseThrow(() -> new IllegalStateException("execute export not found"));
            final ExecutorService pool = Executors.newFixedThreadPool(1);
            try {
                final Future<?> blockedCall = pool.submit(() -> execute.apply(blockedArgId));
                assertTrue(blockEntered.await(10, TimeUnit.SECONDS), "first call never reached its host callback");

                // the only instance is parked in the blocked callback: the pool must
                // fail this call after the (shortened) timeout instead of hanging it
                final ShenyuWasmException ex = assertThrows(ShenyuWasmException.class,
                        () -> execute.apply(ids.incrementAndGet()));
                assertTrue(ex.getMessage().contains("Timed out"), "unexpected message: " + ex.getMessage());
                // the parked call deterministically holds the only instance, so the
                // diagnostics must report the pool as fully in use
                assertTrue(ex.getMessage().contains("pool size 1, 1 in use"),
                        "unexpected message: " + ex.getMessage());

                releaseBlock.countDown();
                blockedCall.get(10, TimeUnit.SECONDS);
                assertEquals(expectedResult, results.remove(blockedArgId));
            } finally {
                releaseBlock.countDown();
                pool.shutdownNow();
            }
        }
    }

    @Test
    public void getMemoryOutsideExecutionFailsFastTest() {
        try (WasmLoader loader = new TestLoader(1, 0L, null, null)) {
            final IllegalStateException ex = assertThrows(IllegalStateException.class, loader::getMemory);
            assertTrue(ex.getMessage().contains("outside an exported call or host callback"),
                    "unexpected message: " + ex.getMessage());
        }
    }

    private void assertConcurrentCallsAreIsolated(final WasmLoader loader,
                                                  final LongFunction<String> expectedFor) throws Exception {
        try (loader) {
            final ExportFunction execute = loader.getWasmExtern("execute")
                    .orElseThrow(() -> new IllegalStateException("execute export not found"));
            final int threads = 8;
            final int iterations = 100;
            final ExecutorService pool = Executors.newFixedThreadPool(threads);
            try {
                final List<Future<List<String>>> futures = new ArrayList<>();
                for (int i = 0; i < threads; i++) {
                    futures.add(pool.submit(() -> runIterations(execute, iterations, expectedFor)));
                }
                final List<String> failures = new ArrayList<>();
                for (Future<List<String>> future : futures) {
                    failures.addAll(future.get());
                }
                assertTrue(failures.isEmpty(), () -> String.join(System.lineSeparator(), failures));
                assertTrue(results.isEmpty(), "stray results indicate cross-call payload leakage");
            } finally {
                pool.shutdownNow();
            }
        }
    }

    private List<String> runIterations(final ExportFunction execute, final int iterations,
                                       final LongFunction<String> expectedFor) {
        final List<String> failures = new ArrayList<>();
        for (int i = 0; i < iterations; i++) {
            final long argumentId = ids.incrementAndGet();
            try {
                execute.apply(argumentId);
                final String result = results.remove(argumentId);
                final String expected = expectedFor.apply(argumentId);
                if (!expected.equals(result)) {
                    failures.add(String.format("%s argId=%d expected '%s' but got '%s'",
                            Thread.currentThread().getName(), argumentId, expected, result));
                }
            } catch (RuntimeException e) {
                failures.add(String.format("%s argId=%d threw %s: %s",
                        Thread.currentThread().getName(), argumentId, e.getClass().getSimpleName(), e.getMessage()));
            }
        }
        return failures;
    }

    /**
     * Mirrors real plugin subclasses: built-in callbacks reach guest memory through
     * {@code getMemory()}. A non-zero {@code blockArgId} parks the matching call
     * inside its {@code get_args} callback until {@code releaseBlock} fires.
     */
    class TestLoader extends WasmLoader {

        private final long blockArgId;

        private final CountDownLatch blockEntered;

        private final CountDownLatch releaseBlock;

        TestLoader(final int poolSize, final long blockArgId,
                   final CountDownLatch blockEntered, final CountDownLatch releaseBlock) {
            super(AbstractWasmPluginTest.GoWasmPlugin.class, null, poolSize);
            this.blockArgId = blockArgId;
            this.blockEntered = blockEntered;
            this.releaseBlock = releaseBlock;
        }

        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            if (blockArgId != 0L && argId == blockArgId) {
                blockEntered.countDown();
                try {
                    assertTrue(releaseBlock.await(10, TimeUnit.SECONDS), "blocked callback was never released");
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
            final byte[] data = ("hello from java " + argId).getBytes(StandardCharsets.UTF_8);
            final int writeLen = Math.min(len, data.length);
            getMemory().write(Math.toIntExact(addr), data, 0, writeLen);
            return writeLen;
        }

        @Override
        protected long onPutResult(final long argId, final long addr, final int len) {
            results.put(argId, new String(getMemory().readBytes(Math.toIntExact(addr), len), StandardCharsets.UTF_8));
            return 0;
        }
    }

    /** Forces the interpreter machine to cover the fallback execution path. */
    final class InterpretedTestLoader extends TestLoader {

        InterpretedTestLoader(final int poolSize) {
            super(poolSize, 0L, null, null);
        }

        @Override
        protected Function<Instance, Machine> compileMachineFactory(final WasmModule module) {
            return null;
        }
    }

    /**
     * Drives nested export calls from the {@code get_args} callback: a negative
     * argId {@code -n} recurses to level {@code n + 1} until {@code depth} is reached.
     */
    class RecursiveTestLoader extends TestLoader {

        private final int depth;

        RecursiveTestLoader(final int poolSize, final int depth) {
            super(poolSize, 0L, null, null);
            this.depth = depth;
        }

        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            if (argId < 0L && -argId < depth) {
                getWasmExtern("execute").orElseThrow().apply(argId - 1L);
            }
            return super.onGetArgs(argId, addr, len);
        }
    }

    /** Shortens the lease wait so pool-exhaustion timeouts surface in milliseconds. */
    final class TimeoutTestLoader extends TestLoader {

        TimeoutTestLoader(final int poolSize, final long blockArgId,
                          final CountDownLatch blockEntered, final CountDownLatch releaseBlock) {
            super(poolSize, blockArgId, blockEntered, releaseBlock);
        }

        @Override
        protected long leaseTimeoutMillis() {
            return 200L;
        }
    }

    /** Name carrier for the echo fixture built from wasm-concurrent/echo.wat. */
    static class EchoPlugin {
    }

    /**
     * Loads the echo guest: {@code put_result} returns exactly the bytes
     * {@code get_args} delivered, so results must match each call's own input.
     */
    class EchoTestLoader extends WasmLoader {

        EchoTestLoader(final int poolSize) {
            super(EchoPlugin.class, null, poolSize);
        }

        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            final byte[] data = ("hello from java " + argId).getBytes(StandardCharsets.UTF_8);
            final int writeLen = Math.min(len, data.length);
            getMemory().write(Math.toIntExact(addr), data, 0, writeLen);
            return writeLen;
        }

        @Override
        protected long onPutResult(final long argId, final long addr, final int len) {
            results.put(argId, new String(getMemory().readBytes(Math.toIntExact(addr), len), StandardCharsets.UTF_8));
            return 0;
        }
    }
}
