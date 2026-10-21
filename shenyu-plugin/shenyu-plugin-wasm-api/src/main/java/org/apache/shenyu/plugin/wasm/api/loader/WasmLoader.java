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

package org.apache.shenyu.plugin.wasm.api.loader;

import com.dylibso.chicory.compiler.MachineFactoryCompiler;
import com.dylibso.chicory.runtime.ExportFunction;
import com.dylibso.chicory.runtime.HostFunction;
import com.dylibso.chicory.runtime.ImportValues;
import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Machine;
import com.dylibso.chicory.runtime.Memory;
import com.dylibso.chicory.runtime.Store;
import com.dylibso.chicory.wasm.Parser;
import com.dylibso.chicory.wasm.WasmModule;
import com.dylibso.chicory.wasm.types.FunctionType;
import com.dylibso.chicory.wasm.types.ValType;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmInitException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * The WasmLoader aims to load wasm file and provide the wasm function to java,
 * also provide the java function to wasm if we need.
 *
 * <p>Powered by <a href="https://chicory.dev/">Chicory</a> — a pure-Java WebAssembly
 * runtime with zero native dependencies.
 *
 * <p>Concurrency model: a Chicory {@link Instance} is not thread-safe (interpreter
 * stacks, guest heap, linear memory), so every exported call leases one pooled instance
 * exclusively, waiting at most {@link #leaseTimeoutMillis()} for a free one; host
 * callbacks run inside the lease and {@link #getMemory()} resolves to the instance
 * executing on the calling thread, failing fast outside a call.
 */
public class WasmLoader implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(WasmLoader.class);

    private static final String IMPORT_WASM_MODULE_NAME = "shenyu";

    // Upper bound on parallel executions; capped so worst-case memory (grown guest
    // heaps persist per instance) stays bounded.
    private static final int DEFAULT_POOL_SIZE = Math.min(Runtime.getRuntime().availableProcessors(), 8);

    // Bounded wait for a pooled instance: pool exhaustion (or a nested-call circular
    // wait) fails the request after this timeout instead of hanging it forever.
    private static final long DEFAULT_LEASE_TIMEOUT_MILLIS = 30_000L;

    // Instances currently executing on this thread (a stack: nested calls push/pop);
    // getMemory() in host callbacks resolves against the top entry.
    private final ThreadLocal<ArrayDeque<Instance>> bindings = ThreadLocal.withInitial(ArrayDeque::new);

    private final AtomicBoolean closed = new AtomicBoolean(false);

    // becomes true after the instance pool is fully populated
    private volatile boolean ready;

    private final String wasmName;

    private final Store store;

    // first created instance; probes export existence for getWasmExtern (all pooled
    // instances share one module, so any of them answers for all)
    private final Instance primary;

    private final BlockingQueue<Instance> freeInstances;

    private final int poolSize;

    public WasmLoader() {
        this(null, null);
    }

    /**
     * Constructs a WasmLoader with the default pool size (at most 8 instances).
     *
     * <p>Initialization order:
     * <ol>
     *   <li>Register full WASI preview1 functions via Chicory's built-in implementation</li>
     *   <li>If {@code initializer} is non-null, call it; otherwise register built-in host functions
     *       ({@code get_args}/{@code put_result}) and call {@link #initWasmCallJavaFunc(Store)}</li>
     * </ol>
     *
     * @param wasmClass   the class whose name determines the .wasm resource path; if null, uses
     *                    {@code this.getClass()} (inheritance mode)
     * @param initializer optional consumer to register host functions; when non-null the built-in
     *                    host functions and {@link #initWasmCallJavaFunc(Store)} are skipped
     */
    public WasmLoader(final Class<?> wasmClass, final Consumer<Store> initializer) {
        this(wasmClass, initializer, DEFAULT_POOL_SIZE);
    }

    /**
     * Constructs a WasmLoader with an explicit instance pool size.
     *
     * @param wasmClass   the class whose name determines the .wasm resource path; if null, uses
     *                    {@code this.getClass()} (inheritance mode)
     * @param initializer optional consumer to register host functions; when non-null the built-in
     *                    host functions and {@link #initWasmCallJavaFunc(Store)} are skipped
     * @param poolSize    the number of WASM instances to instantiate (at least 1)
     */
    public WasmLoader(final Class<?> wasmClass, final Consumer<Store> initializer, final int poolSize) {
        final Class<?> clazz = Objects.nonNull(wasmClass) ? wasmClass : this.getClass();
        this.wasmName = clazz.getName() + ".wasm";
        try {
            // locate `.wasm` lib.
            URL resource = clazz.getClassLoader().getResource(wasmName);
            if (Objects.isNull(resource)) {
                throw new ShenyuWasmInitException("Can't find wasm file: " + wasmName);
            }
            // Instantiates the WebAssembly module.

            this.store = new Store();

            // Register WASI preview1 functions via Chicory's built-in implementation,
            // then override proc_exit with a no-op to avoid WasiExitException.
            registerWasiStubs(store);

            // Allow subclasses to register custom host functions (get_args/put_result).
            if (Objects.nonNull(initializer)) {
                initializer.accept(store);
            } else {
                registerBuiltinHostFunctions(store);
            }

            // Allow WasmLoader subclasses to override and register additional host functions.
            // Only called when not using Consumer initializer, to avoid double invocation.
            if (Objects.isNull(initializer)) {
                initWasmCallJavaFunc(store);
            }

            // Reads the WebAssembly module as bytes.
            final byte[] wasmBytes;
            try (var in = resource.openStream()) {
                wasmBytes = in.readAllBytes();
            }
            WasmModule module = Parser.parse(wasmBytes);

            final ImportValues importValues = store.toImportValues();
            // Try the runtime compiler (factory shared by all pooled instances);
            // fall back to the interpreter if compilation or first instantiation fails.
            Function<Instance, Machine> machineFactory;
            Instance firstInstance;
            try {
                machineFactory = compileMachineFactory(module);
                if (Objects.isNull(machineFactory)) {
                    firstInstance = createInstance(module, importValues, null);
                    LOG.debug("Using interpreter for {}", wasmName);
                } else {
                    firstInstance = createInstance(module, importValues, machineFactory);
                    LOG.debug("Using runtime compiler for {}", wasmName);
                }
            } catch (LinkageError | RuntimeException compilerError) {
                LOG.warn("Runtime compiler unavailable, falling back to interpreter for {}: {}",
                        wasmName, compilerError.getMessage());
                machineFactory = null;
                firstInstance = createInstance(module, importValues, null);
            }

            final int size = Math.max(1, poolSize);
            final List<Instance> instances = new ArrayList<>(size);
            instances.add(firstInstance);
            for (int i = 1; i < size; i++) {
                instances.add(createInstance(module, importValues, machineFactory));
            }
            this.poolSize = size;
            this.primary = firstInstance;
            this.freeInstances = new ArrayBlockingQueue<>(size);
            // guest initialization runs after the execution mode is fixed,
            // so its failures are not retried as compiler failures
            for (Instance instance : instances) {
                initialize(instance);
                freeInstances.offer(instance);
            }
            this.ready = true;
        } catch (IOException e) {
            throw new ShenyuWasmInitException(e);
        }
    }

    /**
     * Compiles the module for JVM-bytecode execution. Overriding to return null
     * forces the interpreter machine (used by tests to cover the fallback path).
     *
     * @param module the parsed WASM module
     * @return the compiled machine factory, or null for the interpreter
     */
    protected Function<Instance, Machine> compileMachineFactory(final WasmModule module) {
        return MachineFactoryCompiler.compile(module);
    }

    private Instance createInstance(final WasmModule module, final ImportValues importValues,
                                    final Function<Instance, Machine> machineFactory) {
        Instance.Builder builder = Instance.builder(module)
                .withImportValues(importValues)
                .withStart(false);
        if (Objects.nonNull(machineFactory)) {
            builder = builder.withMachineFactory(machineFactory);
        }
        Instance instance = builder.build();
        if (Objects.isNull(instance.memory())) {
            throw new ShenyuWasmInitException("memory not available in wasm file: " + wasmName);
        }
        return instance;
    }

    private void initialize(final Instance instance) {
        // Call _initialize if present (required by TinyGo -target wasm-unknown),
        // bound to this instance so init-time host callbacks see its own memory.
        findExport(instance, "_initialize").ifPresent(initFn -> {
            executeBound(instance, initFn, new long[0]);
            LOG.debug("Called _initialize for {}", wasmName);
        });
    }

    private void registerWasiStubs(final Store store) {
        // Use Chicory's built-in full WASI preview1 implementation for comprehensive
        // WASI function coverage, matching the previous wasmtime-java behavior.
        // Only inherit stdout/stderr so WASM guest output (e.g. Go println,
        // Rust eprintln!) is visible for debugging. Avoid inheriting the full
        // system context (env/args/filesystem): besides security hardening,
        // preopened files would add mutable state to the WASI object shared
        // by all pooled instances.
        var options = com.dylibso.chicory.wasi.WasiOptions.builder()
                .withStdout(System.out)
                .withStderr(System.err)
                .build();
        var wasi = com.dylibso.chicory.wasi.WasiPreview1.builder()
                .withOptions(options)
                .build();
        store.addFunction(wasi.toHostFunctions());

        // Override proc_exit with a no-op: valid WASI program exit (proc_exit(0))
        // is normal termination and should not propagate as a Java exception in
        // a hosted plugin runtime.
        store.addFunction(new HostFunction(
                "wasi_snapshot_preview1", "proc_exit",
                FunctionType.of(List.of(ValType.I32), List.of()),
                (inst, args) -> {
                    int exitCode = (int) args[0];
                    if (exitCode != 0) {
                        throw new ShenyuWasmException("WASI proc_exit(" + exitCode + ") from " + wasmName);
                    }
                    return new long[0];
                }
        ));
    }

    private void registerBuiltinHostFunctions(final Store store) {
        store.addFunction(new HostFunction(
                IMPORT_WASM_MODULE_NAME, "get_args",
                FunctionType.of(List.of(ValType.I64, ValType.I64, ValType.I32), List.of(ValType.I32)),
                (instance, args) -> new long[]{onGetArgs(args[0], args[1], (int) args[2])}
        ));
        store.addFunction(new HostFunction(
                IMPORT_WASM_MODULE_NAME, "put_result",
                FunctionType.of(List.of(ValType.I64, ValType.I64, ValType.I32), List.of(ValType.I32)),
                (instance, args) -> new long[]{onPutResult(args[0], args[1], (int) args[2])}
        ));
    }

    /**
     * Override in subclasses to register additional host functions exposed to the
     * WASM module. This is called during construction after the built-in host
     * functions (WASI stubs and {@code get_args}/{@code put_result}) have been
     * registered, allowing subclasses to add their own imports.
     *
     * <p>Example: register a custom host function in the "shenyu" namespace:
     * <pre>{@code
     * @Override
     * protected void initWasmCallJavaFunc(final Store store) {
     *     store.addFunction(new HostFunction(
     *         "shenyu", "my_func",
     *         FunctionType.of(List.of(ValType.I32), List.of(ValType.I32)),
     *         (instance, args) -> new long[]{(int) args[0] + 1}
     *     ));
     * }
     * }</pre>
     *
     * @param store the Chicory Store where host functions can be registered
     */
    protected void initWasmCallJavaFunc(final Store store) {
        // no-op by default
    }

    /**
     * Called when the WASM module invokes the {@code get_args} host import
     * ({@code (argId: i64, addr: i64, len: i32) -> i32}).
     *
     * <p>The WASM side passes a buffer address and length in its linear memory.
     * The Java implementation should write serialized argument data into that
     * buffer and return the number of bytes actually written. Returning 0
     * signals "no data".
     *
     * <p>Corresponding WASM-side call (via {@code wasmabi} Go helper):
     * <pre>{@code
     * buf := make([]byte, 1024)
     * input := wasmabi.GetArgs(argId, buf)
     * }</pre>
     *
     * @param argId the argument identifier passed from WASM (maps to a Java-side argument)
     * @param addr  the starting address in WASM linear memory to write into
     * @param len   the maximum number of bytes to write
     * @return the number of bytes actually written, or 0 if no data is available
     */
    protected long onGetArgs(final long argId, final long addr, final int len) {
        return 0;
    }

    /**
     * Called when the WASM module invokes the {@code put_result} host import
     * ({@code (argId: i64, addr: i64, len: i32) -> i32}).
     *
     * <p>The WASM side passes a pointer and length referencing result data in
     * its linear memory. The Java implementation should read and process that
     * data. Returning 0 signals "success".
     *
     * <p>Corresponding WASM-side call (via {@code wasmabi} Go helper):
     * <pre>{@code
     * wasmabi.PutResult(argId, []byte("my result"))
     * }</pre>
     *
     * @param argId the argument identifier passed from WASM (maps to a Java-side argument)
     * @param addr  the starting address in WASM linear memory to read from
     * @param len   the number of bytes to read
     * @return 0 on success, or a non-zero error code
     */
    protected long onPutResult(final long argId, final long addr, final int len) {
        return 0;
    }

    /**
     * Get the WASM exported function.
     *
     * <p>The returned function leases a pooled instance for the whole call,
     * making concurrent invocations safe.
     *
     * <p>Reentrancy constraint: exports must not be invoked from a host callback
     * that runs during construction (e.g. triggered by {@code _initialize}) —
     * the pool is not populated yet, so such calls fail fast.
     *
     * @param funcName the name of the WASM exported function
     * @return an Optional containing the ExportFunction if found, otherwise empty
     */
    public Optional<ExportFunction> getWasmExtern(final String funcName) {
        return findWasmExtern(funcName);
    }

    /**
     * Check export names before resolving a function so absent hooks remain optional
     * while a matching export of the wrong kind still fails validation.
     *
     * @param funcName the exported function name
     * @return the exported function, or empty only when the name is absent
     */
    private Optional<ExportFunction> findWasmExtern(final String funcName) {
        if (findExport(primary, funcName).isPresent()) {
            return Optional.of(args -> executeOnPool(funcName, args));
        }
        LOG.debug("WASM export function '{}' not found in {}", funcName, wasmName);
        return Optional.empty();
    }

    private static Optional<ExportFunction> findExport(final Instance instance, final String funcName) {
        // Chicory throws InvalidException for an absent export. Check the names first
        // so optional hooks remain optional without hiding invalid export types.
        var exports = instance.module().exportSection();
        for (int i = 0; i < exports.exportCount(); i++) {
            if (exports.getExport(i).name().equals(funcName)) {
                return Optional.of(instance.export(funcName));
            }
        }
        return Optional.empty();
    }

    private long[] executeOnPool(final String funcName, final long[] args) {
        final Instance lease = acquire(funcName);
        Throwable failure = null;
        try {
            return executeBound(lease, lease.export(funcName), args);
        } catch (final Throwable t) {
            // remembered so a release failure below cannot mask the original failure
            failure = t;
            throw t;
        } finally {
            try {
                release(lease);
            } catch (final RuntimeException e) {
                if (Objects.nonNull(failure)) {
                    failure.addSuppressed(e);
                } else {
                    throw e;
                }
            }
        }
    }

    // binds the thread to the instance so guest host callbacks resolve getMemory() to it;
    // pops on exit (nested calls each push their own entry)
    private long[] executeBound(final Instance instance, final ExportFunction export, final long[] args) {
        final ArrayDeque<Instance> stack = bindings.get();
        stack.push(instance);
        try {
            return export.apply(args);
        } finally {
            stack.pop();
        }
    }

    /**
     * Max time to wait for a free pooled instance before failing the call.
     * Overridable so tests can exercise the timeout path quickly.
     *
     * @return the lease timeout in milliseconds
     */
    protected long leaseTimeoutMillis() {
        return DEFAULT_LEASE_TIMEOUT_MILLIS;
    }

    private Instance acquire(final String funcName) {
        if (!ready) {
            // reached only from a host callback during construction (e.g. a nested
            // export call from _initialize): no initialized instance is free yet,
            // so waiting could never succeed
            throw new IllegalStateException(
                    "Nested export call during construction of " + wasmName
                            + "; the instance pool is not ready yet");
        }
        final ArrayDeque<Instance> active = bindings.get();
        if (active.size() >= poolSize) {
            // nesting deeper than the pool: this thread already leases every instance,
            // so waiting could never succeed
            throw new IllegalStateException(
                    "Nested export calls exceed the WASM instance pool size for " + wasmName);
        }
        final long timeoutMillis = leaseTimeoutMillis();
        final Instance lease;
        try {
            lease = freeInstances.poll(timeoutMillis, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ShenyuWasmException(
                    "Interrupted while waiting for a pooled WASM instance of " + wasmName, e);
        }
        if (Objects.isNull(lease)) {
            final int inUse = poolSize - freeInstances.size();
            throw new ShenyuWasmException(
                    "Timed out after " + timeoutMillis + "ms waiting for a pooled WASM instance of "
                            + wasmName + " for export '" + funcName + "' (pool size " + poolSize
                            + ", " + inUse + " in use, " + active.size() + " held by this thread)");
        }
        return lease;
    }

    private void release(final Instance lease) {
        if (!freeInstances.offer(lease)) {
            throw new IllegalStateException(
                    "Failed to return pooled WASM instance for " + wasmName
                            + "; queue is full, which indicates a double-release or pool corruption");
        }
    }

    /**
     * Get the WASM linear memory of the instance executing on the current thread.
     *
     * <p>Only valid inside an exported call or a host callback on the calling thread
     * (where it resolves to the leased instance): pooled instances each own their
     * memory, so there is no safe fallback and unbound callers fail fast.
     *
     * @return the WASM linear memory of the currently executing instance
     * @throws IllegalStateException when no instance is executing on this thread
     */
    public Memory getMemory() {
        final Instance current = bindings.get().peek();
        if (Objects.isNull(current)) {
            throw new IllegalStateException(
                    "getMemory() called outside an exported call or host callback of " + wasmName
                            + "; pooled instances have per-instance memory");
        }
        return current.memory();
    }

    /**
     * Get the wasm file name.
     *
     * @return the wasm file name
     */
    public String getWasmName() {
        return wasmName;
    }

    /**
     * Get the Chicory Store.
     *
     * @return the Chicory Store
     */
    public Store getStore() {
        return store;
    }

    @Override
    public void close() {
        if (this.closed.compareAndSet(false, true)) {
            // Chicory is a pure-Java runtime — Instance, Store, and WasmModule
            // do not hold native resources and are managed by the JVM GC.
            // No explicit cleanup is required. Add cleanup logic here if
            // the underlying WASM runtime changes in the future.
            return;
        }
    }
}
