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
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
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
 * exclusively; host callbacks run inside the lease and {@link #getMemory()} resolves to
 * the instance executing on the calling thread.
 */
public class WasmLoader implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(WasmLoader.class);

    private static final String IMPORT_WASM_MODULE_NAME = "shenyu";

    // One instance per processor: matches event-loop parallelism; guest memory is paid per instance.
    private static final int DEFAULT_POOL_SIZE = Runtime.getRuntime().availableProcessors();

    // The instance executing on this thread; getMemory() in host callbacks resolves against it.
    private static final ThreadLocal<Instance> CURRENT_INSTANCE = new ThreadLocal<>();

    private final AtomicBoolean closed = new AtomicBoolean(false);

    private final String wasmName;

    private final Store store;

    private final Instance primary;

    private final PooledInstance[] pool;

    private final AtomicInteger nextIndex = new AtomicInteger();

    public WasmLoader() {
        this(null, null);
    }

    /**
     * Constructs a WasmLoader with the default pool size (one instance per processor).
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
                machineFactory = MachineFactoryCompiler.compile(module);
                firstInstance = createInstance(module, importValues, machineFactory);
                LOG.debug("Using runtime compiler for {}", wasmName);
            } catch (LinkageError | RuntimeException compilerError) {
                LOG.warn("Runtime compiler unavailable, falling back to interpreter for {}: {}",
                        wasmName, compilerError.getMessage());
                machineFactory = null;
                firstInstance = createInstance(module, importValues, null);
            }

            final int size = Math.max(1, poolSize);
            this.pool = new PooledInstance[size];
            this.pool[0] = new PooledInstance(firstInstance);
            for (int i = 1; i < size; i++) {
                this.pool[i] = new PooledInstance(createInstance(module, importValues, machineFactory));
            }
            this.primary = pool[0].instance;
        } catch (IOException e) {
            throw new ShenyuWasmInitException(e);
        }
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

        // Call _initialize if present (required by TinyGo -target wasm-unknown),
        // bound to this instance so init-time host callbacks see its own memory.
        findExport(instance, "_initialize").ifPresent(initFn -> {
            executeBound(instance, initFn, new long[0]);
            LOG.debug("Called _initialize for {}", wasmName);
        });

        if (Objects.isNull(instance.memory())) {
            throw new ShenyuWasmInitException("memory not available in wasm file: " + wasmName);
        }
        return instance;
    }

    private void registerWasiStubs(final Store store) {
        // Use Chicory's built-in full WASI preview1 implementation for comprehensive
        // WASI function coverage, matching the previous wasmtime-java behavior.
        // Only inherit stdout/stderr so WASM guest output (e.g. Go println,
        // Rust eprintln!) is visible for debugging. Avoid inheriting the full
        // system context (env/args/filesystem) for security hardening.
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
     * @param funcName the name of the WASM exported function
     * @return an Optional containing the ExportFunction if found, otherwise empty
     */
    public Optional<ExportFunction> getWasmExtern(final String funcName) {
        return findWasmExtern(funcName);
    }

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
        final PooledInstance lease = acquire();
        try {
            return executeBound(lease.instance, lease.instance.export(funcName), args);
        } finally {
            lease.lock.unlock();
        }
    }

    // binds the thread to the instance so guest host callbacks resolve getMemory() to it;
    // restores the previously bound instance on exit (nested calls)
    private long[] executeBound(final Instance instance, final ExportFunction export, final long[] args) {
        final Instance previous = CURRENT_INSTANCE.get();
        CURRENT_INSTANCE.set(instance);
        try {
            return export.apply(args);
        } finally {
            if (Objects.nonNull(previous)) {
                CURRENT_INSTANCE.set(previous);
            } else {
                CURRENT_INSTANCE.remove();
            }
        }
    }

    private PooledInstance acquire() {
        final int start = Math.floorMod(nextIndex.getAndIncrement(), pool.length);
        for (int offset = 0; offset < pool.length; offset++) {
            final PooledInstance candidate = pool[Math.floorMod(start + offset, pool.length)];
            if (candidate.lock.tryLock()) {
                return candidate;
            }
        }
        // every instance is busy: queue on the round-robin candidate
        final PooledInstance candidate = pool[start];
        candidate.lock.lock();
        return candidate;
    }

    /**
     * Get the WASM module's linear memory for use in host functions.
     *
     * <p>During an exported call (including host callbacks) this returns the memory of
     * the instance executing on the current thread; otherwise the primary instance's.
     *
     * @return the WASM linear memory
     */
    public Memory getMemory() {
        final Instance current = CURRENT_INSTANCE.get();
        return Objects.nonNull(current) ? current.memory() : primary.memory();
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

    /**
     * One pooled WASM instance with its single-owner lease lock.
     */
    private static final class PooledInstance {

        private final Instance instance;

        private final ReentrantLock lock = new ReentrantLock();

        private PooledInstance(final Instance instance) {
            this.instance = instance;
        }
    }
}
