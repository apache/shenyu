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
import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Memory;
import com.dylibso.chicory.runtime.Store;
import com.dylibso.chicory.wasm.Parser;
import com.dylibso.chicory.wasm.WasmModule;
import com.dylibso.chicory.wasm.types.FunctionType;
import com.dylibso.chicory.wasm.types.ValType;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmInitException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * The WasmLoader aims to load wasm file and provide the wasm function to java,
 * also provide the java function to wasm if we need.
 *
 * <p>Powered by <a href="https://chicory.dev/">Chicory</a> — a pure-Java WebAssembly
 * runtime with zero native dependencies.
 */
public class WasmLoader implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(WasmLoader.class);

    private static final String IMPORT_WASM_MODULE_NAME = "shenyu";

    private final AtomicBoolean closed = new AtomicBoolean(false);
    
    private final String wasmName;

    private final Store store;

    private Instance instance;

    public WasmLoader() {
        this(null, null);
    }
    
    /**
     * Constructs a WasmLoader, locates the {@code .wasm} resource, and instantiates the module.
     *
     * <p>Initialization order:
     * <ol>
     *   <li>Register WASI preview1 stubs</li>
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

            // Register WASI preview1 stubs. Both Go and Rust modules need these,
            // but we implement our own no-op versions instead of using Chicory's
            // WasiPreview1 to avoid WasiExitException from proc_exit(0).
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
            byte[] wasmBytes = Files.readAllBytes(Paths.get(resource.toURI()));
            WasmModule module = Parser.parse(wasmBytes);

            // Try runtime compiler (WASM → JVM bytecode) for better execution speed.
            // Falls back gracefully to interpreter if something goes wrong.
            try {
                this.instance = Instance.builder(module)
                        .withImportValues(store.toImportValues())
                        .withMachineFactory(MachineFactoryCompiler.compile(module))
                        .withStart(false)
                        .build();
                LOG.debug("Using runtime compiler for {}", wasmName);
            } catch (LinkageError | RuntimeException compilerError) {
                LOG.warn("Runtime compiler unavailable, falling back to interpreter for {}: {}",
                        wasmName, compilerError.getMessage());
                this.instance = Instance.builder(module)
                         .withImportValues(store.toImportValues())
                         .withStart(false)
                         .build();
            }

            // Call _initialize if present (required by TinyGo -target wasm-unknown).
            try {
                ExportFunction initFn = instance.export("_initialize");
                if (Objects.nonNull(initFn)) {
                    initFn.apply();
                    LOG.debug("Called _initialize for {}", wasmName);
                }
            } catch (com.dylibso.chicory.wasm.InvalidException e) {
                LOG.debug("No _initialize export in {}", wasmName);
            }

            if (Objects.isNull(instance.memory())) {
                throw new ShenyuWasmInitException("memory not available in wasm file: " + wasmName);
            }

            Runtime.getRuntime().addShutdownHook(new Thread(this::close));
        } catch (URISyntaxException | IOException e) {
            throw new ShenyuWasmInitException(e);
        }
    }

    private void registerWasiStubs(final Store store) {
        // proc_exit(code: i32) -> ()
        store.addFunction(new HostFunction(
                "wasi_snapshot_preview1", "proc_exit",
                FunctionType.of(List.of(ValType.I32), List.of()),
                (inst, args) -> new long[0]
        ));
        // random_get(buf: i32, buf_len: i32) -> errno: i32
        store.addFunction(new HostFunction(
                "wasi_snapshot_preview1", "random_get",
                FunctionType.of(List.of(ValType.I32, ValType.I32), List.of(ValType.I32)),
                (inst, args) -> {
                    int buf = (int) args[0];
                    int bufLen = (int) args[1];
                    byte[] randomBytes = new byte[bufLen];
                    ThreadLocalRandom.current().nextBytes(randomBytes);
                    inst.memory().write(buf, randomBytes);
                    return new long[]{0};
                }
        ));
        // fd_write(fd: i32, iovs: i32, iovs_len: i32, nwritten: i32) -> errno: i32
        store.addFunction(new HostFunction(
                "wasi_snapshot_preview1", "fd_write",
                FunctionType.of(List.of(ValType.I32, ValType.I32, ValType.I32, ValType.I32), List.of(ValType.I32)),
                (inst, args) -> {
                    int fd = (int) args[0];
                    int iovs = (int) args[1];
                    int iovsLen = (int) args[2];
                    int nwrittenPtr = (int) args[3];
                    long total = 0;
                    StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < iovsLen; i++) {
                        int off = iovs + i * 8;
                        int bufOff = (int) inst.memory().readI32(off);
                        int bufLen = (int) inst.memory().readI32(off + 4);
                        if (bufLen > 0) {
                            sb.append(new String(inst.memory().readBytes(bufOff, bufLen), java.nio.charset.StandardCharsets.UTF_8));
                        }
                        total += bufLen;
                    }
                    String output = sb.toString();
                    if (fd == 1) {
                        System.out.print(output);
                    } else if (fd == 2) {
                        System.err.print(output);
                    }
                    inst.memory().writeI32(nwrittenPtr, (int) total);
                    return new long[]{0};
                }
        ));
        // environ_get(environ: i32, environ_buf: i32) -> errno: i32
        store.addFunction(new HostFunction(
                "wasi_snapshot_preview1", "environ_get",
                FunctionType.of(List.of(ValType.I32, ValType.I32), List.of(ValType.I32)),
                (inst, args) -> new long[]{0}
        ));
        // environ_sizes_get(count: i32, buf_size: i32) -> errno: i32
        store.addFunction(new HostFunction(
                "wasi_snapshot_preview1", "environ_sizes_get",
                FunctionType.of(List.of(ValType.I32, ValType.I32), List.of(ValType.I32)),
                (inst, args) -> {
                    int countPtr = (int) args[0];
                    int sizePtr = (int) args[1];
                    inst.memory().writeI32(countPtr, 0);
                    inst.memory().writeI32(sizePtr, 0);
                    return new long[]{0};
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
     * @param funcName the name of the WASM exported function
     * @return an Optional containing the ExportFunction if found, otherwise empty
     */
    public Optional<ExportFunction> getWasmExtern(final String funcName) {
        ExportFunction fn = instance.export(funcName);
        if (Objects.isNull(fn)) {
            LOG.debug("WASM export function '{}' not found in {}", funcName, wasmName);
        }
        return Optional.ofNullable(fn);
    }
    
    /**
     * Get the WASM module's linear memory.
     *
     * @return the WASM linear memory
     */
    public Memory getMemory() {
        return instance.memory();
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
