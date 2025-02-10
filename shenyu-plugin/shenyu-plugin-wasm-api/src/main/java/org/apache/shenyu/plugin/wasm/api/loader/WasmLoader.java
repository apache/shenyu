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

import io.github.kawamuray.wasmtime.Extern;
import io.github.kawamuray.wasmtime.Func;
import io.github.kawamuray.wasmtime.Linker;
import io.github.kawamuray.wasmtime.Memory;
import io.github.kawamuray.wasmtime.Module;
import io.github.kawamuray.wasmtime.Store;
import io.github.kawamuray.wasmtime.wasi.WasiCtx;
import io.github.kawamuray.wasmtime.wasi.WasiCtxBuilder;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmInitException;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

/**
 * The WasmLoader aims to load wasm file and provide the wasm function to java,
 * also provide the java function to wasm if we need.
 */
public class WasmLoader implements AutoCloseable {
    
    private static final String IMPORT_WASM_MODULE_NAME = "shenyu";
    
    private static final String MEMORY_METHOD_NAME = "memory";
    
    private final AtomicBoolean closed = new AtomicBoolean(false);
    
    private final String wasmName;
    
    private final WasiCtx wasiCtx = new WasiCtxBuilder().inheritStdout().inheritStderr().build();
    
    /**
     * the WASM store.
     *
     * @see io.github.kawamuray.wasmtime.WasmFunctions#consumer
     * @see io.github.kawamuray.wasmtime.WasmFunctions#func
     */
    private final Store<Void> store = Store.withoutData(wasiCtx);
    
    private final Linker linker = new Linker(store.engine());
    
    /**
     * wasmCallJavaFuncName -> wasmCallJavaFunc.
     */
    private final Map<String, Func> wasmCallJavaFuncMap = new HashMap<>();
    
    private final Module module;
    
    private final Memory memRef;
    
    public WasmLoader() {
        this(null, null);
    }
    
    /**
     * This constructor is designed for classes that cannot extend WasmLoader.
     *
     * @see io.github.kawamuray.wasmtime.WasmFunctions#wrap
     */
    public WasmLoader(final Class<?> wasmClass, final Function<Store<Void>, Map<String, Func>> initializer) {
        final Class<?> clazz = Objects.nonNull(wasmClass) ? wasmClass : this.getClass();
        this.wasmName = clazz.getName() + ".wasm";
        try {
            // locate `.wasm` lib.
            URL resource = clazz.getClassLoader().getResource(wasmName);
            if (Objects.isNull(resource)) {
                throw new ShenyuWasmInitException("Can't find wasm file: " + wasmName);
            }
            // Reads the WebAssembly module as bytes.
            byte[] wasmBytes = Files.readAllBytes(Paths.get(resource.toURI()));
            // Instantiates the WebAssembly module.
            if (Objects.nonNull(initializer)) {
                Map<String, Func> wasmFunctionMap = initializer.apply(store);
                if (Objects.nonNull(wasmFunctionMap) && !wasmFunctionMap.isEmpty()) {
                    wasmCallJavaFuncMap.putAll(wasmFunctionMap);
                }
            }
            Map<String, Func> wasmFunctionMap = initWasmCallJavaFunc(store);
            if (Objects.nonNull(wasmFunctionMap) && !wasmFunctionMap.isEmpty()) {
                wasmCallJavaFuncMap.putAll(wasmFunctionMap);
            }
            this.module = Module.fromBinary(store.engine(), wasmBytes);
            WasiCtx.addToLinker(linker);
            // maybe need define many functions
            if (!wasmCallJavaFuncMap.isEmpty()) {
                wasmCallJavaFuncMap.forEach((funcName, wasmCallJavaFunc) ->
                        linker.define(store, IMPORT_WASM_MODULE_NAME, funcName, Extern.fromFunc(wasmCallJavaFunc)));
            }
            linker.module(store, "", module);
            // Let the `wasmCallJavaFunc` function to refer this as a placeholder of Memory because
            // we have to add the function as import before loading the module exporting Memory.
            Optional<Extern> extern = this.getWasmExtern(MEMORY_METHOD_NAME);
            if (!extern.isPresent()) {
                throw new ShenyuWasmInitException(MEMORY_METHOD_NAME + " function not find in wasm file: " + wasmName);
            }
            this.memRef = extern.get().memory();
            Runtime.getRuntime().addShutdownHook(new Thread(this::close));
        } catch (URISyntaxException | IOException e) {
            throw new ShenyuWasmInitException(e);
        }
    }
    
    protected Map<String, Func> initWasmCallJavaFunc(final Store<Void> store) {
        return null;
    }
    
    /**
     * get the WASI function.
     *
     * @param wasiFuncName the WASI function name
     * @return the WASI function
     */
    public Optional<Extern> getWasmExtern(final String wasiFuncName) {
        return linker.get(store, "", wasiFuncName);
    }
    
    /**
     * get the wasm file name.
     *
     * @return wasm file name
     */
    public String getWasmName() {
        return wasmName;
    }
    
    /**
     * use this when call WASI.
     *
     * @return the Store
     */
    public Store<Void> getStore() {
        return store;
    }
    
    /**
     * use this in wasmCallJavaFunc.
     *
     * @return the ByteBuffer
     */
    public ByteBuffer getBuffer() {
        return memRef.buffer(store);
    }
    
    @Override
    public void close() {
        if (this.closed.compareAndSet(false, true)) {
            this.wasiCtx.close();
            this.store.close();
            this.linker.close();
            if (!wasmCallJavaFuncMap.isEmpty()) {
                this.wasmCallJavaFuncMap.forEach((funcName, wasmCallJavaFunc) -> wasmCallJavaFunc.close());
            }
            this.module.close();
        }
    }
}
