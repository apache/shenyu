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

package org.apache.shenyu.wasm;

import java.util.HashMap;
import java.util.Map;
import org.apache.shenyu.wasm.exports.Export;
import org.apache.shenyu.wasm.exports.Function;

/**
 * `Exports` is a Java class that represents the set of WebAssembly exports.
 *
 * <p>Example:
 * <pre>{@code
 * Instance instance = new Instance(wasmBytes);
 *
 * // Get and run an exported function.
 * Object[] result = instance.exports.getFunction("sum").apply(1, 2);
 *
 * // Get, manually downcast, and run an exported function.
 * Export sum = instance.exports.get("sum");
 * Object[] result = ((Function) sum).apply(1, 2);
 * }</pre>
 */
@SuppressWarnings("unused")
public class Exports {
    
    private final Map<String, Export> inner;
    
    private Instance instance;
    
    /**
     * Lambda expression for currying. This takes a function name and returns the function to call WebAssembly
     * function.
     */
    private final java.util.function.Function<String, Function> functionWrapperGenerator =
        functionName -> arguments -> this.instance.nativeCallExportedFunction(
                this.instance.getInstancePointer(), functionName, arguments);
    
    /**
     * The constructor instantiates new exported functions.
     *
     * @param instance Instance object which holds the exports object.
     */
    protected Exports(final Instance instance) {
        this.inner = new HashMap<>();
        this.instance = instance;
    }
    
    /**
     * Return the export with the name `name`.
     *
     * @param name name of the export to return.
     * @return the export
     */
    public Export get(final String name) {
        return this.inner.get(name);
    }
    
    /**
     * Return the export with the name `name` as an exported function.
     *
     * @param name Name of the exported function.
     * @return the exported function
     * @throws ClassCastException if class cast failed
     */
    public Function getFunction(final String name) throws ClassCastException {
        return (Function) this.inner.get(name);
    }
    
    /**
     * Return the export with the name `name` as an exported memory.
     *
     * @param name Name of the exported memory.
     * @return The exported memory with the name
     * @throws ClassCastException if class cast failed
     */
    public Memory getMemory(final String name) throws ClassCastException {
        return (Memory) this.inner.get(name);
    }
    
    /**
     * Called by Rust to add a new exported function.
     */
    private void addFunction(final String name) {
        this.inner.put(name, this.generateFunctionWrapper(name));
    }
    
    /**
     * Called by Rust to add a new exported memory.
     */
    private void addMemory(final String name, final Memory memory) {
        this.inner.put(name, memory);
    }
    
    /**
     * Generate the exported function wrapper.
     */
    private Function generateFunctionWrapper(final String functionName) {
        return this.functionWrapperGenerator.apply(functionName);
    }
}
