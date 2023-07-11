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

import org.apache.shenyu.wasm.exports.Function;

/**
 * `Instance` is a Java class that represents a WebAssembly instance.
 *
 * <p>Example:
 * <pre>{@code
 * Instance instance = new Instance(wasmBytes);
 * }</pre>
 */
public class Instance {
    
    static {
        // Native bindings.
        Native.init();
    }
    
    /**
     * All WebAssembly exports.
     */
    private final Exports exports;
    
    /**
     * The instance pointer.
     */
    private long instancePointer;
    
    /**
     * The constructor instantiates a new WebAssembly instance based on WebAssembly bytes.
     *
     * @param moduleBytes WebAssembly bytes.
     */
    public Instance(final byte[] moduleBytes) {
        this.exports = new Exports(this);
        
        long instancePointer = this.nativeInstantiate(this, moduleBytes);
        this.instancePointer = instancePointer;
        
        nativeInitializeExportedFunctions(instancePointer);
        nativeInitializeExportedMemories(instancePointer);
    }
    
    protected Instance() {
        this.exports = new Exports(this);
    }
    
    /**
     * Delete an instance object pointer.
     */
    public void close() {
        if (this.instancePointer != 0L) {
            this.nativeDrop(this.instancePointer);
            this.instancePointer = 0L;
        }
    }
    
    /**
     * Delete an instance object pointer, which is called by the garbage collector before an object is removed from the
     * memory.
     */
    @Override
    protected void finalize() throws Throwable {
        this.close();
        super.finalize();
    }
    
    /**
     * get instancePointer.
     *
     * @return the instance pointer
     */
    public long getInstancePointer() {
        return instancePointer;
    }
    
    /**
     * set instancePointer.
     *
     * @param instancePointer the instance pointer
     */
    public void setInstancePointer(final long instancePointer) {
        this.instancePointer = instancePointer;
    }
    
    /**
     * Return the export with the name `name` as an exported function.
     *
     * @param name Name of the exported function.
     * @return the exported function
     * @throws ClassCastException if class cast failed
     */
    public Function getFunction(final String name) throws ClassCastException {
        return this.exports.getFunction(name);
    }
    
    private native long nativeInstantiate(Instance self, byte[] moduleBytes);
    
    private native void nativeDrop(long instancePointer);
    
    protected native Object[] nativeCallExportedFunction(long instancePointer, String exportName, Object[] arguments);
    
    protected static native void nativeInitializeExportedFunctions(long instancePointer);
    
    protected static native void nativeInitializeExportedMemories(long instancePointer);
    
}
