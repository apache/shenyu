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

/**
 * `Module` is a Java class that represents a WebAssembly module.
 *
 * <p>Example:
 * <pre>{@code
 * boolean isValid = Module.validate(wasmBytes);
 *
 * Module module = new Module(wasmBytes);
 * Instance instance = module.instantiate();
 * }</pre>
 */
@SuppressWarnings("unused")
public class Module {
    
    static {
        // Native bindings.
        Native.init();
    }
    
    private long modulePointer;
    
    private Module() {
    }
    
    /**
     * The constructor instantiates a new WebAssembly module based on WebAssembly bytes.
     *
     * @param moduleBytes webassembly bytes.
     */
    public Module(final byte[] moduleBytes) {
        this.modulePointer = this.nativeModuleInstantiate(this, moduleBytes);
    }
    
    /**
     * Create an original Module object from a byte array.
     *
     * @param serializedBytes serialized bytes
     *
     * @return Module object.
     */
    public static Module deserialize(final byte[] serializedBytes) {
        Module module = new Module();
        module.modulePointer = Module.nativeDeserialize(module, serializedBytes);
        return module;
    }
    
    /**
     * Check that given bytes represent a valid WebAssembly module.
     *
     * @param moduleBytes WebAssembly bytes.
     * @return true if, and only if, given bytes are valid as a WebAssembly module.
     */
    public static boolean validate(final byte[] moduleBytes) {
        return Module.nativeValidate(moduleBytes);
    }
    
    /**
     * Delete a module object pointer.
     */
    public void close() {
        if (this.modulePointer != 0L) {
            this.nativeDrop(this.modulePointer);
            this.modulePointer = 0L;
        }
    }
    
    /**
     * Delete a module object pointer, which is called by the garbage collector before an object is removed from the
     * memory.
     */
    @Override
    public void finalize() throws Throwable {
        this.close();
        super.finalize();
    }
    
    /**
     * Create an instance object based on a module object.
     *
     * @return Instance object.
     */
    public Instance instantiate() {
        Instance instance = new Instance();
        long instancePointer = this.nativeInstantiate(this.modulePointer, instance);
        instance.setInstancePointer(instancePointer);
        
        Instance.nativeInitializeExportedFunctions(instancePointer);
        Instance.nativeInitializeExportedMemories(instancePointer);
        return instance;
    }
    
    /**
     * Create a serialized byte array from a WebAssembly module.
     *
     * @return Serialized bytes.
     */
    public byte[] serialize() {
        return this.nativeSerialize(this.modulePointer);
    }
    
    private native long nativeModuleInstantiate(Module self, byte[] moduleBytes);
    
    private native void nativeDrop(long modulePointer);
    
    private native long nativeInstantiate(long modulePointer, Instance instance);
    
    private static native boolean nativeValidate(byte[] moduleBytes);
    
    private native byte[] nativeSerialize(long modulePointer);
    
    private static native long nativeDeserialize(Module module, byte[] serializedBytes);
    
}
