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

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import org.apache.shenyu.wasm.exports.Export;

/**
 * `Memory` is a Java class that represents a WebAssembly memory.
 *
 * <p>Example:
 * <pre>{@code
 * Instance instance = new Instance(wasmBytes);
 * Memory memory = instance.exports.getMemory("memory-name");
 * ByteBuffer memoryBuffer = memory.buffer();
 *
 * // Write bytes.
 * memoryBuffer.position(0);
 * memoryBuffer.put(new byte[]{1, 2, 3, 4, 5});
 *
 * // Read bytes.
 * byte[] bytes = new byte[5];
 * memoryBuffer.position(0);
 * memoryBuffer.get(bytes);
 * }</pre>
 */
@SuppressWarnings("unused")
public final class Memory implements Export {
    
    /**
     * Represents the actual WebAssembly memory data, borrowed from the runtime (in Rust). The `setBuffer` method must
     * be used to set this attribute.
     */
    private ByteBuffer buffer;
    
    private long memoryPointer;
    
    private Memory() {
        // This object is instantiated by Rust.
    }
    
    /**
     * Return a _new_ direct byte buffer borrowing the memory data.
     *
     * @return A new direct byte buffer.
     */
    public ByteBuffer buffer() {
        this.nativeMemoryView(this, this.memoryPointer);
        
        return this.buffer;
    }
    
    /**
     * Set the `ByteBuffer` of this memory. See `Memory.buffer` to learn more.
     *
     * <p>In addition, this method correctly sets the endianess of the `ByteBuffer`.
     */
    private void setBuffer(final ByteBuffer buffer) {
        this.buffer = buffer;
        
        // Ensure the endianess matches WebAssemly specification.
        if (this.buffer.order() != ByteOrder.LITTLE_ENDIAN) {
            this.buffer.order(ByteOrder.LITTLE_ENDIAN);
        }
    }
    
    /**
     * Grow this memory by the specified number of pages.
     *
     * @param page The number of pages to grow. 1 page size is 64KiB.
     * @return The previous number of pages.
     */
    public int grow(final int page) {
        return this.nativeMemoryGrow(this, this.memoryPointer, page);
    }
    
    private native void nativeMemoryView(Memory memory, long memoryPointer);
    
    private native int nativeMemoryGrow(Memory memory, long memoryPointer, int page);
    
}
