package org.apache.shenyu.wasm.exports;

/**
 * Functional interface for WebAssembly exported functions, i.e. it creates a new type for a closure that mimics a
 * WebAssembly exported function.
 * <p>
 * The apply method takes an arbitrary number of arguments and returns an output.
 */
@FunctionalInterface
public interface Function extends Export {
    
    Object[] apply(Object... inputs);
}
