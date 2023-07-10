package org.apache.shenyu.wasm;

import java.util.HashMap;
import java.util.Map;
import org.apache.shenyu.wasm.exports.Export;
import org.apache.shenyu.wasm.exports.Function;

/**
 * `Exports` is a Java class that represents the set of WebAssembly exports.
 *
 * Example:
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
public class Exports {
    private Map<String, Export> inner;
    private Instance instance;

    /**
     * The constructor instantiates new exported functions.
     *
     * @param instance Instance object which holds the exports object.
     */
    protected Exports(Instance instance) {
        this.inner = new HashMap<String, Export>();
        this.instance = instance;
    }

    /**
     * Return the export with the name `name`.
     *
     * @param name Name of the export to return.
     */
    public Export get(String name) {
        return this.inner.get(name);
    }

    /**
     * Return the export with the name `name` as an exported function.
     *
     * @param name Name of the exported function.
     */
    public Function getFunction(String name) throws ClassCastException {
        return (Function) this.inner.get(name);
    }

    /**
     * Return the export with the name `name` as an exported memory.
     *
     * @param name Name of the exported memory.
     */
    public Memory getMemory(String name) throws ClassCastException {
        return (Memory) this.inner.get(name);
    }

    /**
     * Called by Rust to add a new exported function.
     */
    private void addFunction(String name) {
        this.inner.put(name, this.generateFunctionWrapper(name));
    }

    /**
     * Called by Rust to add a new exported memory.
     */
    private void addMemory(String name, Memory memory) {
        this.inner.put(name, memory);
    }

    /**
     * Lambda expression for currying.
     * This takes a function name and returns the function to call WebAssembly function.
     */
    private java.util.function.Function<String, Function> functionWrapperGenerator =
        functionName -> arguments -> this.instance.nativeCallExportedFunction(this.instance.instancePointer, functionName, arguments);

    /**
     * Generate the exported function wrapper.
     */
    private Function generateFunctionWrapper(String functionName) {
        return this.functionWrapperGenerator.apply(functionName);
    }
}
