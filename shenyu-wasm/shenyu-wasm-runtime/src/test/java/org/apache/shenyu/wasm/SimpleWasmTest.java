package org.apache.shenyu.wasm;

import org.junit.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class SimpleWasmTest {
    @Test
    public void test() throws Throwable {
        // `simple.wasm` is located at `tests/resources/`.
        Path wasmPath = Paths.get(this.getClass().getClassLoader().getResource("simple.wasm").toURI());
        
        // Reads the WebAssembly module as bytes.
        byte[] wasmBytes = Files.readAllBytes(wasmPath);
        
        // Instantiates the WebAssembly module.
        Instance instance = new Instance(wasmBytes);
        
        // Calls an exported function, and returns an object array.
        Object[] results = instance.exports.getFunction("sum").apply(5, 37);
        
        System.out.println(results[0]); // 42
        
        // Drops an instance object pointer which is stored in Rust.
        instance.close();
    }
}
