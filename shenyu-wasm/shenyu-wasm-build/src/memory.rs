use crate::{
    exception::{joption_or_throw, runtime_error, Error},
    types::{jptr, Pointer},
};
use jni::{
    objects::{JClass, JObject},
    sys::jint,
    JNIEnv,
};
use std::{cell::Cell, panic, rc::Rc, slice};
use wasmer_runtime::memory::MemoryView;
use wasmer_runtime::units::Pages;
use wasmer::Memory as WasmMemory;

#[derive(Clone)]
pub struct Memory {
    pub memory: Rc<WasmMemory>,
}

impl Memory {
    pub fn new(memory: Rc<WasmMemory>) -> Self {
        Self { memory }
    }

    pub fn grow(&self, number_of_pages: u32) -> Result<u32, Error> {
        self.memory
            .grow(Pages(number_of_pages))
            .map(|previous_pages| previous_pages.0)
            .map_err(|e| runtime_error(format!("Failed to grow the memory: {}", e)))
    }
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasmer_Memory_nativeMemoryView(
    env: JNIEnv,
    _class: JClass,
    memory_object: JObject,
    memory_pointer: jptr,
) {
    let output = panic::catch_unwind(|| {
        let memory: &Memory = Into::<Pointer<Memory>>::into(memory_pointer).borrow();
        let view: MemoryView<u8> = memory.memory.view();
        let data = unsafe {
            slice::from_raw_parts_mut(view[..].as_ptr() as *mut Cell<u8> as *mut u8, view.len())
        };

        // Create a new `JByteBuffer`, aka `java.nio.ByteBuffer`,
        // borrowing the data from the WebAssembly memory.
        let byte_buffer = env.new_direct_byte_buffer(data)?;

        // Try to rewrite the `org.wasmer.Memory.buffer` attribute by
        // calling the `org.wasmer.Memory.setBuffer` method.
        env.call_method(
            memory_object,
            "setBuffer",
            "(Ljava/nio/ByteBuffer;)V",
            &[JObject::from(byte_buffer).into()],
        )?;

        Ok(())
    });

    joption_or_throw(&env, output);
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasmer_Memory_nativeMemoryGrow(
    env: JNIEnv,
    _class: JClass,
    memory_object: JObject,
    memory_pointer: jptr,
    number_of_pages: jint,
) -> jint {
    let output = panic::catch_unwind(|| {
        let memory: &Memory = Into::<Pointer<Memory>>::into(memory_pointer).borrow();
        let old_pages = memory.grow(number_of_pages as u32)?;

        let view: MemoryView<u8> = memory.memory.view();
        let data = unsafe {
            std::slice::from_raw_parts_mut(
                view[..].as_ptr() as *mut Cell<u8> as *mut u8,
                view.len(),
            )
        };
        // Create a new `JByteBuffer`, aka `java.nio.ByteBuffer`,
        // borrowing the data from the WebAssembly memory.
        let byte_buffer = env.new_direct_byte_buffer(data)?;

        // Try to rewrite the `org.wasmer.Memory.buffer` attribute by
        // calling the `org.wasmer.Memory.setBuffer` method.
        env.call_method(
            memory_object,
            "setBuffer",
            "(Ljava/nio/ByteBuffer;)V",
            &[JObject::from(byte_buffer).into()],
        )?;

        Ok(old_pages as i32)
    });

    joption_or_throw(&env, output).unwrap_or(0)
}

pub mod java {
    use crate::{
        exception::Error,
        instance::Instance,
        types::{jptr, Pointer},
    };
    use jni::{objects::JObject, JNIEnv};

    pub fn initialize_memories(env: &JNIEnv, instance: &Instance) -> Result<(), Error> {
        let exports_object: JObject = env
            .get_field(
                instance.java_instance_object.as_obj(),
                "exports",
                "Lorg/wasmer/Exports;",
            )?
            .l()?;

        // Get the `org.wasmer.Memory` class.
        let memory_class = env.find_class("org/wasmer/Memory")?;

        for (memory_name, memory) in &instance.memories {
            // Instantiate the `Memory` class.
            let memory_object = env.new_object(memory_class, "()V", &[])?;

            // Try to set the memory pointer to the field `org.wasmer.Memory.memoryPointer`.
            let memory_pointer: jptr = Pointer::new(memory.clone()).into();
            env.set_field(memory_object, "memoryPointer", "J", memory_pointer.into())?;

            // Add the newly created `org.wasmer.Memory` in the
            // `org.wasmer.Exports` collection.
            env.call_method(
                exports_object,
                "addMemory",
                "(Ljava/lang/String;Lorg/wasmer/Memory;)V",
                &[
                    JObject::from(env.new_string(memory_name)?).into(),
                    memory_object.into(),
                ],
            )?;
        }

        Ok(())
    }
}
