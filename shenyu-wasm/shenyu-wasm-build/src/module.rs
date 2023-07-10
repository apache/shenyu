use crate::{
    exception::{joption_or_throw, runtime_error, Error},
    instance::Instance,
    memory::Memory,
    types::{jptr, Pointer},
};
use jni::{
    objects::{GlobalRef, JClass, JObject},
    sys::{jboolean, jbyteArray},
    JNIEnv,
};
use std::{collections::HashMap, panic, rc::Rc};
use wasmer_runtime::{self as runtime, validate, Export};
use wasmer_runtime::{cache::Artifact, imports, load_cache_with};

pub struct Module {
    #[allow(unused)]
    java_module_object: GlobalRef,
    module: runtime::Module,
}

impl Module {
    fn new(java_module_object: GlobalRef, module_bytes: Vec<u8>) -> Result<Self, Error> {
        let module_bytes = module_bytes.as_slice();
        let module = runtime::compile(module_bytes)
            .map_err(|e| runtime_error(format!("Failed to compile the module: {}", e)))?;

        Ok(Self {
            java_module_object,
            module,
        })
    }

    fn serialize(&self) -> Result<Vec<u8>, Error> {
        match self.module.cache() {
            Ok(artifact) => match artifact.serialize() {
                Ok(serialized_artifact) => Ok(serialized_artifact),
                Err(_) => {
                    return Err(runtime_error(format!(
                        "Failed to serialize the module artifact."
                    )))
                }
            },
            Err(_) => return Err(runtime_error(format!("Failed to get the module artifact."))),
        }
    }

    fn deserialize(
        java_module_object: GlobalRef,
        serialized_module: Vec<u8>,
    ) -> Result<Self, Error> {
        let module = match unsafe { Artifact::deserialize(serialized_module.as_slice()) } {
            Ok(artifact) => match load_cache_with(artifact) {
                Ok(module) => module,
                Err(_) => {
                    return Err(runtime_error(format!(
                        "Failed to compile the serialized module."
                    )))
                }
            },
            Err(_) => return Err(runtime_error(format!("Failed to deserialize the module."))),
        };

        Ok(Self {
            java_module_object,
            module,
        })
    }
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Module_nativeModuleInstantiate(
    env: JNIEnv,
    _class: JClass,
    this: JObject,
    module_bytes: jbyteArray,
) -> jptr {
    let output = panic::catch_unwind(|| {
        let module_bytes = env.convert_byte_array(module_bytes)?;
        let java_module = env.new_global_ref(this)?;

        let module = Module::new(java_module, module_bytes)?;

        Ok(Pointer::new(module).into())
    });

    joption_or_throw(&env, output).unwrap_or(0)
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Module_nativeDrop(
    _env: JNIEnv,
    _class: JClass,
    module_pointer: jptr,
) {
    let _: Pointer<Module> = module_pointer.into();
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Module_nativeInstantiate(
    env: JNIEnv,
    _class: JClass,
    module_pointer: jptr,
    instance_object: JObject,
) -> jptr {
    let output = panic::catch_unwind(|| {
        let java_instance_object = env.new_global_ref(instance_object)?;

        let module: &Module = Into::<Pointer<Module>>::into(module_pointer).borrow();
        let import_object = imports! {};
        let instance = module.module.instantiate(&import_object).map_err(|e| {
            runtime_error(format!("Failed to instantiate a WebAssembly module: {}", e))
        })?;

        let memories: HashMap<String, Memory> = instance
            .exports()
            .filter_map(|(export_name, export)| match export {
                Export::Memory(memory) => Some((
                    export_name.to_string(),
                    Memory::new(Rc::new(memory.clone())),
                )),
                _ => None,
            })
            .collect();

        Ok(Pointer::new(Instance {
            java_instance_object,
            instance: Rc::new(instance),
            memories,
        })
        .into())
    });

    joption_or_throw(&env, output).unwrap_or(0)
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Module_nativeValidate(
    env: JNIEnv,
    _class: JClass,
    module_bytes: jbyteArray,
) -> jboolean {
    let output = panic::catch_unwind(|| {
        let module_bytes = env.convert_byte_array(module_bytes)?;
        match validate(module_bytes.as_slice()) {
            true => Ok(1),
            false => Ok(0),
        }
    });

    joption_or_throw(&env, output).unwrap_or(0)
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Module_nativeSerialize(
    env: JNIEnv,
    _class: JClass,
    module_pointer: jptr,
) -> jbyteArray {
    let output = panic::catch_unwind(|| {
        let module: &Module = Into::<Pointer<Module>>::into(module_pointer).borrow();
        let serialized_module = module.serialize()?;
        let java_serialized_module = env.byte_array_from_slice(serialized_module.as_slice())?;
        Ok(java_serialized_module)
    });

    joption_or_throw(&env, output).unwrap_or(JObject::null().into_inner())
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Module_nativeDeserialize(
    env: JNIEnv,
    _class: JClass,
    java_module: JObject,
    java_serialized_module: jbyteArray,
) -> jptr {
    let output = panic::catch_unwind(|| {
        let java_module_object = env.new_global_ref(java_module)?;
        let serialized_module = env.convert_byte_array(java_serialized_module)?;
        let module = Module::deserialize(java_module_object, serialized_module)?;
        Ok(Pointer::new(module).into())
    });

    joption_or_throw(&env, output).unwrap_or(0)
}
