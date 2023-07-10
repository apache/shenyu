use crate::{
    exception::{joption_or_throw, runtime_error, Error},
    memory,
    memory::Memory,
    types::{jptr, Pointer},
    value::{Value, DOUBLE_CLASS, FLOAT_CLASS, INT_CLASS, LONG_CLASS},
};
use jni::{
    objects::{GlobalRef, JClass, JObject, JString, JValue},
    sys::{jbyteArray, jobjectArray},
    JNIEnv,
};
use std::{collections::HashMap, convert::TryFrom, panic, rc::Rc};
use wasmer_runtime as core;
use wasmer_runtime::{imports, instantiate, DynFunc, Export, Value as WasmValue};

pub struct Instance {
    pub java_instance_object: GlobalRef,
    pub instance: Rc<core::Instance>,
    pub memories: HashMap<String, Memory>,
}

impl Instance {
    fn new(java_instance_object: GlobalRef, module_bytes: Vec<u8>) -> Result<Self, Error> {
        let module_bytes = module_bytes.as_slice();
        let imports = imports! {};
        let instance = match instantiate(module_bytes, &imports) {
            Ok(instance) => Rc::new(instance),
            Err(e) => {
                return Err(runtime_error(format!(
                    "Failed to instantiate the module: {}",
                    e
                )))
            }
        };

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

        Ok(Self {
            java_instance_object,
            instance,
            memories,
        })
    }

    fn call_exported_function(
        &self,
        export_name: String,
        arguments: Vec<WasmValue>,
    ) -> Result<Box<[WasmValue]>, Error> {
        let function: DynFunc = self.instance.exports.get(&export_name).map_err(|_| {
            runtime_error(format!(
                "Exported function `{}` does not exist",
                export_name
            ))
        })?;

        function
            .call(arguments.as_slice())
            .map_err(|e| runtime_error(format!("{}", e)))
    }
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Instance_nativeInstantiate(
    env: JNIEnv,
    _class: JClass,
    this: JObject,
    module_bytes: jbyteArray,
) -> jptr {
    let output = panic::catch_unwind(|| {
        let module_bytes = env.convert_byte_array(module_bytes)?;
        let java_instance = env.new_global_ref(this)?;

        let instance = Instance::new(java_instance, module_bytes)?;

        Ok(Pointer::new(instance).into())
    });

    joption_or_throw(&env, output).unwrap_or(0)
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Instance_nativeDrop(
    _env: JNIEnv,
    _class: JClass,
    instance_pointer: jptr,
) {
    let _: Pointer<Instance> = instance_pointer.into();
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Instance_nativeCallExportedFunction<'a>(
    env: JNIEnv<'a>,
    _class: JClass,
    instance_pointer: jptr,
    export_name: JString,
    arguments_pointer: jobjectArray,
) -> jobjectArray {
    let output = panic::catch_unwind(|| {
        let instance: &Instance = Into::<Pointer<Instance>>::into(instance_pointer).borrow();
        let export_name: String = env.get_string(export_name)?.into();

        let arguments_length = env.get_array_length(arguments_pointer)?;

        let arguments = (0..arguments_length)
            .map(|i| env.get_object_array_element(arguments_pointer, i))
            .collect::<Result<Vec<JObject>, Error>>()?;

        let results = instance.call_exported_function(
            export_name.clone(),
            arguments
                .iter()
                .enumerate()
                .map(|(nth, argument)| {
                    Ok(
                        Value::try_from((&env, *argument))
                            .map_err(|_| {
                                runtime_error(format!(
                                    "Failed to convert the argument {}nth of `{}` into a WebAssembly value.",
                                    nth,
                                    export_name,
                                ))
                            })?
                            .inner())
                })
                .collect::<Result<Vec<WasmValue>, Error>>()?,
        )?;

        let obj_array = env.new_object_array(
            i32::try_from(results.len()).map_err(|e| runtime_error(e.to_string()))?,
            "java/lang/Object",
            JObject::null(),
        )?;

        if results.len() > 0 {
            for (nth, result) in results.iter().enumerate() {
                let obj = match result {
                    WasmValue::I32(val) => env.new_object(INT_CLASS, "(I)V", &[JValue::from(*val)]),
                    WasmValue::I64(val) => {
                        env.new_object(LONG_CLASS, "(J)V", &[JValue::from(*val)])
                    }
                    WasmValue::F32(val) => {
                        env.new_object(FLOAT_CLASS, "(F)V", &[JValue::from(*val)])
                    }
                    WasmValue::F64(val) => {
                        env.new_object(DOUBLE_CLASS, "(D)V", &[JValue::from(*val)])
                    }
                    _ => unreachable!(),
                }?;

                env.set_object_array_element(obj_array, nth as i32, obj)?;
            }

            Ok(obj_array)
        } else {
            Ok(JObject::null().into_inner())
        }
    });

    joption_or_throw(&env, output).unwrap_or(JObject::null().into_inner())
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Instance_nativeInitializeExportedFunctions(
    env: JNIEnv,
    _class: JClass,
    instance_pointer: jptr,
) {
    let output = panic::catch_unwind(|| {
        let instance: &Instance = Into::<Pointer<Instance>>::into(instance_pointer).borrow();

        let exports_object: JObject = env
            .get_field(
                instance.java_instance_object.as_obj(),
                "exports",
                "Lorg/apache/shenyu/wasm/Exports;",
            )?
            .l()?;

        for (export_name, export) in instance.instance.exports() {
            if let Export::Function { .. } = export {
                let name = env.new_string(export_name)?;

                env.call_method(
                    exports_object,
                    "addFunction",
                    "(Ljava/lang/String;)V",
                    &[JObject::from(name).into()],
                )?;
            }
        }
        Ok(())
    });

    joption_or_throw(&env, output).unwrap_or(())
}

#[no_mangle]
pub extern "system" fn Java_org_apache_shenyu_wasm_Instance_nativeInitializeExportedMemories(
    env: JNIEnv,
    _class: JClass,
    instance_pointer: jptr,
) {
    let output = panic::catch_unwind(|| {
        let instance: &Instance = Into::<Pointer<Instance>>::into(instance_pointer).borrow();

        memory::java::initialize_memories(&env, instance)?;

        Ok(())
    });

    joption_or_throw(&env, output).unwrap_or(())
}
