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

import java.util.Locale;

/**
 * The type JVMTI utils.
 */
public class NativeUtils {
    
    private static final String OPERATING_SYSTEM_NAME = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);
    
    private static String libName;
    
    static {
        if (OPERATING_SYSTEM_NAME.startsWith("mac") || OPERATING_SYSTEM_NAME.startsWith("darwin")) {
            libName = "libshenyu_wasm_build.dylib";
        }
        if (OPERATING_SYSTEM_NAME.startsWith("linux")) {
            libName = "libshenyu_wasm_build.so";
        }
        if (OPERATING_SYSTEM_NAME.startsWith("windows")) {
            libName = "libshenyu_wasm_build.dll";
        }
    }
    
    /**
     * detect native library name.
     *
     * @return the native library name
     */
    public static String detectLibName() {
        return libName;
    }
}
