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

import java.io.File;
import java.util.concurrent.atomic.AtomicBoolean;
import org.scijava.nativelib.JniExtractor;
import org.scijava.nativelib.NativeLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Code reduced and simplified from zmq integration in Java. See
 * https://github.com/zeromq/jzmq/blob/3384ea1c04876426215fe76b5d1aabc58c099ca0/jzmq-jni/src/main/java/org/zeromq/EmbeddedLibraryTools.java.
 */
public class Native {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(Native.class);
    
    private static final AtomicBoolean INITED = new AtomicBoolean();
    
    private static final AtomicBoolean SUCCESS = new AtomicBoolean();
    
    /**
     * load the native library.
     *
     * @return true if load succeeded
     */
    public static boolean init() {
        if (!SUCCESS.get() && INITED.compareAndSet(false, true)) {
            try {
                final JniExtractor extractor = NativeLoader.getJniExtractor();
                final String path = extractor.extractJni("", "shenyu_wasm_build").getAbsolutePath();
                System.load(path);
                SUCCESS.set(true);
            } catch (Throwable ignored) {
                try {
                    File path = new File(Native.class.getProtectionDomain().getCodeSource().getLocation().getPath());
                    String libPath = new File(path, NativeUtils.detectLibName()).getAbsolutePath();
                    System.load(libPath);
                    SUCCESS.set(true);
                } catch (Throwable t) {
                    LOGGER.error("JVMTI init failed !", t);
                }
            }
        }
        return SUCCESS.get();
    }
}
