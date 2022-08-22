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

package org.apache.shenyu.jvmti;

import org.scijava.nativelib.JniExtractor;
import org.scijava.nativelib.NativeLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

/**
 * Use JVMTI technology to implement some things that Java code can't do.
 *
 * <p>Note: Only 64-bit CPU architecture is supported now !
 */
public class JVMTI {
    
    private static final Logger LOG = LoggerFactory.getLogger(JVMTI.class);
    
    private static final String LIB_NAME = "JniLibrary";
    
    static {
        try {
            final JniExtractor extractor = NativeLoader.getJniExtractor();
            final String path = extractor.extractJni("", LIB_NAME).getAbsolutePath();
            System.load(path);
        } catch (Throwable ignored) {
            try {
                File path = new File(JVMTI.class.getProtectionDomain().getCodeSource().getLocation().getPath());
                String libPath = new File(path, JVMTIUtils.detectLibName()).getAbsolutePath();
                System.load(libPath);
            } catch (Throwable t) {
                LOG.error("JVMTI init failed !", t);
            }
        }
    }
    
    /**
     * Get current surviving instance of a class in the jvm.
     *
     * @param klass class type
     * @param <T>   class type
     * @return current surviving instance
     * @throws RuntimeException if find many instances
     */
    public static <T> T getInstance(final Class<T> klass) {
        final T[] instances = getInstances0(klass, 1);
        if (null == instances || instances.length == 0) {
            return null;
        }
        if (instances.length > 1) {
            throw new RuntimeException("expect only one instance, actually find many instances !");
        }
        return instances[0];
    }
    
    /**
     * Get all current surviving instances of a class in the jvm.
     *
     * <p>Note: be careful to use this method !
     *
     * @param klass class type
     * @param <T>   class type
     * @return current surviving instances
     */
    public static <T> T[] getInstances(final Class<T> klass) {
        return getInstances0(klass, -1);
    }
    
    /**
     * Get all current surviving instances of a class in the jvm.
     *
     * <p>Note: be careful to use this method !
     *
     * @param klass class type
     * @param <T>   class type
     * @param limit instance limit, less than 0 means no limit.
     *              It is recommended to pass in a small {@code limit} value which is larger than 0.
     * @return current surviving instances
     */
    public static <T> T[] getInstances(final Class<T> klass, final int limit) {
        return getInstances0(klass, limit);
    }
    
    /**
     * Get all current surviving instances of a class in the jvm.
     *
     * <p>Note: Only 64-bit CPU architecture is supported now !
     *
     * @param klass class type
     * @param <T>   class type
     * @param limit instance limit, less than 0 means no limit
     * @return current surviving instances
     */
    private static synchronized native <T> T[] getInstances0(Class<T> klass, int limit);
}
