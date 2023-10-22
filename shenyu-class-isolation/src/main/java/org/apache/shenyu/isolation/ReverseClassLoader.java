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

package org.apache.shenyu.isolation;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Objects;

public class ReverseClassLoader extends URLClassLoader {

    public ReverseClassLoader(final URL[] urls, final ClassLoader parent) {
        super(urls, parent);
    }

    public ReverseClassLoader(final URL[] urls) {
        super(urls);
    }

    /**
     * Add URL.
     * @param url the URL to be added to the search path of URLs
     */
    public void addURL(final URL url) {
        super.addURL(url);
    }

    @Override
    public Class<?> loadClass(final String name) throws ClassNotFoundException {
        return loadClass(name, false);
    }

    @Override
    protected Class<?> loadClass(final String name, final boolean resolve) throws ClassNotFoundException {
        synchronized (getClassLoadingLock(name)) {
            Class<?> c = null;
            if (c == null) {
                c = findLoadedClass(name);
                try {
                    if (c == null) {
                        c = findClass(name);
                    }
                } catch (ClassNotFoundException e) {
                }
            }
            if (c == null) {
                c = super.loadClass(name, resolve);
            }
            if (resolve) {
                resolveClass(c);
            }
            return c;
        }
    }
}




























