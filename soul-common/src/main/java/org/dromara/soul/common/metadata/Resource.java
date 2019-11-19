/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.common.metadata;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Resource
 *
 * @author sixh
 */
public interface Resource {

    /**
     * Create relative resource.
     *
     * @param relativePath the relative path
     * @return the resource.
     * @throws MalformedURLException the malformed url exception
     * @throws IOException           the io exception.
     */
    Resource createRelative(String relativePath) throws MalformedURLException, IOException;

    /**
     * Gets url.
     *
     * @return url url
     * @throws MalformedURLException the malformed url exception.
     */
    URL getURL() throws MalformedURLException;

    /**
     * Gets input stream.
     *
     * @return the input stream
     * @throws FileNotFoundException the file not found exception
     * @throws IOException           the io exception.
     */
    InputStream getInputStream() throws FileNotFoundException, IOException;

    /**
     * Gets file name.
     *
     * @return the file name.
     */
    String getFileName();
}
