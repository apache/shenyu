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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import org.dromara.soul.common.exception.SoulException;
import org.objectweb.asm.ClassReader;

/**
 * InterfaceMetadataReader
 *
 * @author sixh
 */
final class SimpleMetadataReader implements MetadataReader {

    private ClassLoader cl;

    private Resource resource;

    private ClassMetadata metadata;

    private AnnotationMetadata annotationMetadata;

    /**
     * Instantiates a new Simple metadata reader.
     *
     * @param resource the resource
     * @param cl       the cl
     */
    public SimpleMetadataReader(Resource resource, ClassLoader cl) {
        this.cl = cl;
        this.resource = resource;
        try {
            metadata(resource);
        } catch (IOException e) {
            throw new SoulException("reader class error" + resource.getFileName());
        }
    }

    private void metadata(Resource resource) throws IOException {
        InputStream is = new BufferedInputStream(resource.getInputStream());
        ClassReader classReader;
        try {
            classReader = new ClassReader(is);
        } catch (Exception e) {
            throw new SoulException("not found class " + resource.getFileName());
        } finally {
            is.close();
        }
        AnnotationMetadataReadingVisitor visitor = new AnnotationMetadataReadingVisitor(cl);
        classReader.accept(visitor, ClassReader.SKIP_DEBUG);
        this.annotationMetadata = visitor;
        this.metadata = visitor;
    }

    @Override
    public Resource getResource() {
        return resource;
    }

    @Override
    public ClassMetadata getClassMetadata() {
        return metadata;
    }

    @Override
    public ClassLoader getClassLoader() {
        return cl;
    }

    /**
     * Gets annotation metadata.
     *
     * @return the annotation metadata
     */
    @Override
    public AnnotationMetadata getAnnotationMetadata() {
        return annotationMetadata;
    }
}
