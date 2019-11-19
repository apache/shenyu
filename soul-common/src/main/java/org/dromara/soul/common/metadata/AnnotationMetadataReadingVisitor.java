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

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * AnnotationMetadataReadingVisitor
 *
 * @author sixh
 */
final class AnnotationMetadataReadingVisitor extends ClassMetadataReadingVisitor implements AnnotationMetadata {
    private final ClassLoader classLoader;

    private final Set<String> annotationSet = new LinkedHashSet<>(4);

    private final Map<String, Set<String>> metaAnnotationMap = new LinkedHashMap<>(4);

    private final Set<MethodMetadata> methodMetadataSet = new LinkedHashSet<>(4);

    AnnotationMetadataReadingVisitor(ClassLoader classLoader) {
        this.classLoader = classLoader;
    }

    @Override
    public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
        // Skip bridge methods - we're only interested in original annotation-defining user methods.
        // On JDK 8, we'd otherwise run into double detection of the same annotated method...
        if ((access & Opcodes.ACC_BRIDGE) != 0) {
            return super.visitMethod(access, name, desc, signature, exceptions);
        }
        return new MethodReadingVisitor(name, access, getClassName(),
                                        Type.getReturnType(desc).getClassName(), this.classLoader, this.methodMetadataSet);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        String className = Type.getType(desc).getClassName();
        this.annotationSet.add(className);
        return new AnnotationReadingVisitor(className, this.metaAnnotationMap, this.classLoader);
    }

    @Override
    public Set<String> getAnnotationTypes() {
        return this.annotationSet;
    }

    @Override
    public boolean hasAnnotation(String annotationName) {
        return this.annotationSet.contains(annotationName);
    }

    @Override
    public Set<MethodMetadata> getAnnotatedMethods(String annotationName) {
        Set<MethodMetadata> annotatedMethods = new LinkedHashSet<>(4);
        for (MethodMetadata methodMetadata : this.methodMetadataSet) {
            if (methodMetadata.isAnnotated(annotationName)) {
                annotatedMethods.add(methodMetadata);
            }
        }
        return annotatedMethods;
    }

    @Override
    public boolean isAnnotated(String annotationName) {
        return metaAnnotationMap.containsKey(annotationName);
    }
}
