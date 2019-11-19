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
import java.util.Map;
import java.util.Set;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * MethodReadingVisitor
 *
 * @author sixh
 */
final class MethodReadingVisitor extends MethodVisitor implements MethodMetadata {

    private final String methodName;

    private final int access;

    private final String declaringClassName;

    private final String returnTypeName;

    private final ClassLoader classLoader;

    private final Set<MethodMetadata> methodMetadataSet;

    private final Map<String, Set<String>> metaAnnotationMap = new LinkedHashMap<>(4);

    MethodReadingVisitor(String methodName, int access, String declaringClassName,
                         String returnTypeName, ClassLoader classLoader, Set<MethodMetadata> methodMetadataSet) {
        super(Opcodes.ASM7);
        this.methodName = methodName;
        this.access = access;
        this.declaringClassName = declaringClassName;
        this.returnTypeName = returnTypeName;
        this.classLoader = classLoader;
        this.methodMetadataSet = methodMetadataSet;
    }

    @Override
    public String getMethodName() {
        return this.methodName;
    }

    @Override
    public String getDeclaringClassName() {
        return this.declaringClassName;
    }

    @Override
    public String getReturnTypeName() {
        return this.returnTypeName;
    }

    @Override
    public boolean isAbstract() {
        return ((this.access & Opcodes.ACC_ABSTRACT) != 0);
    }

    @Override
    public boolean isStatic() {
        return ((this.access & Opcodes.ACC_STATIC) != 0);
    }

    @Override
    public boolean isFinal() {
        return ((this.access & Opcodes.ACC_FINAL) != 0);
    }

    @Override
    public boolean isOverridable() {
        return (!isStatic() && !isFinal() && ((this.access & Opcodes.ACC_PRIVATE) == 0));
    }

    @Override
    public boolean isAnnotated(String annotationName) {
        return metaAnnotationMap.containsKey(annotationName);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        this.methodMetadataSet.add(this);
        String className = Type.getType(desc).getClassName();
        return new AnnotationReadingVisitor(
                className, this.metaAnnotationMap, this.classLoader);
    }
}
