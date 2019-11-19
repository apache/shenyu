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

import java.util.LinkedHashSet;
import java.util.Set;
import org.objectweb.asm.*;

/**
 * ASM class visitor, which looks only for the class name and implemented types,
 * interface.
 */
class ClassMetadataReadingVisitor extends ClassVisitor implements ClassMetadata {

    private String className = "";

    private boolean isInterface;

    private boolean isAnnotation;

    private boolean isAbstract;

    private boolean isFinal;

    private String enclosingClassName;

    private boolean independentInnerClass;

    private String superClassName;

    private String[] interfaces = new String[0];

    private Set<String> memberClassNames = new LinkedHashSet<>();

    public ClassMetadataReadingVisitor() {
        super(Opcodes.ASM7);
    }

    @Override
    public void visit(
            int version, int access, String name, String signature, String supername, String[] interfaces) {
        this.className = convertResourcePathToClassName(name);
        this.isInterface = ((access & Opcodes.ACC_INTERFACE) != 0);
        this.isAnnotation = ((access & Opcodes.ACC_ANNOTATION) != 0);
        this.isAbstract = ((access & Opcodes.ACC_ABSTRACT) != 0);
        this.isFinal = ((access & Opcodes.ACC_FINAL) != 0);
        if (supername != null && !this.isInterface) {
            this.superClassName = convertResourcePathToClassName(supername);
        }
        this.interfaces = new String[interfaces.length];
        for (int i = 0; i < interfaces.length; i++) {
            this.interfaces[i] = convertResourcePathToClassName(interfaces[i]);
        }
    }

    @Override
    public void visitOuterClass(String owner, String name, String desc) {
        this.enclosingClassName = convertResourcePathToClassName(owner);
    }

    @Override
    public void visitInnerClass(String name, String outerName, String innerName, int access) {
        if (outerName != null) {
            String fqName = convertResourcePathToClassName(name);
            String fqOuterName = convertResourcePathToClassName(outerName);
            if (this.className.equals(fqName)) {
                this.enclosingClassName = fqOuterName;
                this.independentInnerClass = ((access & Opcodes.ACC_STATIC) != 0);
            } else if (this.className.equals(fqOuterName)) {
                this.memberClassNames.add(fqName);
            }
        }
    }

    private static String convertResourcePathToClassName(String resourcePath) {
        return resourcePath.replace("/", ".");
    }

    @Override
    public void visitSource(String source, String debug) {
        // no-op
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        // no-op
        return new EmptyAnnotationVisitor();
    }

    @Override
    public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
        // no-op
        return new EmptyFieldVisitor();
    }

    @Override
    public MethodVisitor visitMethod(int i, String s, String s1, String s2, String[] strings) {
        return new EmptyMethodVisitor();
    }

    @Override
    public void visitEnd() {
        // no-op
    }

    @Override
    public String getClassName() {
        return this.className;
    }

    @Override
    public boolean isInterface() {
        return this.isInterface;
    }

    @Override
    public boolean isAnnotation() {
        return this.isAnnotation;
    }

    @Override
    public boolean isAbstract() {
        return this.isAbstract;
    }

    @Override
    public boolean isConcrete() {
        return !(this.isInterface || this.isAbstract);
    }

    @Override
    public boolean isFinal() {
        return this.isFinal;
    }


    @Override
    public boolean hasSuperClass() {
        return (this.superClassName != null);
    }

    @Override
    public String getSuperClass() {
        return this.superClassName;
    }

    @Override
    public String[] getInterfaceNames() {
        return this.interfaces;
    }

    @Override
    public boolean isIndependent() {
        return (this.enclosingClassName == null || this.independentInnerClass);
    }

    @Override
    public boolean hasEnclosingClass() {
        return (this.enclosingClassName != null);
    }

    @Override
    public String getEnclosingClassName() {
        return this.enclosingClassName;
    }

    private static class EmptyAnnotationVisitor extends AnnotationVisitor {

        public EmptyAnnotationVisitor() {
            super(Opcodes.ASM7);
        }

        @Override
        public AnnotationVisitor visitAnnotation(String name, String desc) {
            return this;
        }

        @Override
        public AnnotationVisitor visitArray(String name) {
            return this;
        }
    }

    private static class EmptyMethodVisitor extends MethodVisitor {

        public EmptyMethodVisitor() {
            super(Opcodes.ASM7);
        }
    }

    private static class EmptyFieldVisitor extends FieldVisitor {

        public EmptyFieldVisitor() {
            super(Opcodes.ASM7);
        }
    }

}
