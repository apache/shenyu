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

package org.apache.shenyu.common.utils;

import org.apache.shenyu.common.exception.ShenyuException;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Jar package dependency tools.
 */
public class JarDependencyUtils {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(JarDependencyUtils.class);

    /**
     * Obtain the dependency tree of the jar package file at compile time.
     *
     * @param jarBytes Byte array of jar file
     * @return dependency tree
     */
    public static Set<String> getDependencyTree(final byte[] jarBytes) {
        Set<String> dependencies = new HashSet<>();
        try (InputStream inputStream = new ByteArrayInputStream(jarBytes);
             ZipInputStream zipInputStream = new ZipInputStream(inputStream)) {
            ZipEntry entry;
            while ((entry = zipInputStream.getNextEntry()) != null) {
                if (entry.getName().endsWith(".class")) {
                    ClassNode classNode = new ClassNode(Opcodes.ASM7);
                    ClassReader classReader = new ClassReader(zipInputStream);
                    classReader.accept(classNode, 0);
                    addDependencies(classNode.superName, dependencies);
                    for (String interfaceName : classNode.interfaces) {
                        addDependencies(interfaceName, dependencies);
                    }
                    for (FieldNode fieldNode : classNode.fields) {
                        addDependencies(Type.getType(fieldNode.desc).getClassName(), dependencies);
                    }
                    for (MethodNode methodNode : classNode.methods) {
                        addDependencies(Type.getReturnType(methodNode.desc).getClassName(), dependencies);
                        for (Type argumentType : Type.getArgumentTypes(methodNode.desc)) {
                            addDependencies(argumentType.getClassName(), dependencies);
                        }
                    }
                }
            }
            return dependencies;

        } catch (Exception e) {
            LOG.error("get dependency tree error", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * Add dependencies.
     * @param typeName type name
     * @param dependencies dependencies
     */
    private static void addDependencies(final String typeName, final Set<String> dependencies) {
        if (!typeName.startsWith("java") && !typeName.startsWith("javax")) {
            dependencies.add(typeName.replace("/", "."));
        }
    }
}
