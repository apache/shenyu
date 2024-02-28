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

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;

import com.google.common.collect.Lists;

import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.apache.shenyu.common.exception.ShenyuException;

/**
 * Test cases for JarDependencyUtils.
 */
public class JarDependencyUtilsTest {

    @Test
    public void test() {
        try (MockedConstruction<ByteArrayInputStream> byteStream = mockConstruction(ByteArrayInputStream.class);
             MockedConstruction<ZipInputStream> zipStream = mockConstruction(ZipInputStream.class, (mock, context) -> {
                 when(mock.getNextEntry()).thenReturn(new ZipEntry("abc.class")).thenReturn(null);
             });
             MockedConstruction<ClassNode> classNode = mockConstruction(ClassNode.class, (mock, context) -> {
                 Field superName = ClassNode.class.getDeclaredField("superName");
                 superName.set(mock, "superName");
                 Field interfaces = ClassNode.class.getDeclaredField("interfaces");
                 interfaces.set(mock, Lists.newArrayList("interface"));
                 FieldNode fieldNode = mock(FieldNode.class);
                 Field fieldDesc = FieldNode.class.getDeclaredField("desc");
                 fieldDesc.set(fieldNode, "desc");
                 Field fields = ClassNode.class.getDeclaredField("fields");
                 fields.set(mock, Lists.newArrayList(fieldNode));
                 MethodNode methodNode = mock(MethodNode.class);
                 Field methodDesc = MethodNode.class.getDeclaredField("desc");
                 methodDesc.set(methodNode, "desc");
                 Field methods = ClassNode.class.getDeclaredField("methods");
                 methods.set(mock, Lists.newArrayList(methodNode));
             });
             MockedConstruction<ClassReader> classReader = mockConstruction(ClassReader.class);
             MockedStatic type = mockStatic(Type.class)) {
            when(Type.getType(anyString())).thenReturn(Type.BOOLEAN_TYPE);
            when(Type.getReturnType(anyString())).thenReturn(Type.CHAR_TYPE);
            when(Type.getArgumentTypes(anyString())).thenReturn(new Type[] {Type.INT_TYPE});
            assertNotNull(JarDependencyUtils.getDependencyTree(null));
        }
    }

    @Test
    public void testException() {
        assertThrowsExactly(ShenyuException.class, () -> JarDependencyUtils.getDependencyTree(null));
    }
}
