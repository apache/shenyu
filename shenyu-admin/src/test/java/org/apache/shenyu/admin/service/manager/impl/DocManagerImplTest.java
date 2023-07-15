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

package org.apache.shenyu.admin.service.manager.impl;

import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.util.DigestUtils;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertFalse;

/**
 * test cases for DocManagerImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DocManagerImplTest {
    @InjectMocks
    private DocManagerImpl docManager;

    @Mock
    private RegisterApiDocService registerApiDocService;

    @Test
    public void testAddDocInfo() {
        String clusterName = "testClusterName";
        AtomicBoolean atomicBoolean = new AtomicBoolean(false);
        docManager.addDocInfo(clusterName, SwaggerDocParserTest.DOC_INFO_JSON, docInfo -> {
            assertEquals(docInfo.getTitle(), "shenyu-examples-http-swagger2 API");
            assertEquals(docInfo.getClusterName(), "testClusterName");
            atomicBoolean.set(true);
        });

        assertTrue(atomicBoolean.get());
    }

    @Test
    public void testGetDocItem() {
        Class<DocManagerImpl> docManageClass = DocManagerImpl.class;
        Map<String, DocItem> itemDocMap;
        try {
            Field field = docManageClass.getDeclaredField("ITEM_DOC_MAP");
            field.setAccessible(true);
            itemDocMap = (Map<String, DocItem>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        String id = "1";
        DocItem expectedDocItem = new DocItem();
        itemDocMap.put(id, expectedDocItem);
        assertEquals(expectedDocItem, docManager.getDocItem(id));
    }

    @Test
    public void testListAll() {
        Class<DocManagerImpl> docManageClass = DocManagerImpl.class;
        Map<String, DocInfo> docDefinitionMap;
        try {
            Field field = docManageClass.getDeclaredField("DOC_DEFINITION_MAP");
            field.setAccessible(true);
            docDefinitionMap = (Map<String, DocInfo>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        String title1 = "testTitle1";
        String title2 = "testTitle2";
        DocInfo expectedDocInfo1 = new DocInfo();
        DocInfo expectedDocInfo2 = new DocInfo();
        expectedDocInfo1.setTitle(title1);
        expectedDocInfo2.setTitle(title2);
        docDefinitionMap.put(title1, expectedDocInfo1);
        docDefinitionMap.put(title2, expectedDocInfo2);
        Collection<DocInfo> docInfos = docManager.listAll();
        assertFalse(docInfos.isEmpty());
    }

    @Test
    public void testGetDocMd5() {
        Class<DocManagerImpl> docManageClass = DocManagerImpl.class;
        Map<String, String> clusterMd5Map;
        try {
            Field field = docManageClass.getDeclaredField("CLUSTER_MD5_MAP");
            field.setAccessible(true);
            clusterMd5Map = (Map<String, String>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        String title = "testTitle";
        clusterMd5Map.put(title, DigestUtils.md5DigestAsHex(title.getBytes()));
        assertEquals(DigestUtils.md5DigestAsHex(title.getBytes()), docManager.getDocMd5(title));
    }

    @Test
    public void testRemove() {
        Class<DocManagerImpl> docManagerClass = DocManagerImpl.class;
        Map<String, String> clusterMd5Map;
        try {
            Field field = docManagerClass.getDeclaredField("CLUSTER_MD5_MAP");
            field.setAccessible(true);
            clusterMd5Map = (Map<String, String>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        String title = "testTitle";
        clusterMd5Map.put(title, DigestUtils.md5DigestAsHex(title.getBytes()));
        assertEquals(DigestUtils.md5DigestAsHex(title.getBytes()), docManager.getDocMd5(title));
        docManager.remove(title);
        assertNull(docManager.getDocMd5(title));
    }
}
