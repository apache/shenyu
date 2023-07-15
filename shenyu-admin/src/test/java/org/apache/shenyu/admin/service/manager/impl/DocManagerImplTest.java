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
        String docInfoJson = "{\n" +
            "    \"swagger\":\"2.0\",\n" +
            "    \"info\":{\n" +
            "        \"description\":\"shenyu-examples-http-swagger2 API\",\n" +
            "        \"version\":\"2.3.0\",\n" +
            "        \"title\":\"shenyu-examples-http-swagger2 API\",\n" +
            "        \"contact\":{\n" +
            "            \"name\":\"ShenYu\",\n" +
            "            \"url\":\"https://github.com/apache/shenyu\",\n" +
            "            \"email\":\"dev@shenyu.apache.org\"\n" +
            "        }\n" +
            "    },\n" +
            "    \"host\":\"127.0.0.1:8190\",\n" +
            "    \"basePath\":\"/\",\n" +
            "    \"tags\":[\n" +
            "        {\n" +
            "            \"name\":\"Order API\",\n" +
            "            \"description\":\"Order Controller\"\n" +
            "        },\n" +
            "        {\n" +
            "            \"name\":\"http-test-controller\",\n" +
            "            \"description\":\"Http Test Controller\"\n" +
            "        },\n" +
            "    ],\n" +
            "    \"paths\":{\n" +
            "        \"/order/path/{id}/{name}\":{\n" +
            "            \"get\":{\n" +
            "                \"tags\":[\n" +
            "                    \"Order API\"\n" +
            "                ],\n" +
            "                \"summary\":\"getPathVariable\",\n" +
            "                \"description\":\"get path variable.\",\n" +
            "                \"operationId\":\"getPathVariableUsingGET_1\",\n" +
            "                \"produces\":[\n" +
            "                    \"*/*\"\n" +
            "                ],\n" +
            "                \"parameters\":[\n" +
            "                    {\n" +
            "                        \"name\":\"X-Access-Token\",\n" +
            "                        \"in\":\"header\",\n" +
            "                        \"description\":\"user auth\",\n" +
            "                        \"required\":false,\n" +
            "                        \"type\":\"string\"\n" +
            "                    },\n" +
            "                    {\n" +
            "                        \"name\":\"id\",\n" +
            "                        \"in\":\"path\",\n" +
            "                        \"description\":\"id\",\n" +
            "                        \"required\":true,\n" +
            "                        \"type\":\"string\"\n" +
            "                    },\n" +
            "                    {\n" +
            "                        \"name\":\"name\",\n" +
            "                        \"in\":\"path\",\n" +
            "                        \"description\":\"name\",\n" +
            "                        \"required\":true,\n" +
            "                        \"type\":\"string\"\n" +
            "                    }\n" +
            "                ],\n" +
            "                \"responses\":{\n" +
            "                    \"200\":{\n" +
            "                        \"description\":\"OK\",\n" +
            "                        \"schema\":{\n" +
            "                            \"$ref\":\"#/definitions/OrderDTO\"\n" +
            "                        }\n" +
            "                    },\n" +
            "                    \"404\":{\n" +
            "                        \"description\":\"Not Found\"\n" +
            "                    }\n" +
            "                },\n" +
            "                \"deprecated\":false\n" +
            "            }\n" +
            "        },\n" +
            "        \"/test/payment\":{\n" +
            "            \"post\":{\n" +
            "                \"tags\":[\n" +
            "                    \"http-test-controller\"\n" +
            "                ],\n" +
            "                \"summary\":\"payment\",\n" +
            "                \"description\":\"The user pays the order.\",\n" +
            "                \"operationId\":\"postUsingPOST\",\n" +
            "                \"consumes\":[\n" +
            "                    \"application/json\"\n" +
            "                ],\n" +
            "                \"produces\":[\n" +
            "                    \"*/*\"\n" +
            "                ],\n" +
            "                \"parameters\":[\n" +
            "                    {\n" +
            "                        \"name\":\"X-Access-Token\",\n" +
            "                        \"in\":\"header\",\n" +
            "                        \"description\":\"user auth\",\n" +
            "                        \"required\":false,\n" +
            "                        \"type\":\"string\"\n" +
            "                    },\n" +
            "                    {\n" +
            "                        \"in\":\"body\",\n" +
            "                        \"name\":\"userDTO\",\n" +
            "                        \"description\":\"userDTO\",\n" +
            "                        \"required\":true,\n" +
            "                        \"schema\":{\n" +
            "                            \"$ref\":\"#/definitions/UserDTO\"\n" +
            "                        }\n" +
            "                    }\n" +
            "                ],\n" +
            "                \"responses\":{\n" +
            "                    \"200\":{\n" +
            "                        \"description\":\"OK\",\n" +
            "                        \"schema\":{\n" +
            "                            \"$ref\":\"#/definitions/UserDTO\"\n" +
            "                        }\n" +
            "                    },\n" +
            "                    \"401\":{\n" +
            "                        \"description\":\"Unauthorized\"\n" +
            "                    },\n" +
            "                    \"403\":{\n" +
            "                        \"description\":\"Forbidden\"\n" +
            "                    },\n" +
            "                    \"404\":{\n" +
            "                        \"description\":\"Not Found\"\n" +
            "                    }\n" +
            "                },\n" +
            "                \"deprecated\":false\n" +
            "            }\n" +
            "        }\n" +
            "    },\n" +
            "    \"definitions\":{\n" +
            "        \"Mono«string»\":{\n" +
            "            \"type\":\"object\",\n" +
            "            \"title\":\"Mono«string»\"\n" +
            "        },\n" +
            "        \"OrderDTO\":{\n" +
            "            \"type\":\"object\",\n" +
            "            \"required\":[\n" +
            "                \"id\",\n" +
            "                \"name\"\n" +
            "            ],\n" +
            "            \"properties\":{\n" +
            "                \"id\":{\n" +
            "                    \"type\":\"string\",\n" +
            "                    \"example\":100000,\n" +
            "                    \"description\":\"id\"\n" +
            "                },\n" +
            "                \"name\":{\n" +
            "                    \"type\":\"string\",\n" +
            "                    \"example\":\"jack\",\n" +
            "                    \"description\":\"name\"\n" +
            "                }\n" +
            "            },\n" +
            "            \"title\":\"OrderDTO\"\n" +
            "        },\n" +
            "        \"UserDTO\":{\n" +
            "            \"type\":\"object\",\n" +
            "            \"required\":[\n" +
            "                \"userId\"\n" +
            "            ],\n" +
            "            \"properties\":{\n" +
            "                \"userId\":{\n" +
            "                    \"type\":\"string\",\n" +
            "                    \"example\":100000,\n" +
            "                    \"description\":\"user id\"\n" +
            "                },\n" +
            "                \"userName\":{\n" +
            "                    \"type\":\"string\",\n" +
            "                    \"example\":\"shenyu\",\n" +
            "                    \"description\":\"user name\"\n" +
            "                }\n" +
            "            },\n" +
            "            \"title\":\"UserDTO\"\n" +
            "        }\n" +
            "    }\n" +
            "}";

        docManager.addDocInfo(clusterName, docInfoJson, docInfo -> {
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
