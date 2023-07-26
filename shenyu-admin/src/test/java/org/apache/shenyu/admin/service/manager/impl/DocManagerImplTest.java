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

import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath("/testClusterName");
        AtomicBoolean atomicBoolean = new AtomicBoolean(false);
        docManager.addDocInfo(instance, SwaggerDocParserTest.DOC_INFO_JSON, "", docInfo -> {
            assertEquals(docInfo.getTitle(), "shenyu-examples-http-swagger2 API");
            assertEquals(docInfo.getClusterName(), "testClusterName");
            atomicBoolean.set(true);
        });

        assertTrue(atomicBoolean.get());
    }
}
