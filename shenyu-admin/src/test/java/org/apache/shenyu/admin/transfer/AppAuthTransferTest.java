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

package org.apache.shenyu.admin.transfer;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.vo.AppAuthListVO;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test for {@link AppAuthTransfer}.
 */
public final class AppAuthTransferTest {

    private static final String SYNTHETIC_APP_SECRET = "synthetic-app-secret-8181";

    private static final String APP_SECRET_FIELD = "\"appSecret\"";

    @Test
    public void mapToListVoShouldNotExposeAppSecretInSerializedJson() throws Exception {
        AppAuthDO appAuthDO = AppAuthDO.builder()
                .id("app-auth-id")
                .appKey("app-key")
                .appSecret(SYNTHETIC_APP_SECRET)
                .userId("user-id")
                .phone("18600000000")
                .extInfo("{\"extInfo\":\"test\"}")
                .open(true)
                .enabled(true)
                .namespaceId("default")
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build();

        AppAuthListVO appAuthListVO = AppAuthTransfer.INSTANCE.mapToListVO(appAuthDO);
        assertNotNull(appAuthListVO);
        assertEquals(appAuthDO.getAppKey(), appAuthListVO.getAppKey());

        String json = new ObjectMapper().writeValueAsString(appAuthListVO);
        assertFalse(json.contains(APP_SECRET_FIELD));
        assertFalse(json.contains(SYNTHETIC_APP_SECRET));
    }

    @Test
    public void mapToListVoShouldReturnNullForNullEntity() {
        assertNull(AppAuthTransfer.INSTANCE.mapToListVO(null));
    }
}
