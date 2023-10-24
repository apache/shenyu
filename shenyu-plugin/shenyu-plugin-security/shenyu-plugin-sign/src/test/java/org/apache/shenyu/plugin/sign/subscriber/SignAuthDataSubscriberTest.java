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

package org.apache.shenyu.plugin.sign.subscriber;

import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * The Shenyu default sign subscriber test.
 */
@ExtendWith(MockitoExtension.class)
public class SignAuthDataSubscriberTest {

    private SignAuthDataSubscriber signAuthDataSubscriber;

    @BeforeEach
    public void setUp() {
        signAuthDataSubscriber = new SignAuthDataSubscriber();
    }

    @Test
    void onSubscribe() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604222A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        signAuthDataSubscriber.onSubscribe(appAuthData);
        assertEquals(SignAuthDataCache.getInstance().obtainAuthData("D9FD95F496C9495DB5604222A13C3D08"), appAuthData);
    }

    @Test
    void unSubscribe() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604222A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        signAuthDataSubscriber.onSubscribe(appAuthData);
        signAuthDataSubscriber.unSubscribe(appAuthData);
        assertNull(SignAuthDataCache.getInstance().obtainAuthData("D9FD95F496C9495DB5604222A13C3D08"));
    }
}
