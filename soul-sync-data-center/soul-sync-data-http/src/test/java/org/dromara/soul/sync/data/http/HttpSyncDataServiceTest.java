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

package org.dromara.soul.sync.data.http;

import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.http.config.HttpConfig;
import org.dromara.soul.sync.data.http.support.MockHttpDataSyncEndpoint;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;


/**
 * Test cases for {@link HttpSyncDataService}.
 *
 * @author davidliu
 */
public class HttpSyncDataServiceTest {
    
    // mock HttpDataSyncEndpoint at localhost 8080
    private static final MockHttpDataSyncEndpoint SERVER = new MockHttpDataSyncEndpoint(8080);
    
    @BeforeClass
    public static void beforeCase() throws Exception {
        SERVER.start();
    }
    
    @AfterClass
    public static void afterCase() throws Exception {
        SERVER.stop();
    }
    
    /**
     * this method covers {@link HttpSyncDataService} constructor and {@link HttpSyncDataService#close()} method.
     *
     * @throws Exception any exception
     */
    @Test
    public void test() throws Exception {
        try (HttpSyncDataService ignored = this.buildHttpSyncDataService()) {
            // sleep 5 seconds to ensure Http Long polling task run
            TimeUnit.SECONDS.sleep(5);
        }
        
    }
    
    private HttpSyncDataService buildHttpSyncDataService() {
        HttpConfig httpConfig = new HttpConfig();
        httpConfig.setUrl("http://localhost:8080");
        // set http connection timeout
        httpConfig.setConnectionTimeout(3);
        // set delay time
        httpConfig.setDelayTime(3);
        PluginDataSubscriber pluginDataSubscriber = new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
            
            }
        };
        List<MetaDataSubscriber> metaDataSubscribers = Collections.singletonList(new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
            
            }
            
            @Override
            public void unSubscribe(final MetaData metaData) {
            
            }
        });
        List<AuthDataSubscriber> authDataSubscribers = Collections.singletonList(new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
            
            }
            
            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
            
            }
        });
        return new HttpSyncDataService(httpConfig, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
    }
}
