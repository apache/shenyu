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

package org.apache.shenyu.admin.mode.standalone;

import org.apache.shenyu.admin.mode.ShenyuRunningModeService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;

public class ShenyuStandaloneService implements ShenyuRunningModeService {
    
    private final UpstreamCheckService upstreamCheckService;
    
    private final LoadServiceDocEntry loadServiceDocEntry;
    
    public ShenyuStandaloneService(final UpstreamCheckService upstreamCheckService,
                                   final LoadServiceDocEntry loadServiceDocEntry) {
        this.upstreamCheckService = upstreamCheckService;
        this.loadServiceDocEntry = loadServiceDocEntry;
    }
    
    @Override
    public void start(final String host, final int port, final String contextPath) {
        // start upstream check task
        upstreamCheckService.setup();
        // load api
        loadServiceDocEntry.loadApiDocument();
    }
    
    @Override
    public void shutdown() {
        upstreamCheckService.close();
    }
}
