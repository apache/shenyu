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

package org.apache.shenyu.examples.plugin.ext;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * The type My custom shenyu plugin.
 */
@Service
public class ExtendsShenyuPlugin extends AbstractShenyuPlugin {
    
    @Autowired
    private ExtendsShenyuZerBean zerBean;
    
    @Autowired
    private DispatcherHandler dispatcherHandler;
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        return chain.execute(exchange);
    }

    /**
     * get the zer bean from custom plugin.
     * @return  ExtendsShenyuZerBean
     */
    public ExtendsShenyuZerBean getZerBean() {
        return zerBean;
    }

    /**
     * get the dispatcher handler.
     * @return  dispatcherHandler
     */
    public DispatcherHandler getDispatcherHandler() {
        return dispatcherHandler;
    }
    
    @Override
    public int getOrder() {
        return 0;
    }
    
    @Override
    public String named() {
        return "ext";
    }
}
