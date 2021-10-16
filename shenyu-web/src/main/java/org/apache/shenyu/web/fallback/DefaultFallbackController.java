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

package org.apache.shenyu.web.fallback;

import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The default fallback for hystrix plugin and resilience4j plugin.
 */
@RestController
@RequestMapping("/fallback")
public class DefaultFallbackController {
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(DefaultFallbackController.class);

    /**
     * default fallback for hystrix.
     *
     * @return the shenyu result
     */
    @GetMapping("/hystrix")
    public Object hystrixPluginFallback() {
        LOG.error("the default fallback for hystrix");
        return ShenyuResultWrap.error(ShenyuResultEnum.HYSTRIX_PLUGIN_FALLBACK.getCode(), ShenyuResultEnum.HYSTRIX_PLUGIN_FALLBACK.getMsg(), null);
    }

    /**
     * default fallback for resilience4j.
     *
     * @return the shenyu result
     */
    @GetMapping("/resilience4j")
    public Object resilience4jFallBack() {
        LOG.error("the default fallback for resilience4j");
        return ShenyuResultWrap.error(ShenyuResultEnum.RESILIENCE4J_PLUGIN_FALLBACK.getCode(), ShenyuResultEnum.RESILIENCE4J_PLUGIN_FALLBACK.getMsg(), null);
    }
}
