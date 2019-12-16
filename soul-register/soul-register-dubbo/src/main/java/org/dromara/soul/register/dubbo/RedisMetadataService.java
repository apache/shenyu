/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.dubbo;

import com.google.common.base.Joiner;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.extension.Join;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.remoting.redis.RedisClient;
import org.dromara.soul.remoting.redis.operation.ValueOperation;

/**
 * RedisMetadataService
 *
 * @author sixh
 */
@Join
public class RedisMetadataService implements DubboMetadataService {

    RedisClient<String, String> redisClient;

    public RedisMetadataService(URL url) {
        this.redisClient = new RedisClient<>(url);
    }

    @Override
    public String getMetadata(DubboPath path) {
        ValueOperation<String, String> valueOperation = redisClient.valueOperation();
        return valueOperation.get(toKey(path), String.class);
    }

    public String toKey(DubboPath path) {
        String pathStr;
        if (StringUtils.isNotBlank(path.getGroup())) {
            pathStr = Joiner.on(Constants.COLONS).join("dubbo", "metadata", path.getService(), path.getVersion(), path.getGroup(), "provider", path.getApplication());
        } else {
            pathStr = Joiner.on(Constants.COLONS).join("dubbo", "metadata", path.getService(), path.getVersion(), "provider", path.getApplication());
        }
        return Constants.BASE_URL_PATH_KEY + pathStr;
    }
}
