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

package org.apache.shenyu.plugin.grpc.resolver;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.beans.BeanUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Shenyu service instance list.
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class ShenyuServiceInstanceLists {

    private CopyOnWriteArrayList<ShenyuServiceInstance> shenyuServiceInstances;

    private String appName;

    /**
     * Get instance copy.
     *
     * @return list list
     */
    public List<ShenyuServiceInstance> getCopyInstances() {
        List<ShenyuServiceInstance> copy = new ArrayList<>(shenyuServiceInstances.size());
        shenyuServiceInstances.forEach(instance -> {
            ShenyuServiceInstance cp = new ShenyuServiceInstance();
            BeanUtils.copyProperties(instance, cp);
            copy.add(cp);
        });
        return copy;
    }
}
