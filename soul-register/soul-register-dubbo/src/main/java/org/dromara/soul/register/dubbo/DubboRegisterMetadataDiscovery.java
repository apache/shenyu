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

import com.alibaba.dubbo.common.URL;
import java.util.*;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.register.api.PathMethod;
import org.dromara.soul.register.api.RegisterDiscovery;
import org.dromara.soul.register.api.RegisterDiscoveryListener;
import org.dromara.soul.register.api.path.Path;

/**
 * DubboRegisterMetadataDirectory
 * About dubbo2.7.Version of the Metadata listening implementation.
 *
 * @author sixh
 */
public class DubboRegisterMetadataDiscovery extends RegisterDiscovery {

    private DubboMetadataService service;

    private final ExtensionLoader<DubboMetadataService> loader = ExtensionLoader.getExtensionLoader(DubboMetadataService.class);

    /**
     * Instantiates a new Dubbo register metadata directory.
     *
     * @param url       metadata address url.
     * @param listeners the listeners.
     */
    public DubboRegisterMetadataDiscovery(URL url, Set<RegisterDiscoveryListener> listeners) {
        super(UrlAdapter.parse(url), listeners);
        service = loader.getJoin(getUrl().getProtocol());
    }

    /**
     * Instantiates a new Dubbo register metadata directory.
     *
     * @param url      metadata address url.
     * @param listener the listener.
     */
    public DubboRegisterMetadataDiscovery(URL url, RegisterDiscoveryListener listener) {
        super(UrlAdapter.parse(url), listener);
        service = loader.getJoin(getUrl().getProtocol());
    }

    public void bindMetadata(Set<Path> paths) {
        if (!paths.isEmpty()) {
            List<Path> collect = paths.stream().filter(e -> e.status().equals(RegisterDiscoveryListener.ADD)).collect(Collectors.toList());
            Map<String, Map<String, List<PathMethod>>> temporary = new HashMap<>(16);
            for (Path path : collect) {
                if (path instanceof DubboPath) {
                    DubboPath dubboPath = (DubboPath) path;
                    Map<String, List<PathMethod>> map = temporary.get(dubboPath.getServiceKey());
                    if (map == null || map.isEmpty()) {
                        String json = service.getMetadata(dubboPath);
                        map = Optional.ofNullable(json)
                                .filter(toJson -> StringUtils.isNotBlank(json))
                                .map(toJson -> JsonUtils.toMap(json))
                                .map(this::resolveMetadata)
                                .map(Optional::get)
                                .orElse(Collections.emptyMap());
                        temporary.put(dubboPath.getServiceKey(), map);
                    }
                    List<PathMethod> pathMethods = map.get(dubboPath.getMethod());
                    dubboPath.setPathMethods(pathMethods);
                }
            }
            redress(paths);
        }
    }

    @SuppressWarnings("all")
    private Optional<Map<String, List<PathMethod>>> resolveMetadata(Map maps) {
        return Optional.ofNullable(maps).filter(map -> !map.isEmpty()).map(map -> {
            Object methods = map.get("methods");
            if (methods instanceof List) {
                List<Map> methodList = (List<Map>) methods;
                Map<String, List<PathMethod>> collect = methodList.stream().map(methodValue -> {
                    Map<String, Object> methodMap = (Map<String, Object>) methodValue;
                    String name = Optional.ofNullable(methodMap.get("name")).map(String::valueOf).orElse("");
                    String returnType = Optional.ofNullable(methodMap.get("returnType")).map(String::valueOf).orElse(Object.class.getName());
                    List<String> parameterTypes = Optional.ofNullable(methodMap.get("parameterTypes"))
                            .map(parameters -> (List<String>) parameters)
                            .orElse(Collections.emptyList());
                    PathMethod pathMethod = new PathMethod();
                    pathMethod.setMethodName(name);
                    pathMethod.setReturnType(returnType);
                    pathMethod.setParameterTypes(parameterTypes);
                    return pathMethod;
                }).filter(e -> StringUtils.isNotBlank(e.getMethodName()))
                        .collect(Collectors.groupingBy(e -> e.getMethodName()));
                return collect;
            }
            return Collections.emptyMap();
        });
    }

    @Override
    public Future<Boolean> isHealthy(Path path) {
        return null;
    }
}
