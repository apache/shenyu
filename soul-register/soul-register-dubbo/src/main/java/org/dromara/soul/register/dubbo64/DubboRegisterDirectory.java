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

package org.dromara.soul.register.dubbo64;

import com.alibaba.dubbo.common.Constants;
import com.alibaba.dubbo.common.URL;
import com.alibaba.dubbo.common.utils.NetUtils;
import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.registry.NotifyListener;
import com.alibaba.dubbo.registry.RegistryService;
import com.google.common.base.Splitter;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.SetMultimap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.extension.Join;
import org.dromara.soul.register.api.path.EmptyPath;
import org.dromara.soul.register.api.path.Path;
import org.dromara.soul.register.api.RegisterDirectory;
import org.dromara.soul.register.api.RegisterDirectoryListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * DubboRegisterDirectory
 *
 * @author sixh
 */
@Join
public class DubboRegisterDirectory extends RegisterDirectory implements NotifyListener {

    private static final Logger LOG = LoggerFactory.getLogger(DubboRegisterDirectory.class);

    private static final ConcurrentHashMap<String, SetMultimap<String, DubboPath>> CACHE = new ConcurrentHashMap<>();

    private static final URL SUBSCRIBE = new URL(Constants.ADMIN_PROTOCOL, NetUtils.getLocalHost(), 0, "",
                                                 Constants.INTERFACE_KEY, Constants.ANY_VALUE,
                                                 Constants.GROUP_KEY, Constants.ANY_VALUE,
                                                 Constants.VERSION_KEY, Constants.ANY_VALUE,
                                                 Constants.CLASSIFIER_KEY, Constants.ANY_VALUE,
                                                 Constants.CATEGORY_KEY, Constants.PROVIDERS_CATEGORY,
                                                 Constants.ENABLED_KEY, Constants.ANY_VALUE,
                                                 Constants.CHECK_KEY, String.valueOf(false));
    /**
     * Dubbo's registration management.
     */
    private RegistryService registryService;

    /**
     * Instantiates a new Dubbo register directory.
     */
    public DubboRegisterDirectory() {
        DubboPath dubboPath = DubboPath.builder()
                .registerServer(true)
                .service("com.alibaba.dubbo.registry.RegistryService")
                .build();
        ReferenceConfig<RegistryService> registryServiceRef = DubboReferenceCache.INSTANCE.get(dubboPath);
        if (registryServiceRef == null) {
            throw new SoulException("init dubbo RegistryService error");
        }
        this.registryService = registryServiceRef.get();
        this.registryService.subscribe(SUBSCRIBE, this);
    }

    @Override
    public Future<Boolean> isHealthy(Path path) {
        return null;
    }

    @Override
    @SuppressWarnings("all")
    public void notify(List<URL> urls) {
        if (urls == null || urls.isEmpty()) {
            return;
        }
        /*
         * //不同服务提供
         */
        ConcurrentHashMap<String, Set<DubboPath>> categories = new ConcurrentHashMap<>();
        String serviceName = "";
        for (URL url : urls) {
            //得到dubbo的version
            String group = url.getParameter(Constants.GROUP_KEY);
            String version = url.getParameter(Constants.VERSION_KEY);
            String category = url.getParameter(Constants.CATEGORY_KEY, Constants.PROVIDERS_CATEGORY);
            serviceName = url.getServiceInterface();
            //如果是dubbo协议，再确定
            if (Constants.DEFAULT_PROTOCOL.equalsIgnoreCase(url.getProtocol()) && category.equals(Constants.PROVIDERS_CATEGORY)) {
                String methodsKey = url.getParameter(Constants.METHODS_KEY);
                String application = url.getParameter(Constants.APPLICATION_KEY);
                List<String> methods = Splitter.on(",").splitToList(methodsKey);
                String serviceKey = url.getServiceKey();
                Set<DubboPath> set = new HashSet<>(methods.size());
                org.dromara.soul.common.http.URL soulUrl = UrlAdapter.parse(url);
                for (String methodName : methods) {
                    //表示可用的方法;
                    DubboPath config = DubboPath.builder()
                            .nativeUrl(soulUrl)
                            .application(application)
                            .service(serviceName)
                            .version(version)
                            .method(methodName)
                            .group(group)
                            .serviceKey(serviceKey)
                            .registerServer(false).build();
                    set.add(config);
                }
                if (!set.isEmpty()) {
                    if (categories.containsKey(serviceKey)) {
                        Set<DubboPath> oldSet = categories.get(serviceKey);
                        oldSet.addAll(set);
                        categories.put(serviceKey, oldSet);
                    } else {
                        categories.put(serviceKey, set);
                    }
                }
                //如果为空的协议者清除缓存存在
            } else if (Constants.EMPTY_PROTOCOL.equalsIgnoreCase(url.getProtocol()) && category.equals(Constants.PROVIDERS_CATEGORY)) {
                //group == * 或者 version==*  表示下线所有服务
                if (Constants.ANY_VALUE.equals(group)
                    && Constants.ANY_VALUE.equals(version)) {
                    offlineOff(serviceName);
                }
            }
        }
        if (!categories.isEmpty()) {
            //通知服务上线或已经存在的服务进行下线
            SetMultimap<String, DubboPath> multiCache = CACHE.get(serviceName);
            SetMultimap<String, DubboPath> newMultiCache = HashMultimap.create();
            String finalServiceName = serviceName;
            categories.forEach((k, v) -> {
                Set<DubboPath> sets = new HashSet<>();
                if (multiCache != null) {
                    sets = multiCache.get(k);
                    //移除已存在的服务
                    multiCache.removeAll(k);
                }
                HashSet<DubboPath> timeService = new HashSet<>(v);
                newMultiCache.putAll(k, v);
                CACHE.put(finalServiceName, newMultiCache);
                if (sets.size() > v.size()) {
                    //得到差集，需要下线的服务
                    sets.removeAll(v);
                    sets.forEach(e -> {
                        e.setStatus(RegisterDirectoryListener.REMOVE);
                        redress(e);
                    });
                } else if (sets.size() < v.size()) {
                    //需要自动上线的服务
                    //得到差集，需要上线的服务
                    timeService.removeAll(sets);
                    timeService.forEach(e -> {
                        e.setStatus(RegisterDirectoryListener.ADD);
                        redress(e);
                    });
                }
            });
            //下线已经不存在dubbo 提供者的服务
            if (multiCache != null && !multiCache.isEmpty()) {
                multiCache.keySet().forEach(key -> multiCache.get(key).forEach(e -> {
                    e.setStatus(RegisterDirectoryListener.REMOVE);
                    redress(e);
                }));
            }
        }
    }

    private void offlineOff(String serviceName) {
        //清除本地缓存
        SetMultimap<String, DubboPath> caches = CACHE.get(serviceName);
        EmptyPath path = new EmptyPath();
        path.setKey(serviceName);
        redress(path);
        CACHE.remove(serviceName);
    }
}
