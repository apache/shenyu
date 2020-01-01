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

import com.alibaba.dubbo.common.Constants;
import com.alibaba.dubbo.common.URL;
import com.alibaba.dubbo.common.utils.NetUtils;
import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.registry.NotifyListener;
import com.alibaba.dubbo.registry.RegistryService;
import com.google.common.base.Splitter;
import com.google.common.collect.Sets;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.register.api.RegisterDiscovery;
import org.dromara.soul.register.api.RegisterDiscoveryListener;
import org.dromara.soul.register.api.path.Path;

import static java.util.stream.Collectors.toSet;

/**
 * DubboRegisterDirectory
 *
 * @author sixh
 */

public class DubboRegisterDiscovery extends RegisterDiscovery implements NotifyListener, RegisterDiscoveryListener {

    private static final ConcurrentHashMap<String, Map<String, Set<DubboPath>>> CACHE = new ConcurrentHashMap<>();

    private static final URL SUBSCRIBE = new URL(Constants.ADMIN_PROTOCOL, NetUtils.getLocalHost(), 0, "",
            Constants.INTERFACE_KEY, Constants.ANY_VALUE,
            Constants.GROUP_KEY, Constants.ANY_VALUE,
            Constants.VERSION_KEY, Constants.ANY_VALUE,
            Constants.CLASSIFIER_KEY, Constants.ANY_VALUE,
            Constants.CATEGORY_KEY, Constants.PROVIDERS_CATEGORY,
            Constants.ENABLED_KEY, Constants.ANY_VALUE,
            Constants.CHECK_KEY, String.valueOf(false));

    private DubboRegisterMetadataDiscovery metadataDirectory;

    /**
     * Gets metadata directory.
     *
     * @return the metadata directory.
     */
    public RegisterDiscovery getMetadataDirectory() {
        return metadataDirectory;
    }

    /**
     * Instantiates a new Dubbo register directory.
     *
     * @param registerUrl the register url
     * @param metadataUrl the metadata url
     * @param listeners   the listeners.
     */
    DubboRegisterDiscovery(URL registerUrl, URL metadataUrl, Set<RegisterDiscoveryListener> listeners) {
        super(UrlAdapter.parse(registerUrl), listeners);
        metadataDirectory = new DubboRegisterMetadataDiscovery(metadataUrl, this);
        DubboPath dubboPath = DubboPath.builder()
                .registerServer(true)
                .env(getEnv())
                .registry(this.getUrl())
                .service("com.alibaba.dubbo.registry.RegistryService")
                .build();
        ReferenceConfig<RegistryService> registryServiceRef = DubboReferenceCache.INSTANCE.get(dubboPath);
        if (registryServiceRef == null) {
            throw new SoulException("init dubbo RegistryService error");
        }
        RegistryService registryService = registryServiceRef.get();
        registryService.subscribe(SUBSCRIBE, this);
    }

    /**
     * Instantiates a new Dubbo register directory.
     *
     * @param registerUrl the register url
     * @param metadataUrl the metadata url
     * @param listener    the listener.
     */
    public DubboRegisterDiscovery(URL registerUrl, URL metadataUrl, RegisterDiscoveryListener listener) {
        this(registerUrl, metadataUrl, Sets.newHashSet(listener));
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
        List<URL> providres = new ArrayList<>();
        /*
         * //不同服务提供
         */
        ConcurrentHashMap<String, Set<DubboPath>> categories = new ConcurrentHashMap<>();
        String serviceName = "";
        for (URL url : urls) {
            //得到dubbo的version
            String group = url.getParameter(Constants.GROUP_KEY);
            String version = url.getParameter(Constants.VERSION_KEY);
            String category = url.getParameter(Constants.CATEGORY_KEY, Constants.DEFAULT_CATEGORY);
            serviceName = url.getServiceInterface();
            if (Constants.PROVIDERS_CATEGORY.equals(category)) {
                providres.add(url);
            }
        }
        toProviders(providres);
    }

    private void toProviders(List<URL> providers) {
        if (providers == null) {
            return;
        }
        ConcurrentHashMap<String, Map<String, Set<DubboPath>>> categories = new ConcurrentHashMap<>(16);
        for (URL provider : providers) {
            String category = provider.getParameter(Constants.CATEGORY_KEY, Constants.PROVIDERS_CATEGORY);
            String group = provider.getParameter(Constants.GROUP_KEY);
            String version = provider.getParameter(Constants.VERSION_KEY);
            String serviceName = provider.getServiceInterface();
            if (Constants.EMPTY_PROTOCOL.equalsIgnoreCase(provider.getProtocol())
                    && category.equals(Constants.PROVIDERS_CATEGORY)
                    && Constants.ANY_VALUE.equals(group)
                    && Constants.ANY_VALUE.equals(version)) {
                destroyAll(provider);
            } else if (category.equals(Constants.PROVIDERS_CATEGORY)) {
                Map<String, Set<DubboPath>> maps = categories.get(serviceName);
                if (maps == null) {
                    maps = new HashMap<>(16);
                }
                toProvider(provider, maps);
                categories.put(serviceName, maps);
            }
        }
        if (!categories.isEmpty()) {
            categories.forEach((serviceName, newMaps) -> {
                if (newMaps != null && !newMaps.isEmpty()) {
                    Map<String, Set<DubboPath>> oldMaps = CACHE.get(serviceName);
                    refreshProviders(oldMaps, newMaps);
                    CACHE.put(serviceName, newMaps);
                }
            });
        }
    }

    private void refreshProviders(Map<String, Set<DubboPath>> oldMaps,
                                  Map<String, Set<DubboPath>> newMaps) {
        Set<Path> actions = new HashSet<>();
        if (oldMaps == null || oldMaps.isEmpty()) {
            Set<DubboPath> collect = newMaps.values().stream()
                    .flatMap(Collection::stream)
                    .peek(e -> e.setStatus(RegisterDiscoveryListener.ADD))
                    .collect(toSet());
            actions.addAll(collect);
        } else {
            oldMaps.forEach((k, value) -> {
                Set<DubboPath> collect;
                if (newMaps.containsKey(k)) {
                    Sets.SetView<DubboPath> differenceSets = Sets.difference(value, newMaps.get(k));
                    collect = differenceSets.stream().peek(e -> e.setStatus(RegisterDiscoveryListener.REMOVE)).collect(Collectors.toSet());
                } else {
                    collect = value.stream().peek(e -> e.setStatus(RegisterDiscoveryListener.REMOVE)).collect(toSet());
                }
                actions.addAll(collect);
            });
            newMaps.forEach((k, value) -> {
                Set<DubboPath> collect;
                if (oldMaps.containsKey(k)) {
                    Sets.SetView<DubboPath> differenceSets = Sets.difference(value, oldMaps.get(k));
                    collect = differenceSets.stream().peek(e -> e.setStatus(RegisterDiscoveryListener.ADD)).collect(Collectors.toSet());
                } else {
                    collect = value.stream().peek(e -> e.setStatus(RegisterDiscoveryListener.ADD)).collect(toSet());
                }
                actions.addAll(collect);
            });
        }
        redress(actions);
    }

    @SuppressWarnings("all")
    private void toProvider(URL provider, Map<String, Set<DubboPath>> categories) {
        String methodsKey = provider.getParameter(Constants.METHODS_KEY);
        String application = provider.getParameter(Constants.APPLICATION_KEY);
        String serviceKey = provider.getServiceKey();
        String serviceName = provider.getServiceInterface();
        String version = provider.getParameter(Constants.VERSION_KEY);
        String group = provider.getParameter(Constants.GROUP_KEY);
        List<String> methods = Splitter.on(",").splitToList(methodsKey);
        Set<DubboPath> set = new HashSet<>(methods.size());
        org.dromara.soul.common.http.URL soulUrl = UrlAdapter.parse(provider);
        methods.forEach(method -> {
            //Represents the available methods;
            DubboPath config = DubboPath.builder()
                    .nativeUrl(soulUrl)
                    .application(application)
                    .service(serviceName)
                    .version(version)
                    .method(method)
                    .group(group)
                    .registry(this.getUrl())
                    .env(this.getEnv())
                    .serviceKey(serviceKey)
                    .registerServer(false).build();
            set.add(config);
        });
        if (!set.isEmpty()) {
            if (categories.containsKey(serviceKey)) {
                Set<DubboPath> oldSet = categories.get(serviceKey);
                oldSet.addAll(set);
                categories.put(serviceKey, oldSet);
            } else {
                categories.put(serviceKey, set);
            }
        }
    }

    @Override
    protected void redress(Set<Path> paths) {
        metadataDirectory.bindMetadata(paths);
        super.redress(paths);
    }

    private void destroyAll(URL provider) {
        String serviceInterface = provider.getServiceInterface();
        //清除本地缓存
        Map<String, Set<DubboPath>> map = CACHE.remove(serviceInterface);
        Set<DubboPath> actions = new HashSet<>();
        map.forEach((k, v) -> actions.addAll(v));
        Set<Path> paths = actions.stream().peek(e -> e.setStatus(RegisterDiscoveryListener.REMOVE)).collect(toSet());
        redress(paths);
    }

    @Override
    public void apply(Set<Path> paths) {

    }
}
