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

package org.apache.shenyu.k8s.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.exception.ShenyuException;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The cache for tls config.
 */
public final class IngressSecretCache {

    private static final IngressSecretCache INSTANCE = new IngressSecretCache();

    private static final Map<String, Set<String>> INGRESS_DOMAIN_MAP = Maps.newConcurrentMap();

    private static final Map<String, AtomicInteger> DOMAIN_NUMS_MAP = Maps.newConcurrentMap();

    private IngressSecretCache() {
    }

    /**
     * Get singleton of IngressSecretCache.
     *
     * @return IngressSecretCache
     */
    public static IngressSecretCache getInstance() {
        return INSTANCE;
    }

    /**
     * Put tls domain by ingress.
     *
     * @param namespace namespace
     * @param ingressName name
     * @param domain tls domain
     */
    public void putDomainByIngress(final String namespace, final String ingressName, final String domain) {
        Set<String> set = INGRESS_DOMAIN_MAP.computeIfAbsent(getKey(namespace, ingressName), k -> new HashSet<>());
        set.add(domain);
    }

    /**
     * Put tls domain set by ingress.
     *
     * @param namespace namespace
     * @param ingressName name
     * @param domainSet domain set
     */
    public void putDomainByIngress(final String namespace, final String ingressName, final Set<String> domainSet) {
        INGRESS_DOMAIN_MAP.put(getKey(namespace, ingressName), domainSet);
    }

    /**
     * Get domain set by ingress.
     *
     * @param namespace namespace
     * @param ingressName ingressName
     * @return domain set
     */
    public Set<String> getDomainByIngress(final String namespace, final String ingressName) {
        return INGRESS_DOMAIN_MAP.get(getKey(namespace, ingressName));
    }

    /**
     * Remove domain set by ingress.
     *
     * @param namespace namespace
     * @param ingressName ingressName
     * @return domain set
     */
    public Set<String> removeDomainByIngress(final String namespace, final String ingressName) {
        return INGRESS_DOMAIN_MAP.remove(getKey(namespace, ingressName));
    }

    /**
     * Get and increment the number of the ingress that enables the domain name to take effect.
     *
     * @param domain tls domain
     * @return the previous number of the ingress that enables the domain name to take effect
     */
    public Integer getAndIncrementDomainNums(final String domain) {
        AtomicInteger count = DOMAIN_NUMS_MAP.computeIfAbsent(domain, k -> new AtomicInteger(0));
        return count.getAndIncrement();
    }

    /**
     * Get and decrement the number of the ingress that enables the domain name to take effect.
     *
     * @param domain tls domain
     * @return the previous number of the ingress that enables the domain name to take effect
     */
    public Integer getAndDecrementDomainNums(final String domain) {
        AtomicInteger count = DOMAIN_NUMS_MAP.computeIfAbsent(domain, k -> new AtomicInteger(0));
        if (count.intValue() > 0) {
            return count.getAndDecrement();
        }
        throw new ShenyuException("Decrement when domain ssl counts <= 0, an unknown exception has occurred.");
    }

    private String getKey(final String namespace, final String name) {
        return namespace + "-" + name;
    }
}
