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

package org.apache.shenyu.plugin.api.failure;

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.ApplicationContext;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/** Prepared, finite ordered policies; no-match and invalid callbacks use the legacy owner. */
public final class GatewayFailureMapping {

    public static final String FACTS_ATTRIBUTE = GatewayFailure.class.getName();

    private static final GatewayFailureMapping LEGACY = new GatewayFailureMapping(List.of());

    private final List<GatewayFailureMapper> mappers;

    public GatewayFailureMapping(final List<GatewayFailureMapper> mappers) {
        if (mappers.size() > 32) {
            throw new IllegalArgumentException("At most 32 gateway failure mappers are supported");
        }
        List<GatewayFailureMapper> ordered = new ArrayList<>(mappers);
        ordered.sort(Comparator.comparingInt(GatewayFailureMapper::getOrder));
        this.mappers = List.copyOf(ordered);
    }

    /**
     * Resolve the optional startup-prepared bean only on an adopted failure path.
     * @return prepared mapping or the built-in legacy adapter
     */
    public static GatewayFailureMapping current() {
        ApplicationContext context = SpringBeanUtils.getInstance().getApplicationContext();
        if (Objects.isNull(context)) {
            return LEGACY;
        }
        try {
            ObjectProvider<GatewayFailureMapping> provider = context.getBeanProvider(GatewayFailureMapping.class);
            return Objects.isNull(provider) ? LEGACY : provider.getIfAvailable(() -> LEGACY);
        } catch (BeansException ignored) {
            return LEGACY;
        }
    }

    /**
     * First applicable result wins; an invalid or throwing policy returns to the original owner.
     * @param facts immutable classification
     * @return custom presentation or empty for the existing legacy behavior
     */
    public Optional<GatewayFailurePresentation> select(final GatewayFailure facts) {
        if (facts.output() != GatewayFailure.Output.BEFORE_ERROR_WRITE || facts.reason() == GatewayFailure.Reason.CANCELLATION) {
            return Optional.empty();
        }
        for (GatewayFailureMapper mapper : mappers) {
            try {
                Optional<GatewayFailurePresentation> result = mapper.map(facts);
                if (Objects.isNull(result)) {
                    return Optional.empty();
                }
                if (result.isPresent()) {
                    return result;
                }
            } catch (RuntimeException ignored) {
                return Optional.empty();
            }
        }
        return Optional.empty();
    }
}
