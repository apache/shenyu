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

package org.apache.shenyu.admin.scale.scaler;

import io.kubernetes.client.openapi.apis.AppsV1Api;
import io.kubernetes.client.openapi.models.V1Scale;
import io.kubernetes.client.openapi.models.V1ScaleSpec;
import org.apache.shenyu.admin.config.properties.DeploymentProperties;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleAction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class KubernetesScalerTest {

    private static final String DEPLOY_NAME = "shenyu-bootstrap";

    private static final String DEPLOY_NAMESPACE = "shenyu";

    @Mock
    private AppsV1Api appsV1Api;

    private KubernetesScaler kubernetesScaler;

    @BeforeEach
    public void setUp() {
        final DeploymentProperties deploymentProperties = new DeploymentProperties();
        deploymentProperties.setName(DEPLOY_NAME);
        deploymentProperties.setNamespace(DEPLOY_NAMESPACE);
        kubernetesScaler = new KubernetesScaler(Optional.of(appsV1Api), deploymentProperties);
    }

    /**
     * A freshly built {@link V1Scale} carries a null spec, so the previous
     * {@code requireNonNull(scale.getSpec())} threw before the replace call.
     */
    @Test
    public void testScaleByNumSetsSpecBeforeReplace() throws Exception {
        kubernetesScaler.scaleByNum(3);

        final ArgumentCaptor<V1Scale> captor = ArgumentCaptor.forClass(V1Scale.class);
        verify(appsV1Api).replaceNamespacedDeploymentScale(
                eq(DEPLOY_NAME),
                eq(DEPLOY_NAMESPACE),
                captor.capture(),
                isNull(),
                isNull(),
                isNull(),
                isNull());

        final V1Scale sent = captor.getValue();
        assertNotNull(sent.getSpec(), "scale spec must be initialised before the replace call");
        assertEquals(3, sent.getSpec().getReplicas());
    }

    @Test
    public void testScaleByActionReadsCurrentAndAppliesDelta() throws Exception {
        final V1Scale current = new V1Scale();
        current.setSpec(new V1ScaleSpec().replicas(2));
        when(appsV1Api.readNamespacedDeploymentScale(eq(DEPLOY_NAME), eq(DEPLOY_NAMESPACE), any()))
                .thenReturn(current);

        kubernetesScaler.scaleByAction(new ScaleAction(ScaleAction.ActionType.SCALE_UP));

        final ArgumentCaptor<V1Scale> captor = ArgumentCaptor.forClass(V1Scale.class);
        verify(appsV1Api).replaceNamespacedDeploymentScale(
                eq(DEPLOY_NAME),
                eq(DEPLOY_NAMESPACE),
                captor.capture(),
                isNull(),
                isNull(),
                isNull(),
                isNull());
        assertEquals(3, captor.getValue().getSpec().getReplicas());
    }
}
