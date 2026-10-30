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

package org.apache.shenyu.admin.service.fencing;

import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RegistrationFencingTargetMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.RegistrationFencingTargetDO;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Tests for {@link RegistrationFencingGuard}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class RegistrationFencingGuardTest {

    @Mock
    private RegistrationFencingTargetMapper targetMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    private RegistrationFencingGuard guard;

    @BeforeEach
    public void setUp() {
        guard = new RegistrationFencingGuard(targetMapper, selectorMapper, pluginMapper);
    }

    @Test
    public void enabledTargetRejectsLegacyAndManagementWrites() {
        RegistrationFencingTargetDO target = target(Boolean.TRUE);
        when(targetMapper.selectBySelectorAddress("selector-1", "127.0.0.1", 18080)).thenReturn(target);
        when(targetMapper.selectBySelectorId("selector-1")).thenReturn(List.of(target));
        when(targetMapper.selectByNamespaceAddress("ns-1", "127.0.0.1", 18080)).thenReturn(List.of(target));

        RegistrationFencingException exception = assertThrows(RegistrationFencingException.class,
                () -> guard.assertLegacyUriWrite("selector-1", "127.0.0.1", 18080));
        assertEquals(RegistrationFencingProtocol.FENCING_REQUIRED, exception.getCode());
        assertThrows(RegistrationFencingException.class, () -> guard.assertSelectorWriteAllowed("selector-1"));
        assertTrue(guard.isProtectedSelector("selector-1"));
        assertTrue(guard.isProtectedAddress("ns-1", "127.0.0.1", 18080));
    }

    @Test
    public void disabledTombstoneNoLongerProtectsItsSelector() {
        RegistrationFencingTargetDO target = target(Boolean.FALSE);
        when(targetMapper.selectBySelectorAddress("selector-1", "127.0.0.1", 18080)).thenReturn(target);
        when(targetMapper.selectBySelectorId("selector-1")).thenReturn(List.of(target));
        when(targetMapper.selectByNamespaceAddress("ns-1", "127.0.0.1", 18080)).thenReturn(List.of(target));

        assertDoesNotThrow(() -> guard.assertLegacyUriWrite("selector-1", "127.0.0.1", 18080));
        assertDoesNotThrow(() -> guard.assertSelectorWriteAllowed("selector-1"));
        assertFalse(guard.isProtectedSelector("selector-1"));
    }

    @Test
    public void fencedContextMayWriteItsOwnSelector() {
        RegistrationFencingTargetDO target = target(Boolean.TRUE);
        when(targetMapper.selectBySelectorAddress("selector-1", "127.0.0.1", 18080)).thenReturn(target);
        when(targetMapper.selectBySelectorId("selector-1")).thenReturn(List.of(target));

        try (RegistrationFencingContext ignored =
                     RegistrationFencingContext.enter("target-1", "selector-1", "session-1", 1L)) {
            assertDoesNotThrow(() -> guard.assertLegacyUriWrite("selector-1", "127.0.0.1", 18080));
            assertDoesNotThrow(() -> guard.assertSelectorWriteAllowed("selector-1"));
        }
    }

    @Test
    public void legacyMemberWriteIsNotBlockedByAnotherAddressInTheSameSelector() {
        when(targetMapper.selectBySelectorAddress("selector-1", "127.0.0.2", 18081)).thenReturn(null);

        assertDoesNotThrow(() -> guard.assertLegacyUriWrite("selector-1", "127.0.0.2", 18081));
    }

    private RegistrationFencingTargetDO target(final Boolean enabled) {
        RegistrationFencingTargetDO target = new RegistrationFencingTargetDO();
        target.setId("target-1");
        target.setNamespaceId("ns-1");
        target.setSelectorId("selector-1");
        target.setHost("127.0.0.1");
        target.setPort(18080);
        target.setEnabled(enabled);
        return target;
    }
}
