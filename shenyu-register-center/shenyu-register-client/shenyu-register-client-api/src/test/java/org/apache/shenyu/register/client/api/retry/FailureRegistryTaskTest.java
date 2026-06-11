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

package org.apache.shenyu.register.client.api.retry;

import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.register.client.api.FailbackRegistryRepository;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link FailureRegistryTask}.
 */
public final class FailureRegistryTaskTest {

    private static final String TEST_KEY = "test-key";

    @Mock
    private FailbackRegistryRepository mockRepository;

    @Mock
    private TimerTask mockTimerTask;

    private FailureRegistryTask failureRegistryTask;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        failureRegistryTask = new FailureRegistryTask(TEST_KEY, mockRepository);
    }


    @Test
    public void testDoRetry() {
        doNothing().when(mockRepository).accept(anyString());
        doNothing().when(mockRepository).remove(anyString());
        
        failureRegistryTask.doRetry(TEST_KEY, mockTimerTask);
        
        verify(mockRepository, times(1)).accept(TEST_KEY);
        verify(mockRepository, times(1)).remove(TEST_KEY);
    }

    @Test
    public void testDoRetryWithException() {

        doNothing().when(mockRepository).accept(anyString());
        doNothing().when(mockRepository).remove(anyString());
        
        // This should not throw an exception
        failureRegistryTask.doRetry(TEST_KEY, mockTimerTask);
        
        verify(mockRepository, times(1)).accept(TEST_KEY);
        verify(mockRepository, times(1)).remove(TEST_KEY);
    }

    @Test
    public void testMultipleRetries() {

        doNothing().when(mockRepository).accept(anyString());
        doNothing().when(mockRepository).remove(anyString());
        
        // Test multiple retry calls
        for (int i = 0; i < 3; i++) {
            failureRegistryTask.doRetry(TEST_KEY, mockTimerTask);
        }
        
        verify(mockRepository, times(3)).accept(TEST_KEY);
        verify(mockRepository, times(3)).remove(TEST_KEY);
    }

    @Test
    public void testDifferentKeys() {
        final String key1 = "key1";
        final String key2 = "key2";
        
        doNothing().when(mockRepository).accept(anyString());
        doNothing().when(mockRepository).remove(anyString());
        
        failureRegistryTask.doRetry(key1, mockTimerTask);
        failureRegistryTask.doRetry(key2, mockTimerTask);
        
        verify(mockRepository, times(1)).accept(key1);
        verify(mockRepository, times(1)).remove(key1);
        verify(mockRepository, times(1)).accept(key2);
        verify(mockRepository, times(1)).remove(key2);
    }

    @Test
    public void testTaskWithDifferentRepository() {
        TestFailbackRegistryRepository testRepository = new TestFailbackRegistryRepository();
        FailureRegistryTask task = new FailureRegistryTask("test", testRepository);
        
        task.doRetry("test", mockTimerTask);
        
        assertTrue(testRepository.acceptCalled);
        assertTrue(testRepository.removeCalled);
    }

    /**
     * Test implementation of FailbackRegistryRepository for testing.
     */
    private static class TestFailbackRegistryRepository extends FailbackRegistryRepository {
        
        private boolean acceptCalled;
        
        private boolean removeCalled;

        @Override
        public void accept(final String key) {
            acceptCalled = true;
        }

        @Override
        public void remove(final String key) {
            removeCalled = true;
        }

        @Override
        protected void doPersistApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistURI(final URIRegisterDTO registerDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistInterface(final MetaDataRegisterDTO registerDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistMcpTools(final McpToolsRegisterDTO registerDTO) {
            /* Test implementation */
        }
    }
}
