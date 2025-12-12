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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.chat.prompt.Prompt;
import org.springframework.ai.retry.NonTransientAiException;
import org.springframework.web.client.RestClientException;
import reactor.test.StepVerifier;

import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class AiProxyExecutorServiceTest {

    @Mock
    private ChatModel mainChatModel;

    @Mock
    private ChatModel fallbackChatModel;

    private ChatClient mainClient;

    private ChatClient fallbackClient;

    private AiProxyExecutorService executorService;

    @BeforeEach
    void setUp() {
        executorService = spy(new AiProxyExecutorService());
        mainClient = ChatClient.create(mainChatModel);
        fallbackClient = ChatClient.create(fallbackChatModel);
    }

    @Test
    void testExecuteSuccessOnFirstAttempt() {
        final ChatResponse successResponse = mock(ChatResponse.class);
        when(mainChatModel.call(any(Prompt.class))).thenReturn(successResponse);

        StepVerifier.create(executorService.execute(mainClient, Optional.empty(), "request"))
                .expectNext(successResponse)
                .verifyComplete();

        verify(mainChatModel, times(1)).call(any(Prompt.class));
    }

    @Test
    void testExecuteRetryOnRestClientExceptionAndSucceed() {
        final ChatResponse successResponse = mock(ChatResponse.class);
        when(mainChatModel.call(any(Prompt.class)))
                .thenThrow(new RestClientException("transient error"))
                .thenReturn(successResponse);

        StepVerifier.create(executorService.execute(mainClient, Optional.empty(), "request"))
                .expectNext(successResponse)
                .verifyComplete();

        verify(mainChatModel, times(2)).call(any(Prompt.class));
    }

    @Test
    void testExecuteRetryExhaustedThenFallback() {
        final ChatResponse fallbackResponse = mock(ChatResponse.class);
        when(mainChatModel.call(any(Prompt.class))).thenThrow(new RestClientException("transient error"));
        when(fallbackChatModel.call(any(Prompt.class))).thenReturn(fallbackResponse);

        StepVerifier.create(executorService.execute(mainClient, Optional.of(fallbackClient), "request"))
                .expectNext(fallbackResponse)
                .verifyComplete();

        verify(mainChatModel, times(4)).call(any(Prompt.class));
        verify(fallbackChatModel, times(1)).call(any(Prompt.class));
    }

    @Test
    void testExecuteNonTransientExceptionTriggersFallbackDirectly() {
        final ChatResponse fallbackResponse = mock(ChatResponse.class);
        when(mainChatModel.call(any(Prompt.class))).thenThrow(new NonTransientAiException("non-transient error"));
        when(fallbackChatModel.call(any(Prompt.class))).thenReturn(fallbackResponse);

        StepVerifier.create(executorService.execute(mainClient, Optional.of(fallbackClient), "request"))
                .expectNext(fallbackResponse)
                .verifyComplete();

        verify(mainChatModel, times(1)).call(any(Prompt.class));
        verify(fallbackChatModel, times(1)).call(any(Prompt.class));
    }

    @Test
    void testExecuteFallbackFails() {
        final RestClientException fallbackException = new RestClientException("fallback failed");
        when(mainChatModel.call(any(Prompt.class))).thenThrow(new NonTransientAiException("non-transient error"));
        when(fallbackChatModel.call(any(Prompt.class))).thenThrow(fallbackException);

        StepVerifier.create(executorService.execute(mainClient, Optional.of(fallbackClient), "request"))
                .expectErrorMatches(e -> e == fallbackException)
                .verify();

        verify(mainChatModel, times(1)).call(any(Prompt.class));
        verify(fallbackChatModel, times(1)).call(any(Prompt.class));
    }

    @Test
    void testExecuteNoFallbackProvided() {
        final NonTransientAiException mainException = new NonTransientAiException("non-transient error");
        when(mainChatModel.call(any(Prompt.class))).thenThrow(mainException);

        StepVerifier.create(executorService.execute(mainClient, Optional.empty(), "request"))
                .expectErrorMatches(e -> e == mainException)
                .verify();

        verify(mainChatModel, times(1)).call(any(Prompt.class));
    }
}
