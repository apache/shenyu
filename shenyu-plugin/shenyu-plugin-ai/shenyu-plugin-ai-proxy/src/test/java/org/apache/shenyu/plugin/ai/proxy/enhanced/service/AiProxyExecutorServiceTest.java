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
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletion;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionChunk;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;
import org.springframework.ai.retry.NonTransientAiException;
import org.springframework.http.ResponseEntity;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.util.Optional;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class AiProxyExecutorServiceTest {

    private AiProxyExecutorService executorService;

    @BeforeEach
    void setUp() {
        executorService = new AiProxyExecutorService();
    }

    @Test
    void testExecuteDirectStreamSuccess() {
        final OpenAiApi mainApi = mock(OpenAiApi.class);
        final ChatCompletionChunk chunk = mock(ChatCompletionChunk.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        when(mainApi.chatCompletionStream(request)).thenReturn(Flux.just(chunk));

        StepVerifier.create(executorService.executeDirectStream(mainApi, Optional.empty(), request))
                .expectNext(chunk)
                .verifyComplete();

        verify(mainApi, times(1)).chatCompletionStream(request);
    }

    @Test
    void testExecuteDirectStreamErrorWithNoFallback() {
        final OpenAiApi mainApi = mock(OpenAiApi.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        when(mainApi.chatCompletionStream(request)).thenAnswer(inv -> Flux.error(new RuntimeException("upstream error")));

        StepVerifier.create(executorService.executeDirectStream(mainApi, Optional.empty(), request))
                .expectError(NonTransientAiException.class)
                .verify();
    }

    @Test
    void testExecuteDirectStreamFallbackSuccess() {
        final OpenAiApi mainApi = mock(OpenAiApi.class);
        final OpenAiApi fallbackApi = mock(OpenAiApi.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        final ChatCompletionChunk fallbackChunk = mock(ChatCompletionChunk.class);

        when(mainApi.chatCompletionStream(request)).thenAnswer(inv -> Flux.error(new RuntimeException("upstream error")));
        when(fallbackApi.chatCompletionStream(request)).thenReturn(Flux.just(fallbackChunk));

        StepVerifier.create(executorService.executeDirectStream(mainApi, Optional.of(fallbackApi), request))
                .expectNext(fallbackChunk)
                .verifyComplete();

        verify(fallbackApi, times(1)).chatCompletionStream(request);
    }

    @Test
    void testExecuteDirectCallSuccess() {
        final OpenAiApi mainApi = mock(OpenAiApi.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        final ChatCompletion completion = mock(ChatCompletion.class);
        final ResponseEntity<ChatCompletion> responseEntity = ResponseEntity.ok(completion);
        when(mainApi.chatCompletionEntity(request)).thenReturn(responseEntity);

        StepVerifier.create(executorService.executeDirectCall(mainApi, Optional.empty(), request))
                .expectNext(responseEntity)
                .verifyComplete();

        verify(mainApi, times(1)).chatCompletionEntity(request);
    }

    @Test
    void testExecuteDirectCallErrorWithNoFallback() {
        final OpenAiApi mainApi = mock(OpenAiApi.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        when(mainApi.chatCompletionEntity(request)).thenThrow(new RuntimeException("upstream error"));

        StepVerifier.create(executorService.executeDirectCall(mainApi, Optional.empty(), request))
                .expectError(NonTransientAiException.class)
                .verify();
    }

    @Test
    void testExecuteDirectCallFallbackSuccess() {
        final OpenAiApi mainApi = mock(OpenAiApi.class);
        final OpenAiApi fallbackApi = mock(OpenAiApi.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        final ChatCompletion fallbackCompletion = mock(ChatCompletion.class);
        final ResponseEntity<ChatCompletion> fallbackResponse = ResponseEntity.ok(fallbackCompletion);

        when(mainApi.chatCompletionEntity(request)).thenThrow(new RuntimeException("upstream error"));
        when(fallbackApi.chatCompletionEntity(request)).thenReturn(fallbackResponse);

        StepVerifier.create(executorService.executeDirectCall(mainApi, Optional.of(fallbackApi), request))
                .expectNext(fallbackResponse)
                .verifyComplete();

        verify(fallbackApi, times(1)).chatCompletionEntity(request);
    }
}
