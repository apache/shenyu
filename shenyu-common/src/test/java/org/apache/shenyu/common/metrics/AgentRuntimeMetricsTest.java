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

package org.apache.shenyu.common.metrics;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.SignalType;

import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

class AgentRuntimeMetricsTest {

    @Test
    void attemptsAreSeparateFromOneLogicalRequestAndTheApplicationObserver() {
        AgentRuntimeMetrics metrics = mock(AgentRuntimeMetrics.class);
        AiCallObserver observer = mock(AiCallObserver.class);
        AgentRequestContext context = new AgentRequestContext("private-id", "llm", "selector", "rule");
        AiRequestObservation request = new AiRequestObservation(observer, context, metrics);
        AiRequestObservation.Attempt first = request.startAttempt(context, AiAttemptEvent.Kind.PRIMARY, "private-provider", "private-protocol", false);
        first.finish(SignalType.ON_ERROR, AiCallOutcome.TIMEOUT);
        first.finish(SignalType.ON_ERROR, AiCallOutcome.TIMEOUT);
        AiRequestObservation.Attempt second = request.startAttempt(context, AiAttemptEvent.Kind.RETRY, "private-provider", "private-protocol", true);
        second.status(200);
        second.finish(SignalType.ON_COMPLETE, null);
        request.finish(SignalType.ON_COMPLETE, 200);
        request.finish(SignalType.CANCEL, null);
        verify(metrics).started(AgentRuntimeMetrics.Traffic.LLM);
        verify(metrics).attempt(eq(AiAttemptEvent.Kind.PRIMARY), eq(AgentRuntimeMetrics.Outcome.TIMEOUT), anyLong());
        verify(metrics).attempt(eq(AiAttemptEvent.Kind.RETRY), eq(AgentRuntimeMetrics.Outcome.SUCCESS), anyLong());
        verify(metrics).finished(eq(AgentRuntimeMetrics.Traffic.LLM), eq(AgentRuntimeMetrics.Outcome.SUCCESS), eq(AgentRuntimeMetrics.Reason.NONE), anyLong());
        verifyNoMoreInteractions(metrics);
        verify(observer, times(2)).recordAttempt(org.mockito.ArgumentMatchers.any());
        verify(observer).record(org.mockito.ArgumentMatchers.any());
    }

    @Test
    void cancellationFreezesTheOutstandingAttemptAndReleasesLogicalInflightOnce() {
        AgentRuntimeMetrics metrics = mock(AgentRuntimeMetrics.class);
        AgentRequestContext context = new AgentRequestContext("id", "llm", "s", "r");
        AiRequestObservation request = new AiRequestObservation(null, context, metrics);
        AiRequestObservation.Attempt attempt = request.startAttempt(context, AiAttemptEvent.Kind.FALLBACK, "p", "p", true);
        request.finish(SignalType.CANCEL, null);
        attempt.finish(SignalType.CANCEL, null);
        verify(metrics).started(AgentRuntimeMetrics.Traffic.LLM);
        verify(metrics).attempt(eq(AiAttemptEvent.Kind.FALLBACK), eq(AgentRuntimeMetrics.Outcome.CANCELLED), anyLong());
        verify(metrics).finished(eq(AgentRuntimeMetrics.Traffic.LLM), eq(AgentRuntimeMetrics.Outcome.CANCELLED), eq(AgentRuntimeMetrics.Reason.NONE), anyLong());
        verifyNoMoreInteractions(metrics);
    }
}
