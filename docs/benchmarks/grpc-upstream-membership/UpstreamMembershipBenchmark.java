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

package org.apache.shenyu.benchmark;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;

@State(Scope.Thread)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
public class UpstreamMembershipBenchmark {
    @Param({"8", "1024", "4096"})
    public int size;
    @Param({"same", "reversed"})
    public String order;
    private List<GrpcUpstream> saved;
    private List<GrpcUpstream> incoming;

    @Setup
    public void setup() {
        saved = new ArrayList<>();
        incoming = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            saved.add(GrpcUpstream.builder().upstreamUrl("127.0.0.1:" + (8080 + i)).weight(50).status(true).build());
            incoming.add(GrpcUpstream.builder().upstreamUrl("127.0.0.1:" + (8080 + i)).weight(50).status(true).build());
        }
        if (order.equals("reversed")) {
            Collections.reverse(incoming);
        }
        if (baseline() || optimized()) {
            throw new AssertionError("Equal membership must not trigger a refresh");
        }
    }

    @Benchmark
    public boolean baseline() {
        if (saved.size() != incoming.size()) {
            return true;
        }
        return saved.stream().anyMatch(instance -> !incoming.contains(instance));
    }

    @Benchmark
    public boolean optimized() {
        if (saved.size() != incoming.size()) {
            return true;
        }
        if (saved.equals(incoming)) {
            return false;
        }
        Set<GrpcUpstream> incomingSet = new HashSet<>(incoming);
        return !incomingSet.containsAll(saved);
    }
}
