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

package org.apache.shenyu.e2e.engine.scenario;

import com.google.common.collect.ImmutableList;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.jetbrains.annotations.NotNull;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.Supplier;

public class ShenYuScenarioSupplier implements Iterator<ScenarioSpec>, Supplier<ScenarioSpec>, Iterable<ScenarioSpec> {
    private final ImmutableList<ScenarioSpec> supplies;
    
    private int index = -1;
    
    ShenYuScenarioSupplier(List<ScenarioSpec> supplies) {
        this.supplies = ImmutableList.<ScenarioSpec>builder().addAll(supplies).build();
    }
    
    @NotNull
    @Override
    public Iterator<ScenarioSpec> iterator() {
        return supplies.iterator();
    }
    
    public int size() {
        return supplies.size();
    }
    
    public ScenarioSpec get(int index) {
        return supplies.get(index);
    }
    
    @Override
    public boolean hasNext() {
        index++;
        return index < size();
    }
    
    @Override
    public ScenarioSpec next() {
        return get();
    }
    
    public ScenarioSpec get() {
        if (index < 0) {
            throw new NoSuchElementException();
        }
        return get(index);
    }
}
