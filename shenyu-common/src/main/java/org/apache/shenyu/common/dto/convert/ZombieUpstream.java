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

package org.apache.shenyu.common.dto.convert;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * this is zombie divide upstream.
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ZombieUpstream {

    /**
     * divide upstream.
     */
    @EqualsAndHashCode.Include
    private DivideUpstream divideUpstream;

    /**
     * total check times.
     */
    @EqualsAndHashCode.Include
    private int zombieCheckTimes;

    /**
     * origin selector name.
     */
    @EqualsAndHashCode.Include
    private String selectorName;

    /**
     * create zombie upstream with divide upstream.
     * @param divideUpstream {@linkplain DivideUpstream} origin divide upstream.
     * @param zombieCheckTimes total check times.
     * @param selectorName origin selector name.
     * @return new zombie upstream.
     */
    public static ZombieUpstream transform(final DivideUpstream divideUpstream, final int zombieCheckTimes, final String selectorName) {
        return ZombieUpstream.builder().divideUpstream(divideUpstream).zombieCheckTimes(zombieCheckTimes).selectorName(selectorName).build();
    }
}
