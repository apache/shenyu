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

package org.apache.shenyu.common.timer;

import java.util.concurrent.TimeUnit;

/**
 * WheelTimerFactory .
 * global wheel time.
 */
public class WheelTimerFactory {
    
    private static volatile HashedWheelTimer hashedWheelTimer;
    
    /**
     * Gets wheel timer.
     *
     * @return the wheel timer
     */
    public static HashedWheelTimer singletonWheelTimer() {
        if (hashedWheelTimer == null) {
            synchronized (HashedWheelTimer.class) {
                if (hashedWheelTimer == null) {
                    hashedWheelTimer = newWheelTimer(100, TimeUnit.MILLISECONDS, 4069);
                }
            }
        }
        return hashedWheelTimer;
    }
    
    /**
     * New wheel timer hashed wheel timer.
     *
     * @return the hashed wheel timer
     */
    public static HashedWheelTimer newWheelTimer() {
        return new HashedWheelTimer();
    }
    
    /**
     * New wheel timer hashed wheel timer.
     *
     * @param tickDuration the tick duration
     * @param unit         the unit
     * @return the hashed wheel timer
     */
    public static HashedWheelTimer newWheelTimer(final long tickDuration, final TimeUnit unit) {
        return new HashedWheelTimer(tickDuration, unit);
    }
    
    /**
     * New wheel timer hashed wheel timer.
     *
     * @param tickDuration  the tick duration
     * @param unit          the unit
     * @param ticksPerWheel the ticks per wheel
     * @return the hashed wheel timer
     */
    public static HashedWheelTimer newWheelTimer(final long tickDuration, final TimeUnit unit, final int ticksPerWheel) {
        return new HashedWheelTimer(tickDuration, unit, ticksPerWheel);
    }
    
}
