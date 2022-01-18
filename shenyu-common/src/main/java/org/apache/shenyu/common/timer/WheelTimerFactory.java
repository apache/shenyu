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

import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;

import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * WheelTimerFactory .
 * shared wheel time.
 */
public class WheelTimerFactory {
    
    private static final TimerSharedRef SHARED_TIMER = new TimerSharedRef("shared_wheel_timer");
    
    /**
     * Gets wheel timer.
     *
     * @return the wheel timer
     */
    public static Timer getSharedTimer() {
        return SHARED_TIMER.getRef();
    }
    
    /**
     * New wheel timer hashed wheel timer.
     *
     * @return the hashed wheel timer
     */
    public static Timer newWheelTimer() {
        return new HashedWheelTimer(100, TimeUnit.MILLISECONDS, 4069);
    }
    
    /**
     * New wheel timer timer.
     *
     * @param name the name
     * @return the timer
     */
    public static Timer newWheelTimer(final String name) {
        return new HashedWheelTimer(ShenyuThreadFactory.create(name, false), 100, TimeUnit.MILLISECONDS, 4069);
    }
    
    /**
     * New wheel timer hashed wheel timer.
     *
     * @param tickDuration the tick duration
     * @param unit         the unit
     * @return the hashed wheel timer
     */
    public static Timer newWheelTimer(final long tickDuration, final TimeUnit unit) {
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
    public static Timer newWheelTimer(final long tickDuration, final TimeUnit unit, final int ticksPerWheel) {
        return new HashedWheelTimer(tickDuration, unit, ticksPerWheel);
    }
    
    private abstract static class Shared<T> {
        /**
         * The Shared.
         */
        private final T shared;
        
        /**
         * Instantiates a new Shared.
         *
         * @param shared the shared
         */
        Shared(final T shared) {
            this.shared = shared;
        }
        
        /**
         * Gets ref.
         *
         * @return the ref
         */
        public T getRef() {
            return this.current();
        }
        
        /**
         * Gets shared.
         *
         * @return the shared
         */
        protected T getSharedObj() {
            return shared;
        }
        
        /**
         * Current t.
         *
         * @return the t
         */
        protected abstract T current();
    }
    
    private abstract static class SharedRef<T> {
        
        private final String name;
        
        private Shared<T> shared;
        
        /**
         * Instantiates a new Shared ref.
         *
         * @param name the name
         */
        SharedRef(final String name) {
            this.name = name;
        }
        
        /**
         * Gets ref.
         *
         * @return the ref
         */
        public synchronized T getRef() {
            if (shared == null) {
                this.shared = create();
            }
            return this.shared.getRef();
        }
        
        /**
         * Gets name.
         *
         * @return the name
         */
        public String getName() {
            return name;
        }
        
        /**
         * Create shared.
         *
         * @return the shared
         */
        protected abstract Shared<T> create();
    }
    
    private static class TimerShared extends Shared<Timer> implements Timer {
        
        /**
         * Instantiates a new Shared.
         *
         * @param shared the shared
         */
        TimerShared(final Timer shared) {
            super(shared);
        }
        
        @Override
        protected Timer current() {
            return this;
        }
        
        @Override
        public Timeout newTimeout(final TimerTask task, final long delay, final TimeUnit unit) {
            return this.getSharedObj().newTimeout(task, delay, unit);
        }
        
        @Override
        public Set<Timeout> stop() {
            return this.getSharedObj().stop();
        }
        
        @Override
        public boolean isStop() {
            return this.getSharedObj().isStop();
        }
    }
    
    private static class TimerSharedRef extends SharedRef<Timer> {
        
        /**
         * Instantiates a new Shared ref.
         *
         * @param name the name
         */
        TimerSharedRef(final String name) {
            super(name);
        }
        
        /**
         * Create shared.
         *
         * @return the shared
         */
        @Override
        protected Shared<Timer> create() {
            return new TimerShared(WheelTimerFactory.newWheelTimer(this.getName()));
        }
    }
}
