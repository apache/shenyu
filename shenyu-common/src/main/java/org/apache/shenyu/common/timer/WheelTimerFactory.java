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

/**
 * WheelTimerFactory .
 * shared wheel time.
 */
public class WheelTimerFactory {
    
    private static final String NAME = "shared_wheel_timer";
    
    private static final TimerSharedRef SHARED_TIMER = new TimerSharedRef();
    
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
        return new HierarchicalWheelTimer(NAME);
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
        
        private Shared<T> shared;
        
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
        
        /**
         * Add timer task.
         *
         * @param timerTask the timer task
         */
        @Override
        public void add(final TimerTask timerTask) {
            this.getSharedObj().add(timerTask);
        }
        
        /**
         * Advance clock boolean.
         *
         * @param timeoutMs the timeout ms
         * @throws InterruptedException the interrupted exception
         */
        @Override
        public void advanceClock(final long timeoutMs) throws InterruptedException {
            this.getSharedObj().advanceClock(timeoutMs);
        }
        
        /**
         * Size int.
         *
         * @return the int
         */
        @Override
        public int size() {
            return this.getSharedObj().size();
        }
        
        /**
         * Shutdown.
         */
        @Override
        public void shutdown() {
            this.getSharedObj().shutdown();
        }
    }
    
    private static class TimerSharedRef extends SharedRef<Timer> {
        
        /**
         * Create shared.
         *
         * @return the shared
         */
        @Override
        protected Shared<Timer> create() {
            return new TimerShared(WheelTimerFactory.newWheelTimer());
        }
    }
}
