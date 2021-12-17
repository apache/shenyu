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
 *
 */

package org.apache.shenyu.disruptor;

import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.disruptor.consumer.QueueConsumerFactory;
import org.apache.shenyu.disruptor.provider.DisruptorProvider;
import org.junit.Before;
import org.junit.Test;

/**
 * DisruptorTest .
 * Test the problem of disruptor sequential consumption.
 *
 * @author sixh chenbin
 */
public class DisruptorTest {

    private DisruptorProvider<Long> provider;

    /**
     * Sets .
     */
    @Before
    public void setup() {
        DisruptorProviderManage<Long> providerManage = new DisruptorProviderManage<>(new QueueConsumerFactoryTest(), 4, 4096);
        providerManage.startup(true);
        provider = providerManage.getProvider();
    }

    /**
     * Test disruptor order.
     */
    @Test
    public void testDisruptorOrder() throws InterruptedException {
        for (long i = 0; i < 100; i++) {
            provider.onOrderlyData(i, "192.168.3.80:9000");
        }
        Thread.sleep(Integer.MAX_VALUE);
    }

    private static class QueueConsumerFactoryTest extends QueueConsumerExecutor<Long> implements QueueConsumerFactory<Long> {

        /**
         * Create queue consumer executor.
         *
         * @return the queue consumer executor
         */
        @Override
        public QueueConsumerExecutor<Long> create() {
            return new QueueConsumerFactoryTest();
        }

        /**
         * Fix name string.
         *
         * @return the string
         */
        @Override
        public String fixName() {
            return "test";
        }

        /**
         * When an object implementing interface <code>Runnable</code> is used
         * to create a thread, starting the thread causes the object's
         * <code>run</code> method to be called in that separately executing
         * thread.
         * <p>
         * The general contract of the method <code>run</code> is that it may
         * take any action whatsoever.
         *
         * @see Thread#run()
         */
        @Override
        public void run() {
            Long data = this.getData();
            System.out.println(data + "-" + Thread.currentThread().getName());
        }
    }
}
