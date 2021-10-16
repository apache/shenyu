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

package org.apache.shenyu.examples.http.result;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * The type Payment type service.
 */
public class PaymentTypeService {

    private static final Logger LOG = LoggerFactory.getLogger(PaymentTypeService.class);

    private static final String THREAD_NAME = "remote_type";

    private static final int MAX_THREADS = Runtime.getRuntime().availableProcessors() << 1;

    private static final ExecutorService EXECUTOR_SERVICE =
            new ThreadPoolExecutor(MAX_THREADS, MAX_THREADS, 0, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<>(1024),
                    PaymentThreadFactory.create(THREAD_NAME, false),
                    new ThreadPoolExecutor.AbortPolicy());

    /**
     * Inject RemoteService.
     */
    private PaymentRemoteService paymentRemoteService;

    /**
     * Acquire remote payment types list.
     *
     * @param paymentTypes the payment types
     * @return the list
     */
    List<ConsultResult> acquireRemotePaymentTypes(final List<String> paymentTypes) {
        List<Future<ConsultResult>> futureList = new ArrayList<>(paymentTypes.size());
        for (String paymentType : paymentTypes) {
            Future<ConsultResult> result = EXECUTOR_SERVICE.submit(new AcquireTypeThread(paymentRemoteService, paymentType));
            futureList.add(result);
        }
        List<ConsultResult> resultList = new ArrayList<>(futureList.size());
        for (Future<ConsultResult> future : futureList) {
            try {
                resultList.add(future.get());
            } catch (InterruptedException | ExecutionException ex) {
                LOG.error(ex.getMessage(), ex);
            }
        }
        return resultList;
    }

    private static class AcquireTypeThread implements Callable<ConsultResult> {

        private final PaymentRemoteService paymentRemoteProxy;

        private final String paymentType;

        /**
         * Instantiates a new Acquire type thread.
         *
         * @param paymentRemoteProxy the payment remote proxy
         * @param paymentType        the payment type
         */
        AcquireTypeThread(final PaymentRemoteService paymentRemoteProxy, final String paymentType) {
            this.paymentRemoteProxy = paymentRemoteProxy;
            this.paymentType = paymentType;
        }

        @Override
        public ConsultResult call() {
            return paymentRemoteProxy.isEnabled(paymentType);
        }
    }

    private static final class PaymentThreadFactory implements ThreadFactory {

        private static final AtomicLong THREAD_NUMBER = new AtomicLong(1);

        private final boolean daemon;

        private final String namePrefix;

        private PaymentThreadFactory(final String namePrefix, final boolean daemon) {
            this.namePrefix = namePrefix;
            this.daemon = daemon;
        }

        /**
         * create custom thread factory.
         *
         * @param namePrefix prefix
         * @param daemon     daemon
         * @return {@linkplain ThreadFactory}
         */
        public static ThreadFactory create(final String namePrefix,
                                           final boolean daemon) {
            return new PaymentThreadFactory(namePrefix, daemon);
        }

        /**
         * create custom thread factory.
         *
         * @param namePrefix prefix
         * @return {@linkplain ThreadFactory}
         */
        public static ThreadFactory create(final String namePrefix) {
            return new PaymentThreadFactory(namePrefix, false);
        }

        @Override
        public Thread newThread(final Runnable runnable) {
            Thread thread = new Thread(runnable, "payment" + "-" + namePrefix + "-" + THREAD_NUMBER.getAndIncrement());
            thread.setDaemon(daemon);
            if (thread.getPriority() != Thread.NORM_PRIORITY) {
                thread.setPriority(Thread.NORM_PRIORITY);
            }
            return thread;
        }
    }

    private interface PaymentRemoteService {

        /**
         * Is enabled consult result.
         *
         * @param paymentType the payment type
         * @return the consult result
         */
        ConsultResult isEnabled(String paymentType);
    }

    private static class ConsultResult {

        /**
         * isEnable.
         */
        private boolean isEnable;

        /**
         * errorCode.
         */
        private String errorCode;

        /**
         * Instantiates a new Consult result.
         *
         * @param isEnable  the is enable
         * @param errorCode the error code
         */
        ConsultResult(final boolean isEnable, final String errorCode) {
            this.isEnable = isEnable;
            this.errorCode = errorCode;
        }

        /**
         * Gets is enable.
         *
         * @return the is enable
         */
        public boolean getIsEnable() {
            return isEnable;
        }

        /**
         * Gets error code.
         *
         * @return the error code
         */
        public String getErrorCode() {
            return errorCode;
        }

    }

}
