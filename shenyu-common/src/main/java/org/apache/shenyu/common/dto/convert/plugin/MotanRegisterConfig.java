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

package org.apache.shenyu.common.dto.convert.plugin;

import java.io.Serializable;
import java.util.Objects;

/**
 * Motan register config.
 */
public class MotanRegisterConfig implements Serializable {

    private static final long serialVersionUID = 2488053756247710408L;

    private String register;

    private String threadpool;

    private Integer corethreads;

    private Integer threads;

    private Integer queues;

    /**
     * get threadpool.
     *
     * @return threadpool
     */
    public String getThreadpool() {
        return threadpool;
    }

    /**
     * set threadpool.
     *
     * @param threadpool threadpool
     */
    public void setThreadpool(final String threadpool) {
        this.threadpool = threadpool;
    }

    /**
     * get register.
     *
     * @return register
     */
    public String getRegister() {
        return register;
    }

    /**
     * set register.
     *
     * @param register register
     */
    public void setRegister(final String register) {
        this.register = register;
    }

    /**
     * get corethreads.
     *
     * @return corethreads
     */
    public Integer getCorethreads() {
        return corethreads;
    }

    /**
     * set corethreads.
     *
     * @param corethreads corethreads
     */
    public void setCorethreads(final Integer corethreads) {
        this.corethreads = corethreads;
    }

    /**
     * get threads.
     *
     * @return threads
     */
    public Integer getThreads() {
        return threads;
    }

    /**
     * set threads.
     *
     * @param threads threads
     */
    public void setThreads(final Integer threads) {
        this.threads = threads;
    }

    /**
     * get queues.
     *
     * @return queues
     */
    public Integer getQueues() {
        return queues;
    }

    /**
     * set queues.
     *
     * @param queues queues
     */
    public void setQueues(final Integer queues) {
        this.queues = queues;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        MotanRegisterConfig that = (MotanRegisterConfig) o;
        return Objects.equals(register, that.register) && Objects.equals(threadpool, that.threadpool)
                && Objects.equals(corethreads, that.corethreads) && Objects.equals(threads, that.threads)
                && Objects.equals(queues, that.queues);
    }

    @Override
    public int hashCode() {
        return Objects.hash(register, threadpool, corethreads, threads, queues);
    }

    @Override
    public String toString() {
        return "MotanRegisterConfig{"
                + "register='"
                + register
                + '\''
                + ", threadpool='"
                + threadpool
                + '\''
                + ", corethreads='"
                + corethreads
                + '\''
                + ", threads='"
                + threads
                + '\''
                + ", queues='"
                + queues
                + '\''
                + '}';
    }
}
