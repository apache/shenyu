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

package org.apache.shenyu.plugin.grpc.proto;

import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.exception.ShenyuException;

public class MyStreamObserver implements StreamObserver<Boolean> {

    private Boolean state;

    public MyStreamObserver(final Boolean state) {
        this.state = state;
    }

    @Override
    public void onNext(final Boolean value) {
        if (!value) {
            throw new ShenyuException("exception");
        } else {
            state = true;
        }
    }

    @Override
    public void onError(final Throwable t) {
        throw new ShenyuException("exception");
    }

    @Override
    public void onCompleted() {
        throw new ShenyuException("exception");
    }

    /**
     * get state.
     *
     * @return state.
     */
    public Boolean getState() {
        return state;
    }
}
