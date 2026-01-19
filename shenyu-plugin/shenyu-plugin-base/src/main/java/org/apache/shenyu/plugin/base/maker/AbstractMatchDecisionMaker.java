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

package org.apache.shenyu.plugin.base.maker;

import org.apache.shenyu.common.dto.BaseData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.DataProvider;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import java.util.List;

/**
 * Abstract base class for making match decisions in plugins using the Template Method pattern.
 *
 * <p>
 * This class provides a template for processing data matching logic in plugins. Subclasses are expected
 * to implement the abstract methods to define specific behaviors for handling empty data, matching data,
 * and determining whether processing should continue.
 * </p>
 *
 * <p>
 * The Template Method pattern is used here to allow subclasses to override certain steps of the algorithm
 * without changing its structure.
 * </p>
 *
 * @param <T> the type of data to be matched, extending {@link BaseData}
 */
public abstract class AbstractMatchDecisionMaker<T extends BaseData> {

    protected static final String URI_CONDITION_TYPE = "uri";

    private final DataProvider<T> dataProvider;


    /**
     * Constructs an AbstractMatchDecisionMaker with the specified data provider.
     *
     * @param dataProvider the data provider used to retrieve data for matching
     */
    protected AbstractMatchDecisionMaker(final DataProvider<T> dataProvider) {
        this.dataProvider = dataProvider;
    }

    /**
     * Retrieves a list of data associated with the given key.
     *
     * @param key the key used to retrieve data
     * @return a list of data objects associated with the key
     */
    public List<T> getData(final String key) {
        return dataProvider.getData(key);
    }

    /**
     * Handles the scenario when no matching data is found for the given plugin.
     *
     * @param pluginName the name of the plugin
     * @param exchange the current server web exchange
     * @param chain the plugin chain to continue processing
     * @return a {@link Mono} that completes when the handling is done
     */
    protected abstract Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain);

    /**
     * Matches the appropriate data from the provided list based on the exchange and path.
     *
     * @param exchange the current server web exchange
     * @param dataList the list of data to match against
     * @param path the request path to use for matching
     * @return the matched data object, or {@code null} if no match is found
     */
    protected abstract T matchData(ServerWebExchange exchange, String dataName, List<T> dataList, String path, SelectorData selectorData);

    /**
     * Determines whether processing should continue based on the matched data.
     *
     * @param data the matched data object
     * @return {@code true} if processing should continue, {@code false} otherwise
     */
    protected abstract boolean shouldContinue(T data);
}