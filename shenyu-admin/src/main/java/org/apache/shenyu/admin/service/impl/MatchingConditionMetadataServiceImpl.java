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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.model.vo.MatchingConditionCatalogVO;
import org.apache.shenyu.admin.model.vo.MatchingConditionExampleVO;
import org.apache.shenyu.admin.model.vo.MatchingConditionOperatorVO;
import org.apache.shenyu.admin.model.vo.MatchingConditionSourceVO;
import org.apache.shenyu.admin.service.MatchingConditionMetadataService;
import org.apache.shenyu.common.condition.MatchingConditionCatalog;
import org.apache.shenyu.common.condition.MatchingConditionExample;
import org.apache.shenyu.common.condition.MatchingConditionOperator;
import org.apache.shenyu.common.condition.MatchingConditionSource;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Maps the common matching-condition catalog to Admin views.
 */
@Service
public class MatchingConditionMetadataServiceImpl implements MatchingConditionMetadataService {

    private static final MatchingConditionCatalogVO CATALOG = catalogView();

    @Override
    public MatchingConditionCatalogVO catalog() {
        return CATALOG;
    }

    private static MatchingConditionCatalogVO catalogView() {
        List<MatchingConditionSourceVO> sources = MatchingConditionCatalog.sources().stream()
                .map(MatchingConditionMetadataServiceImpl::sourceView).collect(Collectors.toList());
        List<MatchingConditionOperatorVO> operators = MatchingConditionCatalog.operators().stream()
                .map(MatchingConditionMetadataServiceImpl::operatorView).collect(Collectors.toList());
        return new MatchingConditionCatalogVO(MatchingConditionCatalog.version(), sources, operators);
    }

    private static MatchingConditionSourceVO sourceView(final MatchingConditionSource source) {
        List<MatchingConditionExampleVO> examples = source.examples().stream()
                .map(MatchingConditionMetadataServiceImpl::exampleView).collect(Collectors.toList());
        return new MatchingConditionSourceVO(source.id(), source.displayKey(), source.helpKey(), source.status(),
                source.valueType(), source.paramNameRequired(), source.runtimeRegistered(), source.operatorIds(), examples);
    }

    private static MatchingConditionOperatorVO operatorView(final MatchingConditionOperator operator) {
        return new MatchingConditionOperatorVO(operator.id(), operator.displayKey(), operator.helpKey(), operator.status(),
                operator.requiresValue(), operator.valueFormat());
    }

    private static MatchingConditionExampleVO exampleView(final MatchingConditionExample example) {
        return new MatchingConditionExampleVO(example.sourceId(), example.operatorId(), example.paramName(),
                example.paramValue(), example.descriptionKey());
    }
}
