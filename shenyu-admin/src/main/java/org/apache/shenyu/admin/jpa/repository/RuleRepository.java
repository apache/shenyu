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

package org.apache.shenyu.admin.jpa.repository;

import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * RuleRepository.
 */
@Repository
public interface RuleRepository extends JpaRepository<RuleDO, String>, ExistProvider {

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    default Boolean existed(Serializable id) {
        return existsById((String) id);
    }

    /**
     * Find by selector id.
     *
     * @param selectorId the selector id
     * @return list of {@linkplain RuleDO}
     */
    List<RuleDO> findBySelectorId(String selectorId);

    /**
     * Find by selector ids.
     *
     * @param selectorIds the selector ids
     * @return list of {@linkplain RuleDO}
     */
    List<RuleDO> findBySelectorIdIn(List<String> selectorIds);

    /**
     * Find by rule name.
     *
     * @param ruleName the rule name
     * @return {@linkplain RuleDO}
     */
    Optional<RuleDO> findByRuleName(String ruleName);

    /**
     * Find by selector id and rule name.
     *
     * @param selectorId the selector id
     * @param ruleName   the rule name
     * @return {@linkplain RuleDO}
     */
    Optional<RuleDO> findBySelectorIdAndRuleName(String selectorId, String ruleName);

}
