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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.Before;
import org.junit.Test;
import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * Test cases for MetaDataMapper.
 */
public final class MetaDataMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private MetaDataMapper metaDataMapper;

    /**
     * Clean data before test.
     */
    @Before
    public void setUp() {
        List<MetaDataDO> all = metaDataMapper.findAll();
        for (MetaDataDO metaDataDO : all) {
            metaDataMapper.delete(metaDataDO.getId());
        }
    }

    @Test
    public void selectById() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        String id = metaDataDO.getId();
        MetaDataDO result = metaDataMapper.selectById(id);
        assertThat(result.getId(), comparesEqualTo(id));
    }

    @Test
    public void findAll() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        List<MetaDataDO> result = metaDataMapper.findAll();
        assertThat(result.size(), comparesEqualTo(1));
        assertThat(result.get(0).getId(), comparesEqualTo(metaDataDO.getId()));
    }

    @Test
    public void findByPath() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        MetaDataDO result = metaDataMapper.findByPath(metaDataDO.getPath());
        assertThat(result.getId(), comparesEqualTo(metaDataDO.getId()));
    }

    @Test
    public void findByServiceNameAndMethod() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        MetaDataDO result = metaDataMapper.findByServiceNameAndMethod(metaDataDO.getServiceName(), metaDataDO.getMethodName());
        assertThat(result.getId(), comparesEqualTo(metaDataDO.getId()));
    }

    @Test
    public void selectByQuery() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        MetaDataQuery metaDataQuery = new MetaDataQuery();
        metaDataQuery.setAppName(metaDataDO.getAppName());
        metaDataQuery.setPageParameter(new PageParameter());

        List<MetaDataDO> result = metaDataMapper.selectByQuery(metaDataQuery);
        assertThat(result.size(), comparesEqualTo(1));
        assertThat(result.get(0).getId(), comparesEqualTo(metaDataDO.getId()));
    }

    @Test
    public void selectAll() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        List<MetaDataDO> result = metaDataMapper.selectAll();
        assertThat(result.size(), comparesEqualTo(1));
        assertThat(result.get(0).getId(), comparesEqualTo(metaDataDO.getId()));
    }

    @Test
    public void countByQuery() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        MetaDataQuery metaDataQuery = new MetaDataQuery();
        metaDataQuery.setAppName(metaDataDO.getAppName());
        metaDataQuery.setPageParameter(new PageParameter());

        count = metaDataMapper.countByQuery(metaDataQuery);
        assertThat(count, comparesEqualTo(1));
    }

    @Test
    public void insert() {
        List<MetaDataDO> before = metaDataMapper.selectAll();
        assertThat(before.size(), comparesEqualTo(0));

        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        List<MetaDataDO> after = metaDataMapper.selectAll();
        assertThat(after.size(), comparesEqualTo(1));
        assertThat(after.get(0).getId(), comparesEqualTo(metaDataDO.getId()));
    }

    @Test
    public void update() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        metaDataDO.setAppName("testAppName_update");
        metaDataMapper.update(metaDataDO);

        MetaDataDO result = metaDataMapper.selectById(metaDataDO.getId());
        assertThat(result.getAppName(), comparesEqualTo("testAppName_update"));
    }

    @Test
    public void updateEnable() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        metaDataDO.setEnabled(true);
        metaDataMapper.updateEnable(metaDataDO);

        MetaDataDO result = metaDataMapper.selectById(metaDataDO.getId());
        assertTrue(result.getEnabled());
    }

    @Test
    public void delete() {
        MetaDataDO metaDataDO = getMetaDataDO();
        int count = metaDataMapper.insert(metaDataDO);
        assertThat(count, comparesEqualTo(1));

        int result = metaDataMapper.delete(metaDataDO.getId());
        assertThat(result, comparesEqualTo(1));
    }

    private MetaDataDO getMetaDataDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return MetaDataDO.builder()
                .appName("testAppName")
                .path("testPath")
                .pathDesc("testPathDesc")
                .rpcType("testRpcType")
                .serviceName("testServiceName")
                .methodName("testMethodName")
                .parameterTypes("testParameterTypes")
                .rpcExt("testRpcExt")
                .enabled(false)
                .id(UUIDUtils.getInstance().generateShortUuid())
                .dateUpdated(now)
                .dateCreated(now)
                .build();
    }

}
