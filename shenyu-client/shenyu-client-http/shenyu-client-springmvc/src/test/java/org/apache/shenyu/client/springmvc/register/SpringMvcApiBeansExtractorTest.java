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

package org.apache.shenyu.client.springmvc.register;

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.junit.jupiter.api.Test;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class SpringMvcApiBeansExtractorTest {

    private final ApiBeansExtractor extractor = SpringMvcApiBeansExtractor.buildDefaultSpringMvcApiBeansExtractor();

    @Test
    public void testExtractBeans() throws NoSuchMethodException {

        GenericApplicationContext applicationContext = new GenericApplicationContext();
        applicationContext.refresh();
        applicationContext.registerBean(ExtractorTestBean.class);
        applicationContext.registerBean(ExtractorTestWithoutControllerBean.class);

        List<ApiBean> apiBeans = extractor.extract(applicationContext);

        assertThat(apiBeans.size(), is(1));
        ApiBean apiBean = apiBeans.get(0);
        assertThat(apiBean.getBeanClass(), is(ExtractorTestBean.class));
        assertThat(apiBean.getBeanPath(), is("/testBean"));
        assertThat(apiBean.getApiDefinitions().size(), is(1));

        ApiBean.ApiDefinition apiDefinition = apiBean.getApiDefinitions().get(0);
        assertThat(apiDefinition.getApiMethod(), is(ExtractorTestBean.class.getMethod("test")));
        assertThat(apiDefinition.getMethodPath(), is("/testMethod"));
    }

    @Controller
    @RequestMapping("/testBean")
    private static class ExtractorTestBean {
        @RequestMapping("/testMethod")
        public String test() {
            return "";
        }

        public String testWithoutRequestMapping() {
            return "";
        }

    }

    @Service
    private static class ExtractorTestWithoutControllerBean {
    }

}
