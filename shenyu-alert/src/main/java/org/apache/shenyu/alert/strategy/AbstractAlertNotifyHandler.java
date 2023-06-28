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

package org.apache.shenyu.alert.strategy;

import org.apache.shenyu.alert.AlertNotifyHandler;
import org.apache.shenyu.alert.model.AlertContentDTO;
import org.springframework.web.client.RestTemplate;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.Context;

import javax.annotation.Resource;
import java.time.format.DateTimeFormatter;

/**
 * abstract alert notify 
 */
abstract class AbstractAlertNotifyHandler implements AlertNotifyHandler {
    protected static final DateTimeFormatter DTF = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    @Resource
    protected TemplateEngine templateEngine;

    @Resource
    protected RestTemplate restTemplate;

    protected String renderContent(AlertContentDTO alert) {
        Context context = new Context();
        context.setVariable("title", "[ShenYu Alarm]");

        context.setVariable("triggerTimeLabel", "Alarm Time");
        context.setVariable("triggerTime", DTF.format(alert.getDateCreated().toInstant()));

        context.setVariable("contentLabel", "Alarm Content");
        context.setVariable("content", alert.getContent());

        return removeBlankLine(templateEngine.process(templateName(), context));
    }

    /**
     * Get the Thymeleaf template name
     * 获取Thymeleaf模板名称
     *
     * @return Thymeleaf模板名称
     */
    protected abstract String templateName();
	
	private static String removeBlankLine(String value) {
		if (value == null) {
			return null;
		}
		return value.replaceAll("(?m)^\\s*$(\\n|\\r\\n)", "");
	}

}
