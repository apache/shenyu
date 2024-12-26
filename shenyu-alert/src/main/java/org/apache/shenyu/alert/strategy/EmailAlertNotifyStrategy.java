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
import org.apache.shenyu.alert.exception.AlertNoticeException;
import org.apache.shenyu.common.dto.AlarmContent;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.Context;

import jakarta.mail.internet.MimeMessage;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

/**
 * email alert notice.
 */
@Component
final class EmailAlertNotifyStrategy implements AlertNotifyHandler {
    
    private final JavaMailSender javaMailSender;
    
    private final TemplateEngine templateEngine;
    
    @Value("${spring.mail.username}")
    private String emailFromUser;
    
    EmailAlertNotifyStrategy(final TemplateEngine templateEngine, final JavaMailSender javaMailSender) {
        this.javaMailSender = javaMailSender;
        this.templateEngine = templateEngine;
    }
    
    @Override
    public void send(final AlertReceiverDTO receiver, final AlarmContent alert) throws AlertNoticeException {
        try {
            MimeMessage mimeMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper messageHelper = new MimeMessageHelper(mimeMessage, true, "UTF-8");
            messageHelper.setSubject("ShenYu Alarm");
            //Set sender Email 设置发件人Email
            messageHelper.setFrom(emailFromUser);
            //Set recipient Email 设定收件人Email
            messageHelper.setTo(receiver.getEmail());
            messageHelper.setSentDate(new Date());
            //Build email templates 构建邮件模版
            String process = buildAlertHtmlTemplate(alert);
            //Set Email Content Template 设置邮件内容模版
            messageHelper.setText(process, true);
            javaMailSender.send(mimeMessage);
        } catch (Exception e) {
            throw new AlertNoticeException("[Email Notify Error] " + e.getMessage());
        }
    }
    
    private String buildAlertHtmlTemplate(final AlarmContent alert) {
        // Introduce thymeleaf context parameters to render pages
        Context context = new Context();
        context.setVariable("nameTitle", "ShenYu Alarm");
        context.setVariable("nameTriggerTime", "Alarm Time");
        context.setVariable("nameContent", "Alarm Content");
        context.setVariable("content", alert.getContent());
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date alertTime = alert.getDateCreated();
        if (Objects.isNull(alert)) {
            alertTime = new Date();
        }
        String alarmTime = simpleDateFormat.format(alertTime);
        context.setVariable("lastTriggerTime", alarmTime);
        return templateEngine.process("mailAlarm", context);
    }
    
    @Override
    public byte type() {
        return 1;
    }
}
