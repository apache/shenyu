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

import org.apache.shenyu.alert.EmailProp;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.spi.Join;

import javax.mail.Authenticator;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.MessagingException;
import javax.mail.Message;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.util.Properties;

/**
 * email strategy.
 */
@Join
public class EmailStrategy implements AlertStrategy {

    @Override
    public void execute(final String handle) throws Exception {
        EmailProp email = GsonUtils.getInstance().fromJson(handle, EmailProp.class);
        Properties props = props(email);

        Session session = session(email, props);

        MimeMessage message = message(email, session);

        Transport.send(message);

    }

    /**
     * create configuration properties.
     *
     * @param emailProp email properties.
     * @return email properties
     */
    private Properties props(final EmailProp emailProp) {
        Properties props = new Properties();
        props.put("mail.smtp.host", emailProp.getHost());
        props.put("mail.smtp.port", emailProp.getPort());
        props.put("mail.smtp.auth", emailProp.getAuth());
        props.put("mail.smtp.starttls.enable", emailProp.getEnableTls());

        return props;
    }

    /**
     * create session.
     * @param emailProp emailProp
     * @param props props
     * @return Session
     */
    private Session session(final EmailProp emailProp, final Properties props) {
        return Session.getInstance(props, new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(emailProp.getUserName(), emailProp.getPassword());
            }
        });
    }

    /**
     * create message.
     * @param prop prop
     * @param session session
     * @return MimeMessage
     * @throws MessagingException MessagingException
     */
    private MimeMessage message(final EmailProp prop, final Session session) throws MessagingException {
        MimeMessage message = new MimeMessage(session);
        message.setFrom(new InternetAddress(prop.getFrom()));
        message.setRecipient(Message.RecipientType.TO, new InternetAddress(prop.getTo()));
        message.setSubject(prop.getSubject(), "UTF-8");
        message.setText(prop.getText(), "UTF-8");
        return message;
    }
}
