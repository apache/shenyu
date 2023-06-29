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

import org.apache.shenyu.alert.exception.AlertNoticeException;
import org.apache.shenyu.alert.model.AlertContentDTO;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.stereotype.Service;

/**
 * Send alarm information through DingTalk robot
 */
@Service
final class DingTalkRobotAlertNotifyStrategy extends AbstractAlertNotifyHandler {
	
	private static final String dingTalkWebHookUrl = "https://oapi.dingtalk.com/robot/send?access_token=";
	private static final Logger log = LoggerFactory.getLogger(DingTalkRobotAlertNotifyStrategy.class);
	
    @Override
    public void send(AlertReceiverDTO receiver, AlertContentDTO alert) {
        try {
            DingTalkWebHookDto dingTalkWebHookDto = new DingTalkWebHookDto();
            MarkdownDTO markdownDTO = new MarkdownDTO();
            markdownDTO.setText(renderContent(alert));
            markdownDTO.setTitle(alert.getTitle());
            dingTalkWebHookDto.setMarkdown(markdownDTO);
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            HttpEntity<DingTalkWebHookDto> httpEntity = new HttpEntity<>(dingTalkWebHookDto, headers);
            String webHookUrl = dingTalkWebHookUrl + receiver.getAccessToken();
            ResponseEntity<CommonRobotNotifyResp> responseEntity = restTemplate.postForEntity(webHookUrl,
                    httpEntity, CommonRobotNotifyResp.class);
            if (responseEntity.getStatusCode() == HttpStatus.OK) {
                assert responseEntity.getBody() != null;
                if (responseEntity.getBody().getErrCode() == 0) {
                    log.debug("Send dingTalk webHook: {} Success", webHookUrl);
                } else {
                    log.warn("Send dingTalk webHook: {} Failed: {}", webHookUrl, responseEntity.getBody().getErrMsg());
                    throw new AlertNoticeException(responseEntity.getBody().getErrMsg());
                }
            } else {
                log.warn("Send dingTalk webHook: {} Failed: {}", webHookUrl, responseEntity.getBody());
                throw new AlertNoticeException("Http StatusCode " + responseEntity.getStatusCode());
            }
        } catch (Exception e) {
            throw new AlertNoticeException("[DingTalk Notify Error] " + e.getMessage());
        }
    }

    @Override
    public byte type() {
        return 5;
    }

    @Override
    protected String templateName() {
        return "alertNotifyDingTalkRobot";
    }

    /**
     * 钉钉机器人请求消息体
     *
     * @author 花城
     * @version 1.0
     *
     */
    private static class DingTalkWebHookDto {
        private static final String MARKDOWN = "markdown";

        /**
         * 消息类型
         */
        private String msgtype = MARKDOWN;

        /**
         * markdown消息
         */
        private MarkdownDTO markdown;
	    
	    public String getMsgtype() {
		    return msgtype;
	    }
	    
	    public void setMsgtype(String msgtype) {
		    this.msgtype = msgtype;
	    }
	    
	    public MarkdownDTO getMarkdown() {
		    return markdown;
	    }
	    
	    public void setMarkdown(MarkdownDTO markdown) {
		    this.markdown = markdown;
	    }
    }
	
    private static class MarkdownDTO {
        /**
         * 消息内容
         */
        private String text;
        /**
         * 消息标题
         */
        private String title;
	    
	    public String getText() {
		    return text;
	    }
	    
	    public void setText(String text) {
		    this.text = text;
	    }
	    
	    public String getTitle() {
		    return title;
	    }
	    
	    public void setTitle(String title) {
		    this.title = title;
	    }
    }

}
