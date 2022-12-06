package org.apache.shenyu.client.apidocs.core.init;

import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * @author zhengpeng
 * @date 2022/12/6 10:37 上午
 **/
public class ShenyuClientApiDocsListener implements ApplicationListener<ContextRefreshedEvent> {


    //TODO 构造函数




    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {

        //TODO 处理API-DOC
    }
}
