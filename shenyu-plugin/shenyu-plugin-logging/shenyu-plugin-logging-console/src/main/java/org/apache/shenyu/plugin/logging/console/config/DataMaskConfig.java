package org.apache.shenyu.plugin.logging.console.config;

import org.apache.shenyu.plugin.logging.common.datamask.DataMaskByCharReplace;
import org.apache.shenyu.plugin.logging.common.datamask.DataMaskByMD5;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class DataMaskConfig {

    @Bean
    public DataMaskByCharReplace dataMaskByCharReplace(){
        return new DataMaskByCharReplace();
    }

    @Bean
    public DataMaskByMD5 dataMaskByMD5(){
        return new DataMaskByMD5();
    }
}
