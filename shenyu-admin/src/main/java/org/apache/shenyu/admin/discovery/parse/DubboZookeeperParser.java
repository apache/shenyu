package org.apache.shenyu.admin.discovery.parse;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;

import java.util.List;


/**
 * todo
 * path ==>  /dubbo/{interface}/providers
 *            222.这样的数据只有一个 proxySelector  更加的虚拟化
 *            data =[dubbo://192.168.99.1:20880/com.alibaba.dubbo.samples.api.GreetingService?anyhost=true&application=demo-provider&dubbo=2.6.2&generic=false&interface=com.alibaba.dubbo.samples.api.GreetingService&methods=sayHello&pid=12938&side=provider&timestamp=1533264631849]
 */
public class DubboZookeeperParser implements keyValueParser {

    @Override
    public List<DiscoveryUpstreamData> parseValue(String value) {
        return null;
    }

    @Override
    public ProxySelectorData parseKey(String key) {
        return null;
    }
}
