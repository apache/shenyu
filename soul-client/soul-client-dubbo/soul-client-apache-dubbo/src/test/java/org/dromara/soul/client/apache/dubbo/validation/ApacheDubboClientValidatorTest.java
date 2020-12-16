package org.dromara.soul.client.apache.dubbo.validation;

import org.apache.dubbo.common.URL;
import org.dromara.soul.client.apache.dubbo.validation.service.TestService;
import org.dromara.soul.common.enums.PluginEnum;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * ApacheDubboClientValidatorTest.
 *
 * @author KevinClair
 */
public class ApacheDubboClientValidatorTest {

    /**
     * test method {@link ApacheDubboClientValidator#validate(java.lang.String, java.lang.Class[], java.lang.Object[])}.
     */
    @Test
    public void validate() {
        URL url = URL.valueOf("dubbo://127.0.0.1:20880/org.dromara.soul.client.apache.dubbo.validation.service.TestService?accepts=500&anyhost=true&application=soul-proxy&bind.ip=127.0.0.1&bind.port=20880&deprecated=false&dubbo=2.0.2&dynamic=true&generic=false&interface=org.dromara.soul.client.apache.dubbo.validation.service.TestService&keep.alive=true&methods=test&pid=67352&qos.enable=false&release=2.7.0&side=provider&threadpool=fixed&threads=500&timeout=20000&timestamp=1608119259859&validation=soulValidation");
        ApacheDubboClientValidator apacheDubboClientValidator = new ApacheDubboClientValidator(url);
        try {
            apacheDubboClientValidator.validate("test", new Class[]{TestService.TestObject.class}, new Object[]{TestService.TestObject.builder().age(null).build()});
        } catch (Exception e) {
            assertEquals("age cannot be null.", e.getMessage());
        }
    }
}
