package org.dromara.soul.client.apache.dubbo.validation.service;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import javax.validation.constraints.NotNull;

/**
 * TestService.
 *
 * @author KevinClair
 */
public interface TestService {

    String test(TestObject testObject);

    @Data
    @Builder
    @EqualsAndHashCode
    class TestObject {
        @NotNull(message = "age cannot be null.")
        private Integer age;
    }
}
