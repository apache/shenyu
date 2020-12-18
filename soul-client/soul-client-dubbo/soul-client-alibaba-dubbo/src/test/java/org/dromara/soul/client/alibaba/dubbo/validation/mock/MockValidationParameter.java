package org.dromara.soul.client.alibaba.dubbo.validation.mock;

import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * Mock ValidationParameter.
 *
 * @author David Liu
 */
@Data
public class MockValidationParameter {
    /**
     * String parameter.
     */
    @NotNull
    private String stringParameter;
    
    public MockValidationParameter() {
    }
    
    /**
     * constructor with parameter.
     *
     * @param stringParameter the String parameter.
     */
    public MockValidationParameter(final String stringParameter) {
        this.stringParameter = stringParameter;
    }
}
