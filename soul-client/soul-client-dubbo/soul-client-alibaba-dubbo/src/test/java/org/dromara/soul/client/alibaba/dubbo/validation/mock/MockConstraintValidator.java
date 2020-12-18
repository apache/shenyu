package org.dromara.soul.client.alibaba.dubbo.validation.mock;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

/**
 * MockConstraintValidator.
 *
 * @author David Liu
 */
public class MockConstraintValidator implements ConstraintValidator<MockConstraint, Object> {
    
    @Override
    public boolean isValid(final Object value, final ConstraintValidatorContext context) {
        // mock return
        return false;
    }
}
