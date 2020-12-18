package org.dromara.soul.client.alibaba.dubbo.validation.mock;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE_USE;

/**
 * MockConstraint.
 *
 * @author David Liu
 */
@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Constraint(validatedBy = MockConstraintValidator.class)
public @interface MockConstraint {
    
    /**
     * mock message parameter.
     *
     * @return the mock value
     */
    String message() default "{message}";
    
    /**
     * mock class array parameter.
     *
     * @return the mock value
     */
    Class<?>[] groups() default {};
    
    /**
     * constraint required payload.
     *
     * @return the payload value
     */
    Class<? extends Payload>[] payload() default {};
    
    /**
     * mock byte parameter.
     *
     * @return the mock value
     */
    byte mpByte() default '1';
    
    /**
     * mock short parameter.
     *
     * @return the mock value
     */
    short mpShort() default 1;
    
    /**
     * mock char parameter.
     *
     * @return the mock value
     */
    char mpChar() default ' ';
    
    /**
     * mock int parameter.
     *
     * @return the mock value
     */
    int mpInteger() default 0;
    
    /**
     * mock class parameter.
     *
     * @return the mock value
     */
    Class<?> mpClass() default Object.class;
    
    /**
     * mock class parameter.
     *
     * @return the mock value
     */
    MockEnumType mpEnum() default MockEnumType.MEMBER;
    
    /**
     * mock boolean parameter.
     *
     * @return the mock value
     */
    boolean mpBool() default false;
    
    /**
     * mock long parameter.
     *
     * @return the mock value
     */
    long mpLong() default 0;
    
    /**
     * mock float parameter.
     *
     * @return the mock value
     */
    float mpFloat() default 0;
    
    /**
     * mock double parameter.
     *
     * @return the mock value
     */
    double mpDouble() default 0;
    
    /**
     * mock object array parameter.
     *
     * @return the mock value
     */
    Class<?>[] mpClassArray() default {Object.class};
}
