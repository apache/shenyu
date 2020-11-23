package org.dromara.soul.client.sofa.common.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The interface Soul client.
 *
 * @author tydhot
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface SoulSofaClient {
    /**
     * Path string.
     *
     * @return the string
     */
    String path();

    /**
     * Rule name string.
     *
     * @return the string
     */
    String ruleName() default "";

    /**
     * Desc string.
     *
     * @return String string
     */
    String desc() default "";

    /**
     * Enabled boolean.
     *
     * @return the boolean
     */
    boolean enabled() default true;
}
