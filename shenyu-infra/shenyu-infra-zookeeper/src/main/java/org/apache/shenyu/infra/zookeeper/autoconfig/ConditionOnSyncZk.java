package org.apache.shenyu.infra.zookeeper.autoconfig;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static org.apache.shenyu.infra.zookeeper.autoconfig.ZookeeperProperties.CONFIG_PREFIX;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@ConditionalOnProperty(prefix = CONFIG_PREFIX, name = "url")
public @interface ConditionOnSyncZk {
}
