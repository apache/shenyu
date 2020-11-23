package org.dromara.soul.test.sofa.service;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ImportResource;

/**
 * @author tydhot
 */
@SpringBootApplication
@ImportResource({ "classpath*:invoke-server-example.xml"})
public class TestSofaApplication {

    /**
     * Main Entrance.
     *
     * @param args startup arguments
     */
    public static void main(final String[] args) {
        SpringApplication.run(TestSofaApplication.class, args);
    }

}
