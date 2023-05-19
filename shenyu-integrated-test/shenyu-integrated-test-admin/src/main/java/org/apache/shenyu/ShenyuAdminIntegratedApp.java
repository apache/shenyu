package org.apache.shenyu;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class ShenyuAdminIntegratedApp {


    /**
     * Main entrance.
     *
     * @param args startup arguments
     */
    public static void main(final String[] args) {
        SpringApplication.run(ShenyuAdminIntegratedApp.class, args);
    }

}
