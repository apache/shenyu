package org.apache.shenyu.isolation;

import java.net.URLClassLoader;
import java.util.List;

public interface Module {

    String name();

    /**
     * Set classLoader.
     * @param classLoader
     */
    void setClassLoader(URLClassLoader classLoader);


    /**
     * Get register classNames.
     * @return RegisterClassNames
     */
    List<String> getRegisterClassNames();

//
//    Configuration[] getConfigurations();
//
//    boolean modifyConfiguration(Configuration conf);
//
//    int order();
//
//    URL[] getResource();

}
