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
     * Module init.
     * @return instances.
     * @throws Throwable
     */
    List<Object> init() throws Throwable;

//
//    Configuration[] getConfigurations();
//
//    boolean modifyConfiguration(Configuration conf);
//
//    int order();
//
//    URL[] getResource();

}
