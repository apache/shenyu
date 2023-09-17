package org.apache.shenyu.isolation;

import java.net.URLClassLoader;

public interface Module {

    String name();

    /**
     * Set classLoader.
     * @param classLoader
     */
    void setClassLoader(URLClassLoader classLoader);

    /**
     * Module init.
     * @throws Throwable
     */
    void init() throws Throwable;

    //    String getPath();
//
//    Configuration[] getConfigurations();
//
//    boolean modifyConfiguration(Configuration conf);
//
//    int order();
//
//    URL[] getResource();

}
