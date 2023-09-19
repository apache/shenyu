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
     * @throws Throwable
     */
    void init() throws Throwable;

    List<String> getRegisterClassNames();

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
