package org.apache.shenyu.isolation;

import jdk.jfr.Configuration;

import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;

public interface Module {

    String getPath();

    String name();
//
//    Configuration[] getConfigurations();
//
//    boolean modifyConfiguration(Configuration conf);
//
//    int order();
//
//    URL[] getResource();
//
//    void start() throws Throwable;
}
