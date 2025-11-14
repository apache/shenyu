package org.apache.shenyu.plugin.base.provider;

import java.util.List;

public interface DataProvider<T> {
    List<T> getData(String key);
}