package org.apache.shenyu.examples.websocket.service;

import java.util.Map;

public interface SaveFile {

    boolean saveFileFromBytes(byte[] b, Map<String, Object> map);
}
