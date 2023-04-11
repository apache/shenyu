package org.apache.shenyu.protocol.tcp.connection;

import reactor.netty.Connection;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ConnectionHolder {
    public static Map<String, List<Connection>> connectionCache = new ConcurrentHashMap<>();

    public List<Connection> getConnectionList(String key) {
        return connectionCache.get(key);
    }


    public void put(String key, Connection connections) {
        if (connectionCache.containsKey(key)) {
            List<Connection> list = connectionCache.get(key);
            if(!list.contains(connections)){
                list.add(connections);
            }
        }
        List<Connection> objects = new ArrayList<>();
        objects.add(connections);
        connectionCache.put(key, objects);
    }


}
