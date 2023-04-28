package org.apache.shenyu.protocol.tcp.connection;

import io.netty.channel.ChannelFuture;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.netty.Connection;
import reactor.netty.ConnectionObserver;
import reactor.netty.channel.ChannelOperations;

import java.net.SocketAddress;
import java.util.ArrayList;
import java.util.List;

//todo 如果发送了 切换 upstream list  要吧 下游的链接 关闭
public class ActivityConnectionObserver implements ConnectionObserver {
    private static final Logger LOG = LoggerFactory.getLogger(ActivityConnectionObserver.class);

    List<Connection> connections = new ArrayList<>();
    private String name;

    public ActivityConnectionObserver(String name) {
        this.name = name;
    }

    @Override
    public void onStateChange(Connection connection, State newState) {
        if (newState == State.CONNECTED) {
            connections.add(connection);
            LOG.info("{} add connection into cache ={}", name, connection);
        } else if (newState == State.DISCONNECTING
                || newState == State.RELEASED
        ) {
            connections.remove(connection);
            LOG.info("{} remove connection into cache ={}", name, connection);
        }
    }

    //todo 换个思路从
    private void onRemove(List<Upstream> remove) {
        for (Connection connection : connections) {
            ChannelOperations op = ((ChannelOperations) connection);
            SocketAddress socketAddress = op.channel().remoteAddress();
            if(in(remove , socketAddress)){
                // 删除
                connection.disposeNow();
            }
        }

    }

    private boolean in(List<Upstream> remove , SocketAddress socketAddress){
        return false;
    }
}
