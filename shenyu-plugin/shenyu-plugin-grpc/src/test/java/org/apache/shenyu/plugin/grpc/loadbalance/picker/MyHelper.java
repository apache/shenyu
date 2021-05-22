package org.apache.shenyu.plugin.grpc.loadbalance.picker;

import io.grpc.Attributes;
import io.grpc.ConnectivityState;
import io.grpc.ConnectivityStateInfo;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import io.grpc.ManagedChannel;
import io.grpc.NameResolver;
import lombok.SneakyThrows;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannels;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;
import java.net.SocketAddress;
import java.util.Collections;
import java.util.List;

public class MyHelper extends LoadBalancer.Helper {
    @Override
    public ManagedChannel createOobChannel(EquivalentAddressGroup eag, String authority) {
        return null;
    }

    @Override
    public void updateBalancingState(@Nonnull ConnectivityState newState, @Nonnull LoadBalancer.SubchannelPicker newPicker) {

    }

    @Override
    public NameResolver.Factory getNameResolverFactory() {
        return null;
    }

    @Override
    public String getAuthority() {
        return null;
    }


    @Override
    public LoadBalancer.Subchannel createSubchannel(LoadBalancer.CreateSubchannelArgs args) {
        return new LoadBalancer.Subchannel() {
            @Override
            public void start(LoadBalancer.SubchannelStateListener listener) {
                SubChannels.setStateInfo(this, ConnectivityStateInfo.forNonError(ConnectivityState.READY));
            }

            @Override
            public void shutdown() {

            }

            @Override
            public void requestConnection() {

            }

            @SneakyThrows
            @Override
            public Attributes getAttributes() {

//                setStateInfo(final LoadBalancer.Subchannel subchannel, final ConnectivityStateInfo value)
//                SubChannels subChannels = new SubChannels();
//                Field field = subChannels.getClass().getDeclaredField("STATE_INFO_KEY");
//                field.get(subChannels);
                return args.getAttributes();
            }

            @Override
            public List<EquivalentAddressGroup> getAllAddresses() {
                return Collections.singletonList(new EquivalentAddressGroup(new SocketAddress(){}));
            }
        };
    }
}
