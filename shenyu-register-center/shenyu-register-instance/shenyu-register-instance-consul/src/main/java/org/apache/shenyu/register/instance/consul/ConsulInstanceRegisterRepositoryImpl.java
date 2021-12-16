package org.apache.shenyu.register.instance.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.agent.model.NewService;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Properties;

@Join
public class ConsulInstanceRegisterRepositoryImpl implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulInstanceRegisterRepositoryImpl.class);

    private static final char SEPARATOR = '-';

    private ConsulClient consulClient;

    private NewService service;

    @Override
    public void init(ShenyuConfig.InstanceConfig config) {
        final Properties properties = config.getProps();
        consulClient = new ConsulClient(config.getServerLists());
        service = new NewService();
        service.setMeta(new HashMap<>());

        final String appName = properties.getProperty("name");
        service.setName(normalizeForDns(appName));
        final String instanceId = properties.getProperty("instanceId");
        service.setId(normalizeForDns(instanceId));
        final boolean preferAgentAddress = Boolean.parseBoolean(properties.getProperty("preferAgentAddress", "false"));
        if (!preferAgentAddress) {
            service.setAddress(properties.getProperty("hostName"));
        }
        final String tags = properties.getProperty("tags");
        if (StringUtils.isNotBlank(tags)) {
            service.setTags(new ArrayList<>(Arrays.asList(tags.split(","))));
        }
        service.setEnableTagOverride(Boolean.valueOf(properties.getProperty("enableTagOverride", "false")));

        final String port = properties.getProperty("port");
        if (StringUtils.isNotBlank(port)) {
            service.setPort(Integer.parseInt(port));
        }

        consulClient.agentServiceRegister(this.service);
    }

    @Override
    public void persistInstance(InstanceRegisterDTO instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        String instancePath = RegisterPathConstants.buildInstanceParentPath();
        String realNode = RegisterPathConstants.buildRealNode(instancePath, instanceNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        consulClient.setKVValue(realNode, nodeData);

        LOGGER.info("consul client register success: {}", nodeData);
    }

    @Override
    public void close() {

    }

    private String buildInstanceNodeName(final InstanceRegisterDTO instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    private String normalizeForDns(final String s) {
        if (s == null || !Character.isLetter(s.charAt(0))
                || !Character.isLetterOrDigit(s.charAt(s.length() - 1))) {
            throw new IllegalArgumentException(
                    "Consul service ids must not be empty, must start "
                            + "with a letter, end with a letter or digit, "
                            + "and have as interior characters only letters, "
                            + "digits, and hyphen: " + s);
        }

        StringBuilder normalized = new StringBuilder();
        Character prev = null;
        for (char curr : s.toCharArray()) {
            Character toAppend = null;
            if (Character.isLetterOrDigit(curr)) {
                toAppend = curr;
            } else if (prev == null || !(prev == SEPARATOR)) {
                toAppend = SEPARATOR;
            }
            if (toAppend != null) {
                normalized.append(toAppend);
                prev = toAppend;
            }
        }

        return normalized.toString();
    }
}

