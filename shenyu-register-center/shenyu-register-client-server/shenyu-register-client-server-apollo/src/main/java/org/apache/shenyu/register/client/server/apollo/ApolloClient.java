package org.apache.shenyu.register.client.server.apollo;

import com.ctrip.framework.apollo.openapi.client.ApolloOpenApiClient;
import com.ctrip.framework.apollo.openapi.dto.NamespaceReleaseDTO;
import com.ctrip.framework.apollo.openapi.dto.OpenItemDTO;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Date;

public class ApolloClient {

    private final ApolloConfig apolloConfig;

    private final ApolloOpenApiClient apolloOpenApiClient;

    private static final String DEFAULT_USER = "apollo";

    public ApolloClient(ApolloConfig apolloConfig) {
        this.apolloConfig = apolloConfig;

        this.apolloOpenApiClient = ApolloOpenApiClient
                .newBuilder()
                .withPortalUrl(apolloConfig.getPortalUrl())
                .withToken(apolloConfig.getToken())
                .build();
    }

    public String getItemValue(String key) {
        OpenItemDTO openItemDTO = this.apolloOpenApiClient.getItem(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                key
        );
        // no such key
        if (openItemDTO == null) {
            return null;
        }
        // todo handle timeout
        if ("timeout".equals(openItemDTO.getKey())) {
            return null;
        }

        return openItemDTO.getValue();
    }

    public void createItem(String key, Object value, String comment) {
        this.createItem(key, GsonUtils.getInstance().toJson(value), comment);
    }

    public void createItem(String key, String value, String comment) {
        OpenItemDTO openItemDTO = new OpenItemDTO();
        openItemDTO.setKey(key);
        openItemDTO.setValue(value);
        openItemDTO.setComment(comment);
        openItemDTO.setDataChangeCreatedBy(DEFAULT_USER);
        openItemDTO.setDataChangeLastModifiedBy(DEFAULT_USER);
        Date now = new Date();
        openItemDTO.setDataChangeCreatedTime(now);
        openItemDTO.setDataChangeLastModifiedTime(now);

        this.apolloOpenApiClient.createItem(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                openItemDTO
        );
    }

    public void publishNamespace(String releaseTitle, String releaseComment) {
        NamespaceReleaseDTO namespaceReleaseDTO = new NamespaceReleaseDTO();
        namespaceReleaseDTO.setReleaseTitle(releaseTitle);
        namespaceReleaseDTO.setReleaseComment(releaseComment);
        namespaceReleaseDTO.setReleasedBy(DEFAULT_USER);

        this.apolloOpenApiClient.publishNamespace(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                namespaceReleaseDTO
        );
    }
}
