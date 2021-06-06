package org.apache.shenyu.common.dto.convert.rule.impl;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.*;

/**
 * Param mapping handle.
 */
@Data
@NoArgsConstructor
public class ParamMappingHandle implements RuleHandle {

    private Set<String> removeParameterKeys;

    private List<ParamMapInfo> replaceParameterKeys;

    private List<ParamMapInfo> addParameterKeys;

    @Override
    public RuleHandle createDefault(final String path) {
        return this;
    }

    @Data
    public static class ParamMapInfo {

        private String path;

        private String key;

        private String value;
    }
}
