package org.apache.shenyu.common.dto.convert.rule.impl;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Param mapping handle.
 */
@Data
@NoArgsConstructor
public class ParamMappingHandle implements RuleHandle {

    private String mediaType;

    private Set<String> deleteInfos;

    private List<ParamMapInfo> modityInfos;

    private List<ParamMapInfo> addInfos;

    @Override
    public RuleHandle createDefault(final String path) {
        return this;
    }

    @Data
    public class ParamMapInfo {

        private String path;

        private String key;

        private String value;
    }
}
