package org.apache.shenyu.plugin.divide.health;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.apache.shenyu.common.dto.convert.DivideUpstream;

@Data
@AllArgsConstructor
public class UpstreamWithSelectorId {

    private String selectorId;

    private DivideUpstream divideUpstream;
}
