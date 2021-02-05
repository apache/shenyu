package org.dromara.soul.client.tars.common.dto;

import java.util.List;
import javafx.util.Pair;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class TarsRpcExt {
    
    private List<RpcExt> methodInfo;
    
    /**
     * The type Rpc ext.
     */
    @Data
    @Builder
    public static class RpcExt {
        
        private String methodName;
        
        private List<Pair<String, String>> params;
        
        private String returnType;
    }
    
}
