package org.apache.shenyu.plugin.logging.common.datamask;

import org.apache.shenyu.common.utils.Md5Utils;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class DataMaskByMD5 implements DataMaskInterface {

    @Override
    public String mask(String data) {

        if (!StringUtils.hasLength(data)) {
            return "";
        }
        return Md5Utils.md5(data);
    }
}
