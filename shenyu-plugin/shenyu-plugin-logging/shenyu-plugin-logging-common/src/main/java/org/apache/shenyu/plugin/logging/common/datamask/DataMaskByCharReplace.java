package org.apache.shenyu.plugin.logging.common.datamask;

import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class DataMaskByCharReplace implements DataMaskInterface {

    private static final Character MASK = '*';

    @Override
    public String mask(final String data) {

        if (!StringUtils.hasLength(data)) {
            return "";
        }
        return doMask(data, data.length() / 2);
    }

    private String doMask(final String data, final int maskNum) {

        if(data.length()==1){
            return "*";
        }
        StringBuilder sb = new StringBuilder(data);
        int mid = data.length() / 2;
        int l = mid - 1;
        int r = mid;
        int currentMaskNum = 0;
        while (currentMaskNum < maskNum) {
            int tempMaskNum = currentMaskNum;
            if (r < data.length()) {
                sb.setCharAt(r, MASK);
                r++;
                currentMaskNum++;
            }
            if (currentMaskNum >= maskNum) {
                break;
            }
            if (l >= 0) {
                sb.setCharAt(l, MASK);
                l--;
                currentMaskNum++;
            }
            if (tempMaskNum == currentMaskNum) {
                break;
            }
        }
        return sb.toString();
    }
}
