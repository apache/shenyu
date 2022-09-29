package org.apache.shenyu.plugin.logging.mask.spi;

import org.apache.shenyu.spi.Join;

/**
 * character replaces data mask.
 */
@Join
public class CharacterReplaceDataMask extends AbstractShenyuDataMask {

    private static final Character MASK = '*';

    @Override
    protected String doMask(String source) {
        return maskData(source, source.length() / 2);
    }

    private String maskData(final String data, final int maskNum) {
        if (data.length() == 1) {
            return "*";
        }
        StringBuilder sb = new StringBuilder(data);
        int mid = data.length() / 2;
        int l = mid - 1;
        int r = mid;
        int currentMaskNum = 0;
        while (currentMaskNum < maskNum) {
            final int tempMaskNum = currentMaskNum;
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
