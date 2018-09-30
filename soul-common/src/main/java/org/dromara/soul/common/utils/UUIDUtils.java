package org.dromara.soul.common.utils;

import java.util.UUID;

/**
 * UUIDUtils.
 *
 * @author jiangxiaofeng(Nicholas)
 */
public class UUIDUtils {

    private static String[] chars = new String[]{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
            "k", "l", "m", "n", "o", "p", "q", "r", "s",
            "t", "u", "v", "w", "x", "y", "z", "0", "1", "2",
            "3", "4", "5", "6", "7", "8", "9", "A", "B",
            "C", "D", "E", "F", "G", "H", "I",
            "J", "K", "L", "M", "N", "O", "P", "Q", "R",
            "S", "T", "U", "V", "W", "X", "Y", "Z"};

    private static final int COUNT = 8;

    /**
     * generate short uuid.
     *
     * @return short uuid.
     */
    public static String generateShortUuid() {
        StringBuilder stringBuffer = new StringBuilder();
        String uuid = UUID.randomUUID().toString().replace("-", "");
        for (int i = 0; i < COUNT; i++) {
            String str = uuid.substring(i * 4, i * 4 + 4);
            int index = Integer.parseInt(str, 16);
            stringBuffer.append(chars[index % 0x3E]);
        }
        return stringBuffer.toString();
    }
}
