package org.apache.shenyu.examples.websocket.service;

import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;


@Service
public class SaveFileImpl implements SaveFile {
    @Override
    public boolean saveFileFromBytes(byte[] b, Map<String, Object> map) {
        FileOutputStream fstream = null;
        //从map中获取file对象
        File file = (File) map.get("file");
        //判断路径是否存在，不存在就创建
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }
        try {
            fstream = new FileOutputStream(file, true);
            fstream.write(b);
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        } finally {
            if (fstream != null) {
                try {
                    fstream.close();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
        }
        return true;
    }
}
