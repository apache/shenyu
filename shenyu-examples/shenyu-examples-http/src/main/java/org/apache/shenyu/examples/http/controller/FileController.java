package org.apache.shenyu.examples.http.controller;

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.*;

/**
 * FileUploadController
 */

@RestController
@RequestMapping("/file")
@ShenyuSpringMvcClient(path="/file/**")
public class FileController {
    @RequestMapping("/uploadWay1")
    public String fileUploadWay1(@RequestParam("file") MultipartFile file,@RequestParam("filePath") String filePath) throws Exception {
        File folder=new File(filePath);
        if(!folder.exists()){
            folder.mkdirs();
        }
        String fileName=file.getOriginalFilename();

        FileOutputStream out = new FileOutputStream(filePath + fileName);
        try {
            out.write(file.getBytes());
            out.flush();
        } catch (Exception e) {
            return "上传失败";
        } finally {
            out.close();
        }
        return "上传成功";
    }
    @RequestMapping("/uploadWay2")
    public String fileUploadWay2(@RequestParam("file") byte[] file,@RequestParam("filePath") String filePath,@RequestParam("fileName") String fileName) throws Exception {
        File folder=new File(filePath);
        if(!folder.exists()){
            folder.mkdirs();
        }

        FileOutputStream out = new FileOutputStream(filePath + fileName);
        try {
            out.write(file);
            out.flush();
        } catch (Exception e) {
            return "上传失败";
        } finally {
            out.close();
        }
        return "上传成功";
    }

    @RequestMapping("/download")
    public String fileDownload(String path, HttpServletResponse response) throws Exception {

        File file=new File(path);
        FileInputStream fis = null;
        BufferedInputStream bis=null;
        String  fileName= file.getName();
        response.setContentType("application/force-download");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Content-Disposition", "attachment;fileName=" + java.net.URLEncoder.encode(fileName,"UTF-8"));

        byte[] buf=new byte[1024];
        int len=0;
        try {
            fis=new FileInputStream(file);
            bis=new BufferedInputStream(fis);
            OutputStream os=response.getOutputStream();
            while((len = bis.read(buf)) != -1) {
                os.write(buf, 0, len);
            }
        }catch (Exception e){
            return "下载失败";
        }finally {
            fis.close();
        }
        return "下载成功";
    }

}
