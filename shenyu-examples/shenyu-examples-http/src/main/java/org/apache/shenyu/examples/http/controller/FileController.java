package org.apache.shenyu.examples.http.controller;

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.net.URLEncoder;

/**
 * FileController
 */

@RestController
@RequestMapping("/file")
@ShenyuSpringMvcClient(path="/file/**")
public class FileController {
    Logger logger= LoggerFactory.getLogger(FileController.class);
    /**
     * Post fileUploadWay1.
     *
     * @param file the file
     * @param filePath the filePath
     * @return the String
     * @throws Exception the Exception
     */
    @RequestMapping("/uploadWay1")
    public String fileUploadWay1(@RequestParam("file") MultipartFile file,@RequestParam("filePath") String filePath) throws Exception {

        String fileName=file.getOriginalFilename();
        FileOutputStream out;
        try {
            out = new FileOutputStream(filePath + fileName);
            out.write(file.getBytes());
            out.flush();
        } catch (Exception e) {
            return "上传失败";
        }
        return "上传成功";
    }

    /**
     * Post fileUploadWay2.
     *
     * @param file the file
     * @param filePath the filePath
     * @param fileName the fileName
     * @return the String
     * @throws Exception the Exception
     */
    @RequestMapping("/uploadWay2")
    public String fileUploadWay2(@RequestParam("file") byte[] file,@RequestParam("filePath") String filePath,@RequestParam("fileName") String fileName) throws Exception {

        FileOutputStream out ;
        try {
            out= new FileOutputStream(filePath + fileName);
            out.write(file);
            out.flush();
        } catch (Exception e) {
            return "上传失败";
        }
        return "上传成功";
    }

    /**
     * Get fileDownload.
     *
     * @param filePath the filePath
     * @return the String
     * @throws Exception the Exception
     */
    @RequestMapping("/download")
    public String fileDownload(String filePath,HttpServletResponse response) throws Exception {

        File file=new File(filePath);
        String  fileName= file.getName();
        response.setContentType("application/force-download");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Content-Disposition", "attachment;fileName=" + URLEncoder.encode(fileName,"UTF-8"));
        try {
        FileInputStream fis;
        BufferedInputStream bis;
        OutputStream os;
        byte[] buf=new byte[1024];

            fis=new FileInputStream(file);
            bis=new BufferedInputStream(fis);
            os=response.getOutputStream();
            int len=bis.read(buf);
            while(len!= -1){
                os.write(buf, 0, len);
                len=bis.read(buf);
            }
        }catch (Exception e){
            return "下载失败";
        }
        return null;
    }
}
