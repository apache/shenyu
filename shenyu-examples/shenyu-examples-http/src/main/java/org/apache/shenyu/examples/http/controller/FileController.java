package org.apache.shenyu.examples.http.controller;

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;


@RestController
@RequestMapping("/file")
@ShenyuSpringMvcClient(path = "/file/**")
public class FileController {
    @PostMapping("/upload")
    public String fileUpload(@RequestParam("file")MultipartFile file, String filePath){
        String fileName = file.getOriginalFilename();
        File newFile = new File(filePath + fileName);
        try {
            file.transferTo(newFile);
            return "上传成功";
        } catch (IOException e) {
        }
        return "上传失败！";
    }
}

