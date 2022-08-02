package org.apache.shenyu.examples.websocket.controller;


import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.examples.websocket.config.WsSessionManager;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

@RestController
@RequestMapping("/ws")
@ShenyuSpringWebSocketClient("/ws/**")
public class UpLoadController {

    @PostMapping(value = "/file")
    public Object file(String token,@RequestParam("file") final MultipartFile file) {
        try {
            WebSocketSession webSocketSession = WsSessionManager.get(token);
            if (webSocketSession == null) {
                return "User login has expired";
            }
            if (file.getOriginalFilename().endsWith(".bin")){
                webSocketSession.sendMessage(new BinaryMessage(file.getBytes()));
            }
        }catch (Exception e){

        }
        return file.getOriginalFilename();
    }
}
