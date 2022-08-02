package org.apache.shenyu.examples.websocket.ws;


import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.examples.websocket.service.SaveFile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;

@ShenyuSpringWebSocketClient("/upload")
@ServerEndpoint("/upload")
@Controller
public class UpLoadController {

    @Autowired
    private SaveFile saveFile;

    @OnOpen
    public void onOpen(Session session) {
        System.out.println("connect1 successful");
    }

    /**
     * connect close.
     *
     * @param session
     */
    @OnClose
    public void onClose(Session session) {
        System.out.println("connect1 closed");
    }

    @OnMessage
    public void onMessage(String message, Session session) {
        try {
            session.getBasicRemote().sendText("ok");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @OnMessage
    public void onMessage(byte[] message, Session session) {
        System.out.println("i am upload");
        File file = (File) session.getUserProperties().get("file");
        try {
            HashMap map = new HashMap();
            map.put("file",file);
            saveFile.saveFileFromBytes(message,map);
            session.getBasicRemote().sendText("ok");
        }catch (Exception e){

        }
    }
}
