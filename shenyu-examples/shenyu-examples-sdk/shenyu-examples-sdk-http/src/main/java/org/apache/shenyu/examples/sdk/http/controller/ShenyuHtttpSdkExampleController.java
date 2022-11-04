package org.apache.shenyu.examples.sdk.http.controller;

import org.apache.shenyu.examples.sdk.http.api.ShenyuHttpClientApi;
import org.apache.shenyu.examples.sdk.http.dto.SdkTestDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ShenyuHtttpSdkExampleController {

    @Autowired
    private ShenyuHttpClientApi shenyuHttpClientApi;

    @GetMapping("sdk/hello")
    public String sdkHello(@RequestParam("name") String name) {
        return shenyuHttpClientApi.hello(name);
    }

    @GetMapping("sdk/findById")
    public SdkTestDto findById(@RequestParam("id") String id) {
        return shenyuHttpClientApi.findById(id);
    }
}
