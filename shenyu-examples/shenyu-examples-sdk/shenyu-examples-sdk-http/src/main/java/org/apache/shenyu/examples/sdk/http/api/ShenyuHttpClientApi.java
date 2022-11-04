package org.apache.shenyu.examples.sdk.http.api;

import org.apache.shenyu.examples.sdk.http.dto.SdkTestDto;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@ShenyuClient(contextId = "shenyu-gateway", name = "ShenyuSdkApiName")
public interface ShenyuHttpClientApi {

    /**
     * hello.
     *
     * @param name name
     * @return helloMsg
     */
    @GetMapping("/http/shenyu/client/hello")
    String hello(final @RequestParam("name") String name);


    @GetMapping("/http/shenyu/client/findById")
    SdkTestDto findById(final @RequestParam("id") String id);

}
