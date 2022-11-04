package org.apache.shenyu.examples.sdk.http.controller;

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.examples.sdk.http.dto.SdkTestDto;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * HttpServiceController.
 */
@RestController
public class HttpServiceController {
    private static final String HELLO_SUFFIX = "I'm Shenyu-Gateway System. Welcome!";

    /**
     * hello. br
     * no support gateway.
     *
     * @param name say hello user name
     * @return result
     */
    @GetMapping("shenyu/client/hello")
    @ShenyuSpringMvcClient("shenyu/client/hello")
    public String hello(final @RequestParam("name") String name) {
        return "hi! " + name + "! " + HELLO_SUFFIX;
    }

    @GetMapping("shenyu/client/findById")
    @ShenyuSpringMvcClient("shenyu/client/findById")
    public SdkTestDto findById(final @RequestParam("id") String id) {
        SdkTestDto sdkTestDto = new SdkTestDto();
        sdkTestDto.setId(id);
        sdkTestDto.setName("sdk");
        return sdkTestDto;
    }

}
