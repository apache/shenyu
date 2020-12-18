package org.dromara.soul.web;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.extern.slf4j.Slf4j;


/**
 * this is application hystrix controller.
 *
 * @author afj
 */
@RestController
@Slf4j
@RequestMapping("/")
public class OpenController {

    /**
     * Apply soul result.
     *
     * @return the soul result
     */
    @GetMapping("/hystrix")
    public String apply() {
        System.out.println("hystrix: you do not call");
        return "hystrix: you do not call";
    }

}
