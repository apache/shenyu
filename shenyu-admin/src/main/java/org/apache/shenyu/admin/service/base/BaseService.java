package org.apache.shenyu.admin.service.base;

import java.nio.charset.StandardCharsets;
import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.DefaultResponseErrorHandler;
import org.springframework.web.client.RestTemplate;

/**
 * BaseService.
 */
public abstract class BaseService {

    private static RestTemplate restTemplate = new RestTemplate();

    static {
        // Solve the problem of throwing exceptions if the statuscode is not equal to 200.
        restTemplate.setErrorHandler(new DefaultResponseErrorHandler() {
            protected boolean hasErrorfinal(final HttpStatus statusCode) {
                return statusCode == null;
            }
        });
    }

    /**
     * getRestTemplate.
     *
     * @return RestTemplate
     */
    public static RestTemplate getRestTemplate() {
        // HTTP message converter.
        List<HttpMessageConverter<?>> messageConverters = restTemplate.getMessageConverters();
        messageConverters.stream().forEach(messageConverter -> {
            if (messageConverter instanceof StringHttpMessageConverter) {
                StringHttpMessageConverter stringHttpMessageConverter = (StringHttpMessageConverter) messageConverter;
                // Solve Chinese garbled code.
                stringHttpMessageConverter.setDefaultCharset(StandardCharsets.UTF_8);
            }
        });
        return restTemplate;
    }
}
