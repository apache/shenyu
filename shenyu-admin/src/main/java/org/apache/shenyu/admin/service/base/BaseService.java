package org.apache.shenyu.admin.service.base;

import org.springframework.http.HttpStatus;
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
        return restTemplate;
    }
}
