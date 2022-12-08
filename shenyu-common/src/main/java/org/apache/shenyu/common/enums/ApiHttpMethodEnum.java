package org.apache.shenyu.common.enums;

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Arrays;

/**
 * @author zhengpeng
 * @date 2022/12/8 4:11 下午
 **/
public enum ApiHttpMethodEnum {

    /**
     *
     */
    GET("GET",0),

    HEAD("HEAD",1),

    POST("POST",2),

    PUT("PUT",3),

    PATCH("PATCH",4),

    DELETE("DELETE",5),

    OPTIONS("OPTIONS",6),

    TRACE("TRACE",7),

    NOT_HTTP("NOT_HTTP",8),

    ;

    ApiHttpMethodEnum(final String name, final Integer value) {
        this.name = name;
        this.value = value;
    }

    private final String name;

    private final Integer value;


    public static Integer getValueByName(final String name){
        return Arrays.stream(ApiHttpMethodEnum.values())
                .filter(e ->  e.name.equals(name)).findFirst()
                .map(item -> item.value)
                .orElseThrow(() -> new ShenyuException(String.format(" this http method can not support %s", name)));
    }

}
