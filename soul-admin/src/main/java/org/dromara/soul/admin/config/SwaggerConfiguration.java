package org.dromara.soul.admin.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * swagger configuration information
 *
 * @author: 李晓双
 * @create: 2020-11-19 17:35
 **/
@Configuration
@EnableSwagger2
public class SwaggerConfiguration {

    @Value("${swagger.enable:false}")
    private boolean enable;

    @Bean
    public Docket createRestApi() {
        return new Docket(DocumentationType.SWAGGER_2)
                .apiInfo(this.apiInfo())
                .enable(enable)
                .select()
                .apis(RequestHandlerSelectors.basePackage("org.dromara.soul.admin.controller"))
                .paths(PathSelectors.any())
                .build();
    }


    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .title("soul-admin接口文档")
                .description("")
                .version("1.0.0")
                .contact(new Contact("soul", "https://github.com/dromara/soul", ""))
                .build();
    }
}
