package com.polus.formbuilder.config;

import feign.RequestInterceptor;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

@Component
public class FeignClientConfiguration {

    @Bean
    public RequestInterceptor feignClientInterceptor(HttpServletRequest request) {
        return new FeignClientInterceptor(request);
    }
}
