package com.polus.formbuilder.config;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.stereotype.Component;

@Component
public class FeignClientInterceptor implements RequestInterceptor {

    private final HttpServletRequest request;

    public FeignClientInterceptor(HttpServletRequest request) {
        this.request = request;
    }

    @Override
    public void apply(RequestTemplate requestTemplate) {
        String token = request.getHeader("Authorization");
        if (token != null && token.startsWith("Bearer ")) {
            requestTemplate.header("Authorization", token);
        }
    }
}
