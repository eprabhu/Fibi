package com.polus.apigateway.filter;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.gateway.filter.GatewayFilter;
import org.springframework.cloud.gateway.filter.factory.AbstractGatewayFilterFactory;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.stereotype.Component;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ResponseStatusException;

import com.polus.apigateway.utils.JwtUtil;

@Component
public class AuthenticationFilter extends AbstractGatewayFilterFactory<AuthenticationFilter.Config> {

    private static final String COOKIE_FIBI_ACCESS_TOKEN = "Cookie_Token";
    @Autowired
    private RouteValidator validator;

    @Autowired
    private JwtUtil jwtUtil;

    public AuthenticationFilter() {
        super(Config.class);
    }

    @Override
    public GatewayFilter apply(Config config) {
        return ((exchange, chain) -> {
            if (validator.isSecured.test(exchange.getRequest())) {
                String cookieToken = getTokenFromCookie(exchange.getRequest());
                    try {
                        jwtUtil.validateToken(cookieToken);
                    } catch (Exception e) {
                        System.out.println("invalid access...!");
                        throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Unauthorized access to the application");
                    }
            }
            return chain.filter(exchange);
        });
    }

    private String getTokenFromCookie(ServerHttpRequest request) {
        MultiValueMap<String, HttpCookie> cookies = request.getCookies();
        List<HttpCookie> cookieList = cookies.get(COOKIE_FIBI_ACCESS_TOKEN);
        if (cookieList != null && !cookieList.isEmpty()) {
            HttpCookie httpCookie = cookieList.get(0);
            return httpCookie.getValue();
        }
        return null;
    }

    public static class Config {

    }
}
