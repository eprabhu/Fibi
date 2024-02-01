package com.polus.apigateway.filter;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.gateway.filter.GatewayFilter;
import org.springframework.cloud.gateway.filter.factory.AbstractGatewayFilterFactory;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.stereotype.Component;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ResponseStatusException;

import com.polus.apigateway.utils.JwtUtil;

@Component
public class AuthenticationFilter extends AbstractGatewayFilterFactory<AuthenticationFilter.Config> {

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
				if (cookieToken != null) {
					try {
						jwtUtil.validateToken(cookieToken);
					} catch (Exception e) {
						System.out.println("invalid access...!");
						throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Unauthorized access to the application");
					}
				} else {
					// header contains token or not
					if (!exchange.getRequest().getHeaders().containsKey(HttpHeaders.AUTHORIZATION)) {
						throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Unauthorized access to the application");
					}
					String authHeader = exchange.getRequest().getHeaders().get(HttpHeaders.AUTHORIZATION).get(0);
					if (authHeader != null && authHeader.startsWith("Bearer ")) {
						authHeader = authHeader.substring(7);
					}
					try {
						jwtUtil.validateToken(authHeader);
					} catch (Exception e) {
						System.out.println("invalid access...!");
						throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Unauthorized access to the application");
					}
				}
			}
            return chain.filter(exchange);
        });
    }

	private String getTokenFromCookie(ServerHttpRequest request) {
		MultiValueMap<String, HttpCookie> cookies = request.getCookies();
		List<HttpCookie> cookieList = cookies.get("Cookie_Token");
		if (cookieList != null && !cookieList.isEmpty()) {
			HttpCookie httpCookie = cookieList.get(0);
			return httpCookie.getValue();
		}
		return null;
	}

    public static class Config {

    }
}
