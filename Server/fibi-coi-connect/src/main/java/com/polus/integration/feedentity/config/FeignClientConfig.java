package com.polus.integration.feedentity.config;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
//import org.springframework.security.crypto.bcrypt.BCrypt;
import org.springframework.web.server.ResponseStatusException;

import com.polus.integration.constant.Constant;
import com.polus.integration.pojo.UserTokens;
import com.polus.integration.repository.UserTokensRepository;

@Configuration
public class FeignClientConfig {

	@Autowired
	private UserTokensRepository userTokensRepository;

	@Value("${kc.integration.user.name}")
	private String userName;

	@Bean
	RequestInterceptor requestInterceptor() {
		return new RequestInterceptor() {
			@Override
			public void apply(RequestTemplate requestTemplate) {
				/*String hashedUserName = BCrypt.hashpw(userName, Constant.STATIC_SALT);
				UserTokens token = userTokensRepository.findByUserName(hashedUserName);
				String hashedAuthToken = BCrypt.hashpw(authToken, token.getSalt());
				if (token == null || !validateToken(authToken) || !hashedAuthToken.equals(token.getAccessToken())) {
					throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "The provided token is invalid.");
				}
				requestTemplate.header(Constant.HEADER_STRING, Constant.TOKEN_PREFIX + authToken);*/
				UserTokens token = userTokensRepository.findByUserName(userName);
				if (token == null || !validateToken(token.getAccessToken())) {
					throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "The provided token is invalid.");
				}
				requestTemplate.header(Constant.HEADER_STRING, Constant.TOKEN_PREFIX + token.getAccessToken());
			}
		};
	}

	private boolean validateToken(String authToken) {
		try {
			Jwts.parserBuilder().setSigningKey(Keys.hmacShaKeyFor(Constant.SECRET.getBytes())).build().parseClaimsJws(authToken);
			return true;
		} catch (Exception ex) {
			return false;
		}
	}

}
