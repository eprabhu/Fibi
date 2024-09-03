package com.polus.integration.feedentity.config;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
				UserTokens token = userTokensRepository.findByUserName(userName);
				requestTemplate.header(Constant.HEADER_STRING, Constant.TOKEN_PREFIX + token.getAccessToken());
			}
		};
	}

}
