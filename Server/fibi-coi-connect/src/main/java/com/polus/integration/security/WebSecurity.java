package com.polus.integration.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import com.polus.integration.constant.Constant;
import com.polus.integration.security.service.UserTokensService;

import jakarta.servlet.http.HttpServletRequest;

@Configuration
@EnableWebSecurity
@Profile("secure")
public class WebSecurity {

	@Autowired
	private UserTokensService userTokensService;

	@Value("${kc.integration.user.name}")
	private String userName;

	@Bean
	SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
		http.csrf(csrf -> csrf.disable())
		.authorizeHttpRequests(authz -> authz.requestMatchers(Constant.RUN_ENRICH_URL, Constant.RUN_CLEANSE_MATCH_URL)
				.permitAll()
				.anyRequest()
				.authenticated())
				.sessionManagement(sess -> sess.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
				.exceptionHandling(ex -> ex.authenticationEntryPoint(new CustomHttp403ForbiddenEntryPoint()));

		http.addFilterBefore((request, response, chain) -> {
			HttpServletRequest httpRequest = (HttpServletRequest) request;
			String servletPath = httpRequest.getServletPath();
			if (servletPath.equals(Constant.RUN_ENRICH_URL) || servletPath.equals(Constant.RUN_CLEANSE_MATCH_URL)) {
				new ConnectAuthenticationFilter(userTokensService, userName).doFilter(request, response, chain);
			} else {
				chain.doFilter(request, response);
			}
		}, UsernamePasswordAuthenticationFilter.class);

		http.addFilterBefore((request, response, chain) -> {
			HttpServletRequest httpRequest = (HttpServletRequest) request;
			String servletPath = httpRequest.getServletPath();
			if (!(servletPath.equals(Constant.RUN_ENRICH_URL)) && !(servletPath.equals(Constant.RUN_CLEANSE_MATCH_URL))) {
				new ConnectAuthorizationFilter(userTokensService).doFilter(request, response, chain);
			} else {
				chain.doFilter(request, response);
			}
		}, UsernamePasswordAuthenticationFilter.class);

		return http.build();
	}

	@Bean
	CorsConfigurationSource corsConfigurationSource() {
		final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		source.registerCorsConfiguration("/**", new CorsConfiguration().applyPermitDefaultValues());
		return source;
	}

}
