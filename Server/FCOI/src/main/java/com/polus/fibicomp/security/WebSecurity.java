package com.polus.fibicomp.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.login.dao.LoginDao;
import com.polus.fibicomp.login.service.LoginService;
import com.polus.fibicomp.roles.dao.RolesManagementDao;

@EnableWebSecurity
public class WebSecurity extends WebSecurityConfigurerAdapter {

	@Autowired
	private UserDetailsService userDetailsService;

	@Autowired
	private LoginDao loginDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private RolesManagementDao rolesManagementDao;

	@Autowired
	private LoginService loginService;
	
	@Autowired
	private Environment environment;
	
	@Value("${localhost.address}")
	private String localhost;

	public WebSecurity(UserDetailsService userDetailsService) {
		this.userDetailsService = userDetailsService;
	}

	@Override
	protected void configure(HttpSecurity http) throws Exception {
		http.csrf().disable().authorizeRequests().antMatchers(HttpMethod.POST, Constants.SIGN_UP_URL).permitAll()
//				.and().authorizeRequests().antMatchers("/excelity").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/manpowerBaseSalary").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/sapConcur").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/processClaimInvoiceFeedResponseOOE").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/fastIntegrationRevenuePayments").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/processClaimInvoiceFeedRequestOOE").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/fastResponseProcessing").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/fastExpenseTransactionRTProcessing").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/fastRevenueTransactionRTProcessing").hasIpAddress(localhost)
//				.and().authorizeRequests().antMatchers("/fastIntegrationTemplateGeneration").hasIpAddress(localhost)
//				.anyRequest().authenticated().and()
//				.addFilter(new JWTAuthenticationFilter(authenticationManager(), loginDao, commonService, rolesManagementDao, loginService, environment))
//				.addFilter(new JWTAuthorizationFilter(authenticationManager())).sessionManagement()
//				.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
				.and().exceptionHandling().authenticationEntryPoint(new CustomHttp403ForbiddenEntryPoint());
	}

	@Override
	public void configure(AuthenticationManagerBuilder auth) throws Exception {
		auth.userDetailsService(userDetailsService);
	}

	@Bean
	CorsConfigurationSource corsConfigurationSource() {
		final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		source.registerCorsConfiguration("/**", new CorsConfiguration().applyPermitDefaultValues());
		return source;
	}

}
