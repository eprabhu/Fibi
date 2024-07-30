package com.polus.integration.entity.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Configuration
@ConfigurationProperties(prefix = "entity.api.url")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class APIConfig{

	private String cleansematch;
	
	private String authToken;
	   
	
}
