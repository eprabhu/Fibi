package com.polus;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.io.ClassPathResource;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;

@Import({ FibiRepoConfig.class })
@Configuration
public class FibiPropConfig {

	@Bean
	PropertySourcesPlaceholderConfigurer getPropertyPlaceholderConfigurer() {
		PropertySourcesPlaceholderConfigurer placeHolderConfig = new PropertySourcesPlaceholderConfigurer();
		placeHolderConfig.setLocation(new ClassPathResource("application.properties"));
		placeHolderConfig.setIgnoreUnresolvablePlaceholders(true);
		return placeHolderConfig;
	}

	@Bean
	ObjectMapper objectMapper() {
		return new ObjectMapper();
	}

	@Bean
	ObjectWriter objectWriter(ObjectMapper objectMapper) {
		return objectMapper.writerWithDefaultPrettyPrinter();
	}
}
