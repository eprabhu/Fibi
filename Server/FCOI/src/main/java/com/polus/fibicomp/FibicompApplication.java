package com.polus.fibicomp;

import java.util.TimeZone;

import javax.annotation.PostConstruct;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Info;

@SpringBootApplication
//Can change title based on application, title appears on top of API documentation
@OpenAPIDefinition(info = @Info(title = "FCOI"))
@EnableAutoConfiguration
@EnableJpaRepositories
@EnableTransactionManagement
@ComponentScan
@EnableScheduling
@EnableJpaAuditing(auditorAwareRef = "auditorProvider")
@EnableFeignClients
/* @ComponentScan("com.polus.fibicomp.*") */
public class FibicompApplication {

	@PostConstruct
	void started() {
		TimeZone.setDefault(TimeZone.getTimeZone("TimeZone"));
	}

	public static void main(String[] args) {
		SpringApplication.run(FibicompApplication.class, args);
	}
}
