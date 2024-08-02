package com.polus;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@EnableFeignClients
@EnableAsync
@ComponentScan({"com.polus.*"})
public class CoiIntegrationApplication {

	public static void main(String[] args) {
		SpringApplication.run(CoiIntegrationApplication.class, args);
	}

}
