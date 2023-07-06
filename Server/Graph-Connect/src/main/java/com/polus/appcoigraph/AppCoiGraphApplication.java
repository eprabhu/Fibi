package com.polus.appcoigraph;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.data.neo4j.repository.config.EnableNeo4jRepositories;

@SpringBootApplication
public class AppCoiGraphApplication {

	public static void main(String[] args) {
		SpringApplication.run(AppCoiGraphApplication.class, args);
	}

}
