package com.polus.fibi.graphconnect;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;

@SpringBootApplication
@EnableDiscoveryClient
public class GraphConnectMainApp {

	public static void main(String[] args) {
		SpringApplication.run(GraphConnectMainApp.class, args);
	}

}
