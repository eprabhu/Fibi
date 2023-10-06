package com.polus.formbuilder;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan({"com.polus.formbuilder.*","com.polus.appcorelib.*","com.polus.core.common.*"})
public class FormbuilderApplication {

	
	public static void main(String[] args) {
		SpringApplication.run(FormbuilderApplication.class, args);
		
	}

}
