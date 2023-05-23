package com.polus.fibicomp.filemanagement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

@Configuration
@ComponentScan(basePackages = "com.polus.fibicomp.filemanagement")
public class FileUtilConfig {

	@Autowired
    private Environment environment;

    @Bean
    public FileStorageService fileStorageService() {
    	
        String storageType = environment.getProperty("app.filemanagement.storage.type");

        if ("filesystem".equalsIgnoreCase(storageType)) {
            return new FileSystemFileStorageService();
        } else if ("database".equalsIgnoreCase(storageType)) {
            return new DatabaseFileStorageService();
        } else {
            throw new IllegalArgumentException("Invalid storage type");
        }
    }	
	
	
}
