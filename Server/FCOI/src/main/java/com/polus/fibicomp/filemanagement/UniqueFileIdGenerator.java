package com.polus.fibicomp.filemanagement;

import java.util.UUID;

public class UniqueFileIdGenerator {
	
	 public static String generateFileDataId() {
    	UUID uuid = UUID.randomUUID();
        String uuidString = uuid.toString().replace("-", "");
    	uuid.toString().replace("-", "");
    	return uuidString;
	    }
}
