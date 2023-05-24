package com.polus.fibicomp.filemanagement;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FileManagementOutputDto {
	
	private String fileDataId;
	
	private String filePath;
	
	private String moduleCode;
	
	private String fileName;
	
	private String originalFileName;
	
	
}
