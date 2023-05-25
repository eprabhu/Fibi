package com.polus.fibicomp.filemanagement;

import org.springframework.web.multipart.MultipartFile;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FileManagmentInputDto {
	
	private String moduleCode;
	
	private String moduleNumber;
	
	private MultipartFile file;
	
	private String updateUser;
}
