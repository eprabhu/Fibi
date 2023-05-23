package com.polus.fibicomp.filemanagement;

import java.io.OutputStream;

import org.springframework.web.multipart.MultipartFile;

public interface FileStorageService {

	FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto);
	
	OutputStream downloadFile(String fileDataId);
	
	void deleteFile(String fileDataId);
}
