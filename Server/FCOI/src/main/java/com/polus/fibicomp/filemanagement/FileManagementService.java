package com.polus.fibicomp.filemanagement;

import java.io.IOException;

import org.springframework.stereotype.Service;

@Service
public class FileManagementService {

	private FileStorageService fileStorageService;

	public FileManagementService(FileStorageService fileStorageService) {
		this.fileStorageService = fileStorageService;
	}

	public FileManagementOutputDto saveFile(FileManagmentInputDto fileManagementInputDto) throws FileStorageException, IOException {
		return fileStorageService.saveFile(fileManagementInputDto);
	}

	public FileManagementOutputDto downloadFile(String moduleCode, String fileDataId) {
		return fileStorageService.downloadFile(moduleCode, fileDataId);
	}

	public void deleteFile(String moduleCode, String fileDataId) {
		fileStorageService.deleteFile(moduleCode, fileDataId);
	}

	public void removeFileOnException(String filePath, String fileName) {
		fileStorageService.removeFileOnException(filePath, fileName);
	}

}
