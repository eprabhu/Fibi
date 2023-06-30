package com.polus.fibicomp.filemanagement;

public interface FileStorageService {

	FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto);

	FileManagementOutputDto downloadFile(String moduleCode, String fileDataId);

	void deleteFile(String moduleCode, String fileDataId);

	default void removeFileOnException(String filePath, String fileName) {
	}
}
