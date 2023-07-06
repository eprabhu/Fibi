package com.polus.fibicomp.filemanagement;

import java.io.IOException;

public interface FileStorageService {

	FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto) throws IOException;

	FileManagementOutputDto downloadFile(String moduleCode, String fileDataId);

	void deleteFile(String moduleCode, String fileDataId);

	default void removeFileOnException(String filePath, String fileName) {
	}
}
