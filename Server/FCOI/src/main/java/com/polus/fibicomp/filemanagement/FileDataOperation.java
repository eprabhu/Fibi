package com.polus.fibicomp.filemanagement;

import org.springframework.stereotype.Service;

@Service
public interface FileDataOperation {

	public void saveFileDetails(FileManagementOutputDto fileManagementOutputDto);

	public FileManagementOutputDto getFileDataDetails(String fileDataId);

	public void deleteFileDataDetails(String fileDataId);

	public void updateArchiveFlag(String fileDataId, String archiveFlag);

	public String getFileDirectoryPath(String baseDirectoryPath, String moduleNumber);

}
