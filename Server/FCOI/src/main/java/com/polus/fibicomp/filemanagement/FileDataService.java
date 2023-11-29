package com.polus.fibicomp.filemanagement;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class FileDataService {

	@Autowired
	private Map<String,FileDataOperation> fileDataDetailOperations; 

	public void saveFileDetails(FileManagementOutputDto fileManagementOutputDto) {	
		FileDataOperation selectedService = getServiceByModule(fileManagementOutputDto.getModuleCode());
		selectedService.saveFileDetails(fileManagementOutputDto);
	}

	private FileDataOperation getServiceByModule(String moduleCode) {
		String key = new StringBuilder("fileDataOperation").append((moduleCode != null) ? new StringBuilder("_").append(moduleCode): "").toString();
		return fileDataDetailOperations.get(key);
	}

	public FileManagementOutputDto getFileDataDetails(String moduleCode, String fileDataId) {
		FileDataOperation selectedService = getServiceByModule(moduleCode);
		return selectedService.getFileDataDetails(fileDataId);
	}

	public void updateArchiveFlag(String moduleCode, String fileDataId, String archiveFlag) {
		FileDataOperation selectedService = getServiceByModule(moduleCode);
		selectedService.updateArchiveFlag(fileDataId, archiveFlag);
	}

	public void deleteFileDataDetails(String moduleCode, String fileDataId) {
		FileDataOperation selectedService = getServiceByModule(moduleCode);
		selectedService.deleteFileDataDetails(fileDataId);
	}

	public String getFileDirectoryPath(String baseDirectoryPath, String moduleCode, String moduleNumber) {
		FileDataOperation selectedService = getServiceByModule(moduleCode);
		return selectedService.getFileDirectoryPath(baseDirectoryPath, moduleNumber);
	}

}
