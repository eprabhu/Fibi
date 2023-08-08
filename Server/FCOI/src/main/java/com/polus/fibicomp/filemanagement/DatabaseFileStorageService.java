package com.polus.fibicomp.filemanagement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.multipart.MultipartFile;

public class DatabaseFileStorageService implements FileStorageService {

	@Value("${app.filemanagement.file.archiveOnDelete}")
	private String isArchiveFileEnabled;

	@Autowired
	private FileDataDetailsSaveService fileDataDetailsSaveService;

	@Override
	public FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto) {
		if (fileManagmentInputDto == null) {
			throw new FileStorageException("FileManagmentInputDto is null in DatabaseFileStorageService");
		}
		MultipartFile file = fileManagmentInputDto.getFile();
		if (file == null) {
			throw new FileStorageException("File is null in DatabaseFileStorageService");
		}
		try {
			String fileDataId = UniqueFileIdGenerator.generateFileDataId();
			FileManagementOutputDto fileManagementOutputDto = FileManagementOutputDto.builder()
																		.fileDataId(fileDataId)
																		.originalFileName(file.getOriginalFilename())
																		.data(file.getBytes())
																		.moduleCode(fileManagmentInputDto.getModuleCode())
																		.updateUser(fileManagmentInputDto.getUpdateUser())
																		.build();
			fileDataDetailsSaveService.saveFileDetails(fileManagementOutputDto);
			return fileManagementOutputDto;
		} catch (Exception ex) {
			throw new FileStorageException("Failed to store file in DatabaseFileStorageService " + file.getName(), ex);
		}
	}

	@Override
	public FileManagementOutputDto downloadFile(String moduleCode, String fileDataId) {
		try {
			return fileDataDetailsSaveService.getFileDataDetails(moduleCode, fileDataId);
		} catch (Exception e) {
			throw new FileStorageException("An error occurred while downloading the file in DatabaseFileStorageService : " + e.getMessage());
		}
	}

	@Override
	public void deleteFile(String moduleCode, String fileDataId) {
		try {
			if ("Y".equalsIgnoreCase(isArchiveFileEnabled)) {
				fileDataDetailsSaveService.updateArchiveFlag(moduleCode, fileDataId, "Y");
			} else {
				fileDataDetailsSaveService.deleteFileDataDetails(moduleCode, fileDataId);
			}
		} catch (Exception e) {
			throw new FileStorageException("Failed to delete the file in FileSystemFileStorageService, fileDataId = " + fileDataId, e);
		}
	}

}
