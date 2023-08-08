package com.polus.fibicomp.filemanagement;

import java.time.Instant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.pojo.DisclFileData;

@Service
public class FileDataDetailsSaveService {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Transactional
	protected void saveFileDetails(FileManagementOutputDto fileManagementOutputDto) {
		try {
			switch (fileManagementOutputDto.getModuleCode()) {
			case FileManagmentConstant.COI_MODULE_CODE:
				hibernateTemplate.saveOrUpdate(DisclFileData.builder()
						.fileDataId(fileManagementOutputDto.getFileDataId())
						.fileName(fileManagementOutputDto.getFileName())
						.filePath(fileManagementOutputDto.getFilePath())
						.moduleCode(fileManagementOutputDto.getModuleCode())
						.originalFileName(fileManagementOutputDto.getOriginalFileName())
						.updateTimeStamp(Instant.now())
						.updateUser(fileManagementOutputDto.getUpdateUser())
						.data(fileManagementOutputDto.getData())
						.build());
				break;
			}
		} catch (Exception e) {
			throw new FileStorageException("Exception in saveFileDetails in FileDataDetailsSaveService." + e.getMessage());
		}
	}

	public FileManagementOutputDto getFileDataDetails(String moduleCode, String fileDataId) {
		FileManagementOutputDto fileManagementOutputDto = new FileManagementOutputDto();
		try {
			switch (moduleCode) {
			case FileManagmentConstant.COI_MODULE_CODE:
				DisclFileData fileData = hibernateTemplate.get(DisclFileData.class, fileDataId);
				return FileManagementOutputDto.builder()
						.fileDataId(fileData.getFileDataId())
						.data(fileData.getData())
						.fileName(fileData.getFileName())
						.originalFileName(fileData.getOriginalFileName())
						.filePath(fileData.getFilePath())
						.build();
			}
		} catch (Exception e) {
			throw new FileStorageException("Exception in getFileDataDetails in FileDataDetailsSaveService." + e.getMessage());
		}
		return fileManagementOutputDto;
	}

	public void deleteFileDataDetails(String moduleCode, String fileDataId) {
		try {
			switch (moduleCode) {
			case FileManagmentConstant.COI_MODULE_CODE:
				DisclFileData fileData = hibernateTemplate.get(DisclFileData.class, fileDataId);
				hibernateTemplate.delete(fileData);
				break;
			}
		} catch (Exception e) {
			throw new FileStorageException("Exception in deleteFileDataDetails in FileDataDetailsSaveService." + e.getMessage());
		}
	}

	protected void updateArchiveFlag(String moduleCode, String fileDataId, String archiveFlag) {
		try {
			switch (moduleCode) {
			case FileManagmentConstant.COI_MODULE_CODE:
				DisclFileData entity = hibernateTemplate.load(DisclFileData.class, fileDataId);
				if (entity != null) {
					entity.setIsArchived(archiveFlag);
					hibernateTemplate.update(entity);
				}
				break;
			}
		} catch (Exception e) {
			throw new FileStorageException("Exception in updateArchiveFlag in FileDataDetailsSaveService." + e.getMessage());
		}
	}

}
