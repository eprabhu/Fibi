package com.polus.fibicomp.filemanagement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.pojo.FileData;

@Service(value = "fileDataOperation")
// This implementation is used to define the operations related to general module
public class GeneralFileDataOperation implements FileDataOperation {

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public void saveFileDetails(FileManagementOutputDto fileManagementOutputDto) {
		try {
			hibernateTemplate.saveOrUpdate(FileData.builder().fileDataId(fileManagementOutputDto.getFileDataId())
					.attachment(fileManagementOutputDto.getData())
					.build());
		} catch (Exception e) {
			throw new FileStorageException(
					"Exception in saveFileDetails in GeneralFileDataDetailOperation." + e.getMessage());
		}
		
	}

	@Override
	public FileManagementOutputDto getFileDataDetails(String fileDataId) {
		try {
			FileData fileData = hibernateTemplate.get(FileData.class, fileDataId);
			return FileManagementOutputDto.builder().fileDataId(fileData.getFileDataId()).data(fileData.getAttachment()).build();
		} catch (Exception e) {
			throw new FileStorageException(
					"Exception in getFileDataDetails in GeneralFileDataDetailOperation." + e.getMessage());
		}
	}

	@Override
	public void deleteFileDataDetails(String fileDataId) {
		try {
			FileData fileData = hibernateTemplate.get(FileData.class, fileDataId);
			hibernateTemplate.delete(fileData);
		} catch (Exception e) {
			throw new FileStorageException(
					"Exception in deleteFileDataDetails in GeneralFileDataDetailOperation." + e.getMessage());
		}
	}

	@Override
	public void updateArchiveFlag(String fileDataId, String archiveFlag) {
		try {
			FileData entity = hibernateTemplate.load(FileData.class, fileDataId);
			if (entity != null) {
				hibernateTemplate.update(entity);
			}
		} catch (Exception e) {
			throw new FileStorageException(
					"Exception in updateArchiveFlag in DisclFileDataDetailOperation." + e.getMessage());
		}
	}

	@Override
	public String getFileDirectoryPath(String baseDirectoryPath, String moduleNumber) {
		return new StringBuilder(baseDirectoryPath).append("/GENERAL/").append(moduleNumber).toString();
	}

}
