package com.polus.fibicomp.filemanagement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.pojo.DisclFileData;
import com.polus.fibicomp.filemanagement.repository.COIFileDataRepository;

@Service
public class FileDataDetailsSaveService {

    @Autowired
	private HibernateTemplate hibernateTemplate;
	
    @Transactional
	protected void saveFileDetails(FileManagementOutputDto fileManagementOutputDto) {
    	try {
    		hibernateTemplate.saveOrUpdate(DisclFileData.builder()
											.fileDataId(fileManagementOutputDto.getFileDataId())
											.fileName(fileManagementOutputDto.getFileName())
											.filePath(fileManagementOutputDto.getFilePath())
											.originalFileName(fileManagementOutputDto.getOriginalFileName())
											.build());
    		
    		
    	}catch(Exception e) {
    		throw new FileStorageException("Exception in saveFileDetails in FileDataDetailsSaveService." + e.getMessage());
    	}
//		switch(fileManagementOutputDto.getModuleCode()) {
//			
//		case FileManagmentConstant.COI_MODULE_CODE :
//			
//			coiRepository.save(DisclFileData.builder()
//											.fileDataId(fileManagementOutputDto.getFileDataId())
//											.fileName(fileManagementOutputDto.getFileName())
//											.filePath(fileManagementOutputDto.getFilePath())
//											.originalFileName(fileManagementOutputDto.getOriginalFileName())
//											.build()
//							  );
//			break;
				
//	}

}
	
}
