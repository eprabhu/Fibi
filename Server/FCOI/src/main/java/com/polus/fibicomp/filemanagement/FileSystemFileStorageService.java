package com.polus.fibicomp.filemanagement;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;
import org.springframework.test.annotation.Rollback;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.coi.pojo.DisclFileData;
import com.polus.fibicomp.filemanagement.repository.COIFileDataRepository;

@Service
public class FileSystemFileStorageService implements FileStorageService {
	
	
	@Value("${app.filemanagement.storage.path}")
    private String baseDirectoryPath;
    
    private Path uploadPath;
    
    @Autowired
    private FileDataDetailsSaveService fileDataDetailsSaveService;
    
	
	@Override
	@Transactional(rollbackFor = {FileStorageException.class, IOException.class})	
	public FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto) throws FileStorageException{
		
			if(fileManagmentInputDto == null) {
				 throw new FileStorageException("FileManagmentInputDto is null in FileDataDetailsSaveService");
			}
		
			MultipartFile file = fileManagmentInputDto.getFile();
		
			if (file == null) {
		            throw new FileStorageException("File is null in FileDataDetailsSaveService");
		    }
			  		
	         
	   try {
			 
			 String fileDirectoryPath = getFileDirectoryPath(baseDirectoryPath,
					 										 fileManagmentInputDto.getModuleCode(),
					 										 fileManagmentInputDto.getModuleNumber());
						 
	         String fileDataId = UniqueFileIdGenerator.generateFileDataId();
	         
	         String orginalFileName = file.getOriginalFilename();
	         
	         String fileName = generateFileName(fileDataId,orginalFileName);		
	         
	    	// Create upload folder if not exists
            if (!Files.exists(Path.of(fileDirectoryPath))) {
                Files.createDirectories(Path.of(fileDirectoryPath));
            }
                               		        	
            if (file.isEmpty()) {
                throw new FileStorageException("Failed to store empty file in FileDataDetailsSaveService" + file.getName());
            }

            try (InputStream inputStream = file.getInputStream()) {
            	uploadPath = Paths.get(fileDirectoryPath).toAbsolutePath().normalize();
                Path targetPath = uploadPath.resolve(fileName);
                Files.copy(inputStream, targetPath, StandardCopyOption.REPLACE_EXISTING);
            }
            
            FileManagementOutputDto fileManagementOutputDto = FileManagementOutputDto.builder()
					  .fileDataId(fileDataId)
					  .filePath(fileDirectoryPath)
					  .originalFileName(orginalFileName)
					  .fileName(fileName)
					  .moduleCode(fileManagmentInputDto.getModuleCode())
					  .build();

			fileDataDetailsSaveService.saveFileDetails(fileManagementOutputDto);
			
			return fileManagementOutputDto;
			
			            
        } catch (IOException ex) {
            throw new FileStorageException("Failed to store file in FileDataDetailsSaveService " + file.getName(), ex);
        }	    
	    
	}

	
		
	private String generateFileName(String fileDataId, String originalFilename) {
		String fileExtension = originalFilename.substring(originalFilename.lastIndexOf("."));         
		return fileDataId + fileExtension ;
	}

	private String getFileDirectoryPath(String baseDirectoryPath, String moduleCode, String moduleNumber) {
		String directoryPath = baseDirectoryPath;
		switch(moduleCode) {
			case FileManagmentConstant.AWARD_MODULE_CODE :
				directoryPath = baseDirectoryPath.concat("/Award/"+moduleNumber);
			    break;
			    
			case FileManagmentConstant.PROPOSAL_MODULE_CODE :
				directoryPath = baseDirectoryPath.concat("/Proposal/"+moduleNumber);				
				break;
				
			case FileManagmentConstant.COI_MODULE_CODE :
				directoryPath = baseDirectoryPath.concat("/COI/"+moduleNumber);	
				break;
				
			default:
				directoryPath = baseDirectoryPath.concat("/MISC/"+moduleNumber); 	
		}

		return directoryPath;
	}
	
	@Override
	public OutputStream downloadFile(String fileDataId) {
		
		return null;
	}

	@Override
	public void deleteFile(String fileDataId) {
		// TODO Auto-generated method stub
		
	}

	
}
