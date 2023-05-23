package com.polus.fibicomp.filemanagement;

import java.io.OutputStream;
import java.util.UUID;

import javax.annotation.PostConstruct;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Service
public class FileManagementService {
	
	    private FileStorageService fileStorageService;
	   
	    public FileManagementService(FileStorageService fileStorageService) {
	    	this.fileStorageService = fileStorageService;
	    }	   
	    
	    public FileManagementOutputDto saveFile(FileManagmentInputDto fileManagementInputDto) throws FileStorageException{
	    	return fileStorageService.saveFile(fileManagementInputDto);
	    }

	    public OutputStream loadFile(String filename) {
	        return fileStorageService.downloadFile(filename);
	    }

	    public void deleteFile(String filename) {
	    	fileStorageService.deleteFile(filename);
	    }
	    
	   
}
