package com.polus.fibicomp.filemanagement;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

@Service
public class FileSystemFileStorageService implements FileStorageService {

	@Value("${app.filemanagement.storage.path}")
	private String baseDirectoryPath;

	@Value("${app.filemanagement.file.archiveOnDelete}")
	private String isArchiveFileEnabled;

	@Value("${app.filemanagement.storage.archivepath}")
	private String archiveDirectoryPath;

	private Path uploadPath;

	@Autowired
	private FileDataDetailsSaveService fileDataDetailsSaveService;
	
	private static Lock lock = new ReentrantLock();

	@Override
	@Transactional(rollbackFor = { FileStorageException.class, IOException.class })
	public FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto) throws FileStorageException {

		String fileDirectoryPath = null;

		String fileDataId = null;

		String fileName = null;

		if (fileManagmentInputDto == null) {
			throw new FileStorageException("FileManagmentInputDto is null in FileSystemFileStorageService");
		}
		

		if (fileManagmentInputDto.getFile() == null) {
			throw new FileStorageException("File is null in FileSystemFileStorageService");
		}		
		
		lock.lock();
		
		MultipartFile file = fileManagmentInputDto.getFile();
		
		try {

			fileDirectoryPath = getFileDirectoryPath(baseDirectoryPath, fileManagmentInputDto.getModuleCode(),
					fileManagmentInputDto.getModuleNumber());

			fileDataId = UniqueFileIdGenerator.generateFileDataId();

			String orginalFileName = file.getOriginalFilename();

			fileName = generateFileName(fileDataId, orginalFileName);

			// Create upload folder if not exists
			if (!Files.exists(Path.of(fileDirectoryPath))) {
				Files.createDirectories(Path.of(fileDirectoryPath));
			}

			if (file.isEmpty()) {
				throw new FileStorageException(
						"Failed to store empty file in FileSystemFileStorageService" + file.getName());
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
																.updateUser(fileManagmentInputDto.getUpdateUser())
																.build();

			fileDataDetailsSaveService.saveFileDetails(fileManagementOutputDto);

			return fileManagementOutputDto;

		} catch (IOException ex) {

			removeFileOnException(fileDirectoryPath, fileName);

			throw new FileStorageException("Failed to store file in FileSystemFileStorageService " + file.getName(),
					ex);

		}finally {
			lock.unlock();
		}

	}

	private String generateFileName(String fileDataId, String originalFilename) {
		String fileExtension = originalFilename.substring(originalFilename.lastIndexOf("."));
		return fileDataId + fileExtension;
	}

	private String getFileDirectoryPath(String baseDirectoryPath, String moduleCode, String moduleNumber) {
		String directoryPath = baseDirectoryPath;
		switch (moduleCode) {
		case FileManagmentConstant.AWARD_MODULE_CODE:
			directoryPath = baseDirectoryPath.concat("/Award/" + moduleNumber);
			break;

		case FileManagmentConstant.PROPOSAL_MODULE_CODE:
			directoryPath = baseDirectoryPath.concat("/Proposal/" + moduleNumber);
			break;

		case FileManagmentConstant.COI_MODULE_CODE:
			directoryPath = baseDirectoryPath.concat("/COI/" + moduleNumber);
			break;

		default:
			directoryPath = baseDirectoryPath.concat("/MISC/" + moduleNumber);
		}

		return directoryPath;
	}

	@Override
	public FileManagementOutputDto downloadFile(String moduleCode, String fileDataId) {
		try {

			FileManagementOutputDto fileData = fileDataDetailsSaveService.getFileDataDetails(moduleCode, fileDataId);
			Path file = Paths.get(fileData.getFilePath(), fileData.getFileName());
			fileData.setData(Files.readAllBytes(file));
			return fileData;

		} catch (IOException e) {
			throw new FileStorageException(
					"An error occurred while downloading the file in FileSystemFileStorageService : " + e.getMessage());
		}
	}

	@Override
	public void deleteFile(String moduleCode, String fileDataId) {
		try {

			FileManagementOutputDto fileData = fileDataDetailsSaveService.getFileDataDetails(moduleCode, fileDataId);
			Path path = Paths.get(fileData.getFilePath(), fileData.getFileName());

			if (Files.exists(path)) {

				if ("Y".equalsIgnoreCase(isArchiveFileEnabled)) {

					Path destinationPath = Paths.get(archiveDirectoryPath, fileData.getFileName());
					Files.move(path, destinationPath);

				} else {
					Files.delete(path);

				}

			}

		} catch (Exception e) {
			throw new FileStorageException(
					"Failed to delete the file in FileSystemFileStorageService, fileDataId = " + fileDataId, e);
		}

	}

	@Override
	public void removeFileOnException(String filePath, String fileName) {

		Path path = Paths.get(filePath, fileName);

		if (Files.exists(path)) {
			try {

				Files.delete(path);

			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}

}
