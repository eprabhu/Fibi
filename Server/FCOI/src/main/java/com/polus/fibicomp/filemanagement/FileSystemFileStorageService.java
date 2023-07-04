package com.polus.fibicomp.filemanagement;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

@Service
public class FileSystemFileStorageService implements FileStorageService {

	@Value("${app.filemanagement.storage.path}")
	private String baseDirectoryPath;

	@Value("${app.filemanagement.file.archiveOnDelete}")
	private String isArchiveFileEnabled;

	@Value("${app.filemanagement.storage.archivepath}")
	private String archiveDirectoryPath;
	
	@Value("${sftp.host}")
    private String sftpHost;

    @Value("${sftp.port}")
    private int sftpPort;

    @Value("${sftp.username}")
    private String sftpUsername;

    @Value("${sftp.password}")
    private String sftpPassword;

	@Autowired
	private FileDataDetailsSaveService fileDataDetailsSaveService;
	
	private static Lock lock = new ReentrantLock();

	@Override
	@Transactional(rollbackFor = { FileStorageException.class, IOException.class })
	public FileManagementOutputDto saveFile(FileManagmentInputDto fileManagmentInputDto) throws IOException {
		String fileDirectoryPath = null;
		String fileDataId = null;
		String fileName = null;
		JSch jsch = new JSch();
        Session session = null;
        ChannelSftp channelSftp = null;
        InputStream inputStream = null;
		if (fileManagmentInputDto == null) {
			throw new FileStorageException("FileManagmentInputDto is null in FileSystemFileStorageService");
		}
		if (fileManagmentInputDto.getFile() == null) {
			throw new FileStorageException("File is null in FileSystemFileStorageService");
		}		
		lock.lock();
		MultipartFile file = fileManagmentInputDto.getFile();
		try {
			if (file.isEmpty()) {
				throw new FileStorageException("Failed to store empty file in FileSystemFileStorageService" + file.getName());
			}
			fileDirectoryPath = getFileDirectoryPath(baseDirectoryPath, fileManagmentInputDto.getModuleCode(), fileManagmentInputDto.getModuleNumber());
			fileDataId = UniqueFileIdGenerator.generateFileDataId();
			String orginalFileName = file.getOriginalFilename();
			fileName = generateFileName(fileDataId, orginalFileName);
			session = jsch.getSession(sftpUsername, sftpHost, sftpPort);
			session.setPassword(sftpPassword);
			session.setConfig("StrictHostKeyChecking", "no");
			session.connect();
			channelSftp = (ChannelSftp) session.openChannel("sftp");
			channelSftp.connect();
			inputStream = file.getInputStream();
			if (!directoryExists(channelSftp, fileDirectoryPath)) {
				createDirectories(channelSftp, fileDirectoryPath);
			}
			channelSftp.cd(fileDirectoryPath);
			channelSftp.put(inputStream, fileName);
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
		} catch (Exception ex) {
			removeFileOnException(fileDirectoryPath, fileName);
			throw new FileStorageException("Failed to store file in FileSystemFileStorageService " + file.getName(), ex);
		}finally {
			lock.unlock();
			if (channelSftp != null) {
                channelSftp.disconnect();
            }
            if (session != null) {
                session.disconnect();
            }
            if (inputStream != null) {
                inputStream.close();
            }
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
		JSch jsch = new JSch();
		Session session = null;
		ChannelSftp channelSftp = null;
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		FileManagementOutputDto fileData = fileDataDetailsSaveService.getFileDataDetails(moduleCode, fileDataId);
		try {
			session = jsch.getSession(sftpUsername, sftpHost, sftpPort);
			session.setPassword(sftpPassword);
			session.setConfig("StrictHostKeyChecking", "no");
			session.connect();
			channelSftp = (ChannelSftp) session.openChannel("sftp");
			channelSftp.connect();
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append(fileData.getFilePath())
					        .append("/")
					        .append(fileData.getFileName());
			String remoteFilePath = stringBuilder.toString();
			channelSftp.get(remoteFilePath, outputStream);
			fileData.setData(outputStream.toByteArray());
			return fileData;
		} catch (Exception e) {
			throw new FileStorageException("An error occurred while downloading the file in FileSystemFileStorageService : " + e.getMessage());
		} finally {
			if (channelSftp != null) {
				channelSftp.disconnect();
			}
			if (session != null) {
				session.disconnect();
			}
		}
	}

	@Override
	public void deleteFile(String moduleCode, String fileDataId) {
		JSch jsch = new JSch();
        Session session = null;
        ChannelSftp channelSftp = null;
		try {
			session = jsch.getSession(sftpUsername, sftpHost, sftpPort);
			session.setPassword(sftpPassword);
			session.setConfig("StrictHostKeyChecking", "no");
			session.connect();
			channelSftp = (ChannelSftp) session.openChannel("sftp");
			channelSftp.connect();
			FileManagementOutputDto fileData = fileDataDetailsSaveService.getFileDataDetails(moduleCode, fileDataId);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append(fileData.getFilePath())
					        .append("/")
					        .append(fileData.getFileName());
			String sourceFilePath = stringBuilder.toString();
			channelSftp.lstat(sourceFilePath);
			if ("Y".equalsIgnoreCase(isArchiveFileEnabled)) {
				StringBuilder strBuilder = new StringBuilder();
				strBuilder.append(archiveDirectoryPath)
						        .append("/")
						        .append(fileData.getFileName());
				String destinationPath = stringBuilder.toString();
				if (!directoryExists(channelSftp, archiveDirectoryPath)) {
					createDirectories(channelSftp, archiveDirectoryPath);
				}
				channelSftp.rename(sourceFilePath, destinationPath);
				fileDataDetailsSaveService.updateArchiveFlag(moduleCode, fileDataId, "Y");
			} else {
				channelSftp.rm(sourceFilePath);
			}
		} catch (Exception e) {
			throw new FileStorageException("Failed to delete the file in FileSystemFileStorageService, fileDataId = " + fileDataId, e);
		}
	}

	@Override
	public void removeFileOnException(String filePath, String fileName) {
		JSch jsch = new JSch();
		Session session = null;
		ChannelSftp channelSftp = null;
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(filePath)
				        .append("/")
				        .append(fileName);
		String remoteFilePath = stringBuilder.toString();
		try {
			session = jsch.getSession(sftpUsername, sftpHost, sftpPort);
			session.setPassword(sftpPassword);
			session.setConfig("StrictHostKeyChecking", "no");
			session.connect();
			channelSftp = (ChannelSftp) session.openChannel("sftp");
			channelSftp.connect();
			channelSftp.lstat(remoteFilePath);
			channelSftp.rm(remoteFilePath);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
            if (channelSftp != null) {
                channelSftp.disconnect();
            }
            if (session != null) {
                session.disconnect();
            }
        }
	}
	
	private boolean directoryExists(ChannelSftp channelSftp, String directoryPath) {
        try {
            channelSftp.stat(directoryPath);
            return true;
        } catch (SftpException e) {
            return false;
        }
    }
	
	private void createDirectories(ChannelSftp channelSftp, String directory) throws SftpException {
	    String[] directories = directory.split("/");
	    for (String dir : directories) {
	        if (!dir.isEmpty()) {
	            try {
	                channelSftp.cd(dir);
	            } catch (SftpException e) {
	                channelSftp.mkdir(dir);
	                channelSftp.cd(dir);
	            }
	        }
	    }
	}

}
