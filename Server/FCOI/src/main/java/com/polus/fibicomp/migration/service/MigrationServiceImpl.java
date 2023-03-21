package com.polus.fibicomp.migration.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;

import javax.activation.MimetypesFileTypeMap;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.migration.dao.MigrationDao;
import com.polus.fibicomp.migration.pojo.MigrationAttachmentErrorLog;
import com.polus.fibicomp.migration.pojo.TempAttachmentMigration;

@Transactional
@Service(value = "migrationService")
public class MigrationServiceImpl implements MigrationService {

	protected static Logger logger = LogManager.getLogger(MigrationServiceImpl.class.getName());

	@Value("${path.attachment.path}")
	private String attachmentPath;

	@Value("${path.attachment.destination}")
	private String destination;

	@Value("${path.attachment.level}")
	private int attachmentPathLevel;

	@Value("${path.grantcall.attachment.level}")
	private int grantCallAttachmentPathLevel;

	@Autowired
	private MigrationDao migrationDao;

	@Autowired
	private CommonDao commonDao;

	@Value("${path.grantcall.attachment.destination}")
	private String grantCallAttachmentdestination;

	@Override
	public void saveOrUpdateMigrationData(TempAttachmentMigration tempAttachmentMigration) {
		migrationDao.saveOrUpdateMigrationData(tempAttachmentMigration);
	}

	@Override
	public File uploadFile(String directoryName, Hashtable<String, String> hashTable, int counter,
			HashSet<String> wbs) {
		String folderName = null;
		List<File> subFolders = getDirs(new File(directoryName), attachmentPathLevel);
		logger.info("Root Directory size {}", subFolders.size());
		if (!subFolders.isEmpty()) {
			for (File subFolder : subFolders) {
				folderName = subFolder.getName();
				if (folderName.equals("FIN_Files")) {
					File[] FIN_FilesContents = subFolder.listFiles();
					for (File file : FIN_FilesContents) {
						if (hashTable.containsKey(file.getAbsolutePath())) {
							continue;
						}
						saveFIN_FileData(file, file.getParentFile());
						hashTable.put(file.getAbsolutePath(), "absalute path");
						counter++;
						if (counter >= 1) {
							// file.delete();
							return file;
						}
					}
				} else {
					File[] subFolderContent = subFolder.listFiles();
					for (File folders : subFolderContent) {
						File[] subFolderContents = folders.listFiles();
						long fileCount = fileCount(folders.toPath());
						if (fileCount == 0) {
							deleteFile(folders);
						} else {
							for (File folder : subFolderContents) {
								if (folder.isDirectory()) {
									File[] files = folder.listFiles();
									if (files.length > 0) {
										for (File file : files) {
											if (hashTable.containsKey(file.getAbsolutePath())) {
												continue;
											}
											saveData(file, file.getParentFile());
											hashTable.put(file.getAbsolutePath(), "absalute path");
											counter++;
											// if (counter >= fileCount) {
											if (counter >= 1) {
												// String oldWbsNumber = trimName(folders.getName());
												// wbs.add(oldWbsNumber);
												// file.delete();
												return file;
											}
											// file.delete();
										}
									} else {
										deleteFile(folder);
									}
								} else {
									updateErrorlog(folder);

									// deleteFile(folder);
									return folder;
								}
							}
						}
					}
				}
			}
		}
		return null;
	}

	public void moveFile(File file) {
		try {
			Path temp = Files.move(Paths.get(file.getPath()),
					Paths.get(destination + "/" + file.getName()),
					StandardCopyOption.REPLACE_EXISTING);
			if(temp !=null) {
				logger.info("file moved successfully");
			}
		} catch (Exception e) {
			logger.info("exception in file move {}", e);
		}
	}

	public void moveGrantCallFile(File file) {
		try {
			Path temp = Files.move(Paths.get(file.getPath()),
					Paths.get(grantCallAttachmentdestination + "/" + file.getName()),
					StandardCopyOption.REPLACE_EXISTING);
			if(temp !=null) {
				logger.info("file moved successfully");
			}
		} catch (Exception e) {
			logger.info("exception in file move {}", e);
		}
	}

	public void updateErrorlog(File file) {
		MigrationAttachmentErrorLog migrationAttachmentErrorLog = new MigrationAttachmentErrorLog();
		migrationAttachmentErrorLog.setFileName(file.getName());
		String subDir = file.getAbsoluteFile().getParent();
		subDir = subDir.substring(subDir.lastIndexOf("/") + 1);
		//subDir = subDir.substring(subDir.lastIndexOf("\\") + 1);
		migrationAttachmentErrorLog.setErrorType("Validation");
		migrationAttachmentErrorLog.setErrorMessage("The folder level for the given wbs number " + subDir
				+ " is incorrect please find the directory path for further info " + file.getAbsolutePath());
		migrationAttachmentErrorLog.setLegacyWbsNumber(subDir);
		migrationAttachmentErrorLog.setValidationType("Before Migration");
		migrationAttachmentErrorLog.setUpdateUser("admin");
		migrationAttachmentErrorLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		migrationDao.saveErrorLog(migrationAttachmentErrorLog);
		moveFile(file);
		return;
	}

	public boolean deleteFile(File file) {
		try {
			if (file != null) {
				if (file.isDirectory()) {
					File[] files = file.listFiles();

					for (File f : files) {
						deleteFile(f);
					}
				}
				return Files.deleteIfExists(file.toPath());
			}
		} catch (Exception e) {
			logger.info("Exception in deleteFile {}", e);
		}
		return false;
	}

	@Override
	public long fileCount(Path dir) {
		long count = 0;
		try {
			count = Files.walk(dir).parallel().filter(p -> !p.toFile().isDirectory()).count();
		} catch (IOException e) {
			logger.info("Exception in fileCount {}", e);
		}
		return count;
	}

	public List<File> getDirs(File parent, int level) {
		List<File> dirs = new ArrayList<File>();
		File[] files = parent.listFiles();
		if (files == null)
			return dirs;
		for (File f : files) {
			if (f.isDirectory()) {
				if (level == 0)
					dirs.add(f);
				else if (level > 0)
					dirs.addAll(getDirs(f, level - 1));
			}
		}
		return dirs;
	}

	private void saveData(File file, File directory) {
		try {
			logger.info("Request for saveData for Directory {}", directory);
			String awardAttachmentType = null;
			String attachmentType = getProjectType(directory);
			String subDir = directory.getAbsoluteFile().getParent();
			subDir = subDir.substring(subDir.lastIndexOf("/") + 1);
			//subDir = subDir.substring(subDir.lastIndexOf("\\") + 1);
			String fileName = file.getName();
			String projectId = trimName(subDir);
			// String projectId = subDir;
			String ext = getFileExtension(file);
			MigrationAttachmentErrorLog migrationAttachmentErrorLog = new MigrationAttachmentErrorLog();
			if (ext.equals(".db")) {
				migrationAttachmentErrorLog.setFileName(fileName);
				migrationAttachmentErrorLog.setFileType(ext);
				migrationAttachmentErrorLog.setErrorType("Validation");
				migrationAttachmentErrorLog.setErrorMessage("We are not migrating the files having the extension .db ");
				migrationAttachmentErrorLog.setLegacyWbsNumber(projectId);
				migrationAttachmentErrorLog.setValidationType("Before Migration");
				migrationAttachmentErrorLog.setUpdateUser("admin");
				migrationAttachmentErrorLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				migrationDao.saveErrorLog(migrationAttachmentErrorLog);
				file.delete();
				return;
			} else {
				if (!file.isDirectory()) {
					if (attachmentType != null && fileName != null && projectId != null) {
						if (attachmentType.equals("Claim Report or Invoice")) {
							awardAttachmentType = "Claim Report/Invoice";
						} else if (attachmentType.equals("Budget Scrubbing n Scrubbing")) {
							awardAttachmentType = "Budget Phasing or Scrubbing";
						} else if (attachmentType.equals("Budget Scrubbing or Scrubbing")) {
							awardAttachmentType = "Budget Phasing or Scrubbing";
						} else if (attachmentType.equals("Letter of Award or TnCs")
								|| attachmentType.equals("Letter of Award")) {
							awardAttachmentType = "Letter of Awards inc TnCs";
						} else {
							awardAttachmentType = attachmentType;
						}
						logger.info("fileName : {}", fileName);
						logger.info("projectId : {}", projectId);
						logger.info("attachmentType : {}", attachmentType);
						// boolean isFileNotExist = migrationDao.checkFileAlreadyExist(fileName,
						// projectId,
						// attachmentType);
						// logger.info("isFileNotExist : {}", isFileNotExist);
						// if (isFileNotExist) {
						MimetypesFileTypeMap fileTypeMap = new MimetypesFileTypeMap();
						String mimeType = fileTypeMap.getContentType(file.getName());
						TempAttachmentMigration tempAttachmentMigration = new TempAttachmentMigration();
						tempAttachmentMigration.setProjectId(projectId);
						tempAttachmentMigration.setMimeType(mimeType);
						tempAttachmentMigration.setFileName(fileName);
						tempAttachmentMigration.setAttachmentType(awardAttachmentType);
						tempAttachmentMigration.setProjectType("AWARD");
						byte[] imageData = new byte[(int) file.length()];
						FileInputStream fileInputStream = new FileInputStream(file);
						fileInputStream.read(imageData);
						tempAttachmentMigration.setAttachment(imageData);
						fileInputStream.close();
						saveOrUpdateMigrationData(tempAttachmentMigration);
						// file.delete();
						// }
					}
				} /*
					 * else { File[] files = file.listFiles(); for (File subfolder : files) { if
					 * (subfolder.length() > 0) { saveData(subfolder,
					 * subfolder.getParentFile().getParentFile()); } } }
					 */
			}
		} catch (Exception e) {
			e.getMessage();
		}
	}

	private void saveFIN_FileData(File file, File directory) {
		try {
			logger.info("Request for saveData for Directory {}", directory);
			String awardAttachmentType = "Claim Report/Invoice";
			// String attachmentType = "Claim Report/Invoice";
			String fileName = file.getName();
			String projectId = trimName(file.getName());

			logger.info("fileName : {}", fileName);
			logger.info("projectId : {}", projectId);
			logger.info("attachmentType : {}", awardAttachmentType);

			MimetypesFileTypeMap fileTypeMap = new MimetypesFileTypeMap();
			String mimeType = fileTypeMap.getContentType(file.getName());
			TempAttachmentMigration tempAttachmentMigration = new TempAttachmentMigration();
			tempAttachmentMigration.setFinanceProjectId(projectId);
			tempAttachmentMigration.setMimeType(mimeType);
			tempAttachmentMigration.setFileName(fileName);
			tempAttachmentMigration.setAttachmentType(awardAttachmentType);
			tempAttachmentMigration.setProjectType("AWARD");
			byte[] imageData = new byte[(int) file.length()];
			FileInputStream fileInputStream = new FileInputStream(file);
			fileInputStream.read(imageData);
			tempAttachmentMigration.setAttachment(imageData);
			fileInputStream.close();
			saveOrUpdateMigrationData(tempAttachmentMigration);
			file.delete();
		} catch (Exception e) {
			e.getMessage();
		}
	}

	private String getFileExtension(File file) {
		String name = file.getName();
		int lastIndexOf = name.lastIndexOf(".");
		if (lastIndexOf == -1) {
			return "";
		}
		return name.substring(lastIndexOf);
	}

	void deleteDir(File file) {
		File[] contents = file.listFiles();
		if (contents != null) {
			for (File f : contents) {
				if (f.isDirectory() && f.length() == 0) {
					deleteDir(f);
				}
			}
		}
		file.delete();
	}

	public void threadSleep() {
		try {
			Thread.sleep(300);
		} catch (Exception e) {
			logger.error("Error occured in threadSleep : {}", e.getMessage());
		}
	}

	private String getProjectType(File parentDirectory) {
		String directoryName = null;
		if (parentDirectory.isDirectory()) {
			directoryName = parentDirectory.getName();
		}
		return directoryName;
	}

	private String trimName(String Name) {
		String firstWord = Name;
		if (firstWord.contains(" ")) {
			firstWord = firstWord.substring(0, firstWord.indexOf(" "));
		}
		return firstWord;
	}

	private String trimGrantCallAttachmentName(String Name) {
		String firstWord = Name;
		logger.info("Name {}", Name);
		if (firstWord.contains("_")) {
			firstWord = firstWord.substring(0, firstWord.indexOf("_"));
		}
		logger.info("Name after trim {}", firstWord);
		return firstWord;
	}

	private String trimAttachmentName(String Name) {
		String[] grantCallIds = Name.split("_",2);
		String grantCallId = grantCallIds[0];
		return grantCallId;
	}

	@Override
	public void migrationAttachmentFeed(String wbsNumber) {
		migrationDao.migrationAttachmentFeed(wbsNumber);
	}

	@Override
	public File grantCallUploadFile(String directoryName, Hashtable<String, String> hashTable, int counter,
			HashSet<String> grantcallIds) {
		List<File> subFolders = getDirs(new File(directoryName), grantCallAttachmentPathLevel);
		logger.info("Root Directory size {}", subFolders.size());
		if (!subFolders.isEmpty()) {
			for (File subFolder : subFolders) {
				File[] subFolderContent = subFolder.listFiles();
				for (File folders : subFolderContent) {
					File[] subFolderContents = folders.listFiles();
					long fileCount = fileCount(folders.toPath());
					if (fileCount == 0) {
						deleteFile(folders);
					} else {
						for (File folder : subFolderContents) {
							if (folder.isDirectory()) {
								File[] files = folder.listFiles();
								if (files.length > 0) {
									for (File file : files) {
										double fileSize = file.length() / (1024 * 1024);
										if (fileSize < 100) {
											if (hashTable.containsKey(file.getAbsolutePath())) {
												continue;
											}
											saveGrantCallAttachmentData(file, file.getParentFile());
											hashTable.put(file.getAbsolutePath(), "absolute path");
											counter++;
											if (counter >= 1) {
												return file;
											}
										}
									}
								} else {
									deleteFile(folder);
								}
							} else {
								updateErrorlogForGrantCallAttachment(folder);
								return folder;
							}
						}
					}
				}
			}
		}
		return null;
	}

	private void updateErrorlogForGrantCallAttachment(File file) {
		MigrationAttachmentErrorLog migrationAttachmentErrorLog = new MigrationAttachmentErrorLog();
		migrationAttachmentErrorLog.setFileName(file.getName());
		String subDir = file.getAbsoluteFile().getParent();
		subDir = subDir.substring(subDir.lastIndexOf("/") + 1);
		String grantCallId = trimGrantCallAttachmentName(subDir);
		migrationAttachmentErrorLog.setErrorType("Validation");
		migrationAttachmentErrorLog.setErrorMessage("The folder level for the given grant call id " + subDir
				+ " is incorrect please find the directory path for further info " + file.getAbsolutePath());
		migrationAttachmentErrorLog.setGrantHeaderId(grantCallId);
		migrationAttachmentErrorLog.setValidationType("Before Migration");
		migrationAttachmentErrorLog.setUpdateUser("admin");
		migrationAttachmentErrorLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		migrationDao.saveErrorLog(migrationAttachmentErrorLog);
		moveGrantCallFile(file);
		return;
	}

	private void saveGrantCallAttachmentData(File file, File directory) {
		try {
			logger.info("Request for saveGrantCallAttachmentData for Directory {}", directory);
			String grantCallAttachmentType = null;
			String attachmentType = getProjectType(directory);
			logger.info("attachmentType {}", attachmentType);
			String subDir = directory.getAbsoluteFile().getParent();
			subDir = subDir.substring(subDir.lastIndexOf("/") + 1);
//			subDir = subDir.substring(subDir.lastIndexOf("\\") + 1);
			logger.info("subDir {}", subDir);
			String fileName = file.getName();
			logger.info("fileName {}", fileName);
			String projectId = trimGrantCallAttachmentName(subDir);
			logger.info("projectId {}", projectId);
			String ext = getFileExtension(file);
			logger.info("ext {}", ext);
			MigrationAttachmentErrorLog migrationAttachmentErrorLog = new MigrationAttachmentErrorLog();
			if (ext.equals(".db")) {
				migrationAttachmentErrorLog.setFileName(fileName);
				migrationAttachmentErrorLog.setFileType(ext);
				migrationAttachmentErrorLog.setErrorType("Validation");
				migrationAttachmentErrorLog.setErrorMessage("We are not migrating the files having the extension .db ");
				migrationAttachmentErrorLog.setGrantHeaderId(projectId);
				migrationAttachmentErrorLog.setValidationType("Before GrantCallAttachment Migration");
				migrationAttachmentErrorLog.setUpdateUser("admin");
				migrationAttachmentErrorLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				migrationDao.saveErrorLog(migrationAttachmentErrorLog);
				file.delete();
				return;
			} else {
				if (!file.isDirectory()) {
					logger.info("is not Directory()");
					if (attachmentType != null && fileName != null && projectId != null) {
						if (attachmentType.equals("Application Form")) {
							grantCallAttachmentType = "Application Form (mandatory)";
						} else {
							grantCallAttachmentType = attachmentType;
						}
						logger.info("fileName : {}", fileName);
						logger.info("projectId : {}", projectId);
						logger.info("attachmentType : {}", attachmentType);
						MimetypesFileTypeMap fileTypeMap = new MimetypesFileTypeMap();
						String mimeType = fileTypeMap.getContentType(file.getName());
						TempAttachmentMigration tempAttachmentMigration = new TempAttachmentMigration();
						tempAttachmentMigration.setProjectId(projectId);
						tempAttachmentMigration.setMimeType(mimeType);
						tempAttachmentMigration.setFileName(fileName);
						tempAttachmentMigration.setAttachmentType(grantCallAttachmentType);
						tempAttachmentMigration.setProjectType("GRANT CALL");
						byte[] imageData = new byte[(int) file.length()];
						FileInputStream fileInputStream = new FileInputStream(file);
						fileInputStream.read(imageData);
						tempAttachmentMigration.setAttachment(imageData);
						fileInputStream.close();
						saveOrUpdateMigrationData(tempAttachmentMigration);
					}
				} 
			}
		} catch (Exception e) {
			e.getMessage();
	    }
	}

	@Override
	public void grantCallAttachmentDataMigrationFeed(Integer avType) {
		migrationDao.grantCallAttachmentDataMigrationFeed(avType);
	}

}
