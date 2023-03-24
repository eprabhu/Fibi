package com.polus.fibicomp.claims.claimsIntegration.excelity.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.security.NoSuchProviderException;
import java.security.Security;
import java.security.spec.KeySpec;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.openpgp.PGPCompressedData;
import org.bouncycastle.openpgp.PGPEncryptedDataList;
import org.bouncycastle.openpgp.PGPException;
import org.bouncycastle.openpgp.PGPLiteralData;
import org.bouncycastle.openpgp.PGPObjectFactory;
import org.bouncycastle.openpgp.PGPOnePassSignatureList;
import org.bouncycastle.openpgp.PGPPrivateKey;
import org.bouncycastle.openpgp.PGPPublicKeyEncryptedData;
import org.bouncycastle.openpgp.PGPSecretKey;
import org.bouncycastle.openpgp.PGPSecretKeyRingCollection;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.SftpException;
import com.polus.fibicomp.claims.claimsIntegration.excelity.dao.ExcelityDaoImpl;
import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.SftpConfigurationData;
import com.polus.fibicomp.claims.claimsIntegration.excelity.vo.ExcelityReportVo;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Service(value = "excelityService")
public class ExcelityServiceImpl implements ExcelityService {

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public ExcelityDaoImpl excelityDao;

	@Autowired
	public SftpConfigurationService sftpConfigurationService;

	@Autowired
	private CommonService commonService;

	@Autowired
	public EmailService emailService;

	@Autowired
	public ExcelityService excelityService;

	@Value("${manpower.excelity.aeskey.decrypt}")
	private String aesKey;

	@Value("${manpower.excelity.saltValue}")
	private String saltValue;

	protected static Logger logger = LogManager.getLogger(ExcelityServiceImpl.class.getName());

	private static final String EMAILBODY_STRUCTURE = "Please go to ";
	private static final String EMAILBODY_FILE_LOC = "  to view the files<br/>";
	private static final String REGULAR_EXPRESSION = "^\"|\"$";
	private static final String UPDATE_USER = "quickstart";
	private static final String FILE_INTERFACE = "Excelity";
	byte[] iv;
	String secretKey = "";

	@Override
	public void manpowerSftpForExcelity() {
		Integer initialCount = 0;
		File[] fileData = null;
		String excelity = getSftpConfigurationValueAsString(Constants.RISE_EXCELITY_OUTBOUND);
		if (getSftpConfigurationValueAsString(Constants.IS_EXCELITY_SFTP_ENABLED) != null) {
			getExcelityResDirectory(getSftpConfigurationValueAsString(Constants.SFTP_EXCELITY_OUTBOUND), excelity);
		}
		File directory = new File(excelity);
		if (directory.isDirectory()) {
			fileData = directory.listFiles();
		}
		if (fileData != null) {
			for (File file : fileData) {
				ExcelityReportVo excelityReportVo = new ExcelityReportVo();
				if (file.isFile() && file.getName().endsWith(".pgp")) {
					// Decrypt PGP file of Excelity
					try {
						pgpFileDecryption(file, excelityReportVo);
					} catch (Exception e1) {
						logger.error("Error in method pgpFileDecryption {}", e1.getMessage());
						excelityReportVo.setErrorOccured(Boolean.TRUE);
						excelityReportVo.getEmailContent().setError(new StringBuilder());
						excelityReportVo.getEmailContent().getError().append("Error in PGP File Decryption : <br/>").append(e1).append("<br/>");
					}
					File decFile = new File(getSftpConfigurationValueAsString(Constants.PGP_DECRYPTED_FILE_PATH));
					if (decFile.isDirectory()) {
						fileData = decFile.listFiles();
					}
					if (fileData != null) {
						for (File decrptdfile : fileData) {
							if (excelityReportVo.getErrorOccured().equals(Boolean.FALSE)) {
								if (decrptdfile.isFile() && decrptdfile.getName().endsWith(".csv")) {
									logger.info("Excelity file is {}", decrptdfile.getAbsolutePath());
									String pgpFileName = file.getName();
									excelityReportVo = saveExcelityFiles(decrptdfile, pgpFileName);
									try (BufferedReader fileReader = new BufferedReader(new FileReader(decrptdfile.getAbsoluteFile()))) {
										fileReader.readLine();
										do {
											excelityReportVo = excelityFileProcess(excelityReportVo.getFileName(), fileReader, excelityReportVo);
										} while (excelityReportVo.getTotalFileRowCount() != null
												&& !excelityReportVo.getTotalFileRowCount().equals(initialCount));
									} catch (IOException e) {
										logger.error("Error in method manpowerSftpForExcelity {}", e.getMessage());
										excelityReportVo.setErrorOccured(Boolean.TRUE);
										excelityReportVo.getEmailContent().getError().append("Error in Excelity File Processing : <br/>").append(e).append("<br/>");
									}
									deleteDecryptedFile(decrptdfile);
									if (excelityReportVo.getErrorOccured().equals(Boolean.FALSE)) {
										moveExcelityFile(file, excelityReportVo);
									} else {
										deleteFileFromStaff3Rise(file);
									}
								}
							} else {
								deleteDecryptedFile(decrptdfile);
								deleteFileFromStaff3Rise(file);
							}
						}
					}
				} else if (file.isFile() && (file.getName().endsWith(".xlsx") || file.getName().endsWith(".xls"))) {
					logger.info("Excelity file is {}", file.getAbsolutePath());
					excelityReportVo = saveExcelityExcelFiles(file);
						excelityReportVo = excelityExcelFileProcess(file, excelityReportVo);
					if (excelityReportVo.getErrorOccured().equals(Boolean.FALSE)) {
						try {
							Thread.sleep(10000);
							moveExcelityFile(file, excelityReportVo);
						} catch (InterruptedException e) {
							e.printStackTrace();
							excelityReportVo.setErrorOccured(Boolean.TRUE);
							logger.error("Error in method moveExcelityExcelFile {}", e.getMessage());
							excelityReportVo.getEmailContent().getError().append("Error in Moving Migration Excel file : <br/>").append(e).append("<br/>");
						}
					} else {
						deleteFileFromStaff3Rise(file);
					}
				}
				if (excelityReportVo.getErrorOccured().equals(Boolean.TRUE)) {
				sendErrorReportMail(excelityReportVo);
				}
			}
		}
	}

	private void sendErrorReportMail(ExcelityReportVo excelityReportVo) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setBody(excelityReportVo.getEmailContent().getError().toString());
		emailServiceVO.setSubject("ERROR : Integration status report for Excelity");
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String emailAddress = getSftpConfigurationValueAsString(Constants.MANPOWER_SFTP_EXCELITY_EMAIL_RECIPIENT);
		if (emailAddress != null && !emailAddress.isEmpty()) {
			String[] singleEmailAddress = emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
			if (singleEmailAddress.length > 0) {
				for (String recipeientMailAddress : singleEmailAddress) {
					commonService.setNotificationRecipientsforNonEmployees(recipeientMailAddress,
							Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				}
			}
			emailServiceVO.setRecipients(dynamicEmailRecipients);
			emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
			emailService.sendEmail(emailServiceVO);
		}		
	}

	private void deleteFileFromStaff3Rise(File file) {
		if (file != null) {
			try {
				Files.deleteIfExists(Paths.get(file.getAbsolutePath()));
			} catch (Exception e) {
				logger.error("error in deleteFileFromStaff3Rise: {}", e.getMessage());
			}
		}
	}

	private ExcelityReportVo saveExcelityExcelFiles(File file) {
		ExcelityReportVo vo = new ExcelityReportVo();
		try {			
			String fileName = null;			
			fileName = file.toString();
			int rowCount = getExcelRowCount(fileName);
			ClaimFiles excelityClaimFiles = new ClaimFiles();
			excelityClaimFiles.setFileName(file.getName());
			excelityClaimFiles.setNoOfRecords(rowCount);
			excelityClaimFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			excelityClaimFiles.setUpdateUser(UPDATE_USER);
			excelityClaimFiles.setFileInterface(FILE_INTERFACE);
			excelityClaimFiles = excelityDao.saveManpowerSftpFiles(excelityClaimFiles);
			vo.setFileId(excelityClaimFiles.getFileId());
			vo.setTotalFileRowCount(rowCount);
			vo.setFileName(fileName);
			vo.setFileRowIndex(0);			
		} catch (Exception e) {
			logger.error("Error in method saveExcelityExcelFiles {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in saving Excelity excel file information : <br/>").append(e).append("<br/>");
		}
		return vo;
	}

	private Integer getExcelRowCount(String fileName) throws IOException {
		int rows = 0;
		if (fileName.endsWith(".xlsx")) {
			XSSFWorkbook workbook = null;
			XSSFSheet sheet;
			try {
				workbook = new XSSFWorkbook(new FileInputStream(fileName));
				sheet = workbook.getSheetAt(0);
				rows = sheet.getPhysicalNumberOfRows();
				rows--;
				logger.info("Row Count:{}", rows);
			} catch (IOException e) {
				e.printStackTrace();
			}finally {
				workbook.close();
			}
		} else if (fileName.endsWith(".xls")) {
			POIFSFileSystem fs = null;
			HSSFWorkbook wb = null;
			try {
				fs = new POIFSFileSystem(new FileInputStream(fileName));
				wb = new HSSFWorkbook(fs);
				HSSFSheet sheet = wb.getSheetAt(0);
				rows = sheet.getPhysicalNumberOfRows();
				rows--;
				logger.info("Row Count:{}", rows);
			} catch (IOException e) {
				e.printStackTrace();
			}finally {
				fs.close();
				wb.close();
			}
		}
		return rows;
	}

	private ExcelityReportVo excelityExcelFileProcess(File file, ExcelityReportVo vo) {
		try {
			Integer initialCount = 0;
			StringBuilder emailBody = new StringBuilder("");
			ExcelityReportVo manpowerSftpReportVo = excelityExcelFileProcessing(emailBody, file, vo.getFileId(), vo.getTotalFileRowCount(), vo);
			vo.setTotalFileRowCount(manpowerSftpReportVo.getTotalFileRowCount());
			if (manpowerSftpReportVo.getTotalFileRowCount().equals(initialCount)) {
				Set<String> accountCode = getaccountCodeByFileId(vo.getFileId());
				vo.setEmailBody(setExcelityEmailBody(accountCode, emailBody));
				sendExcelityReportMail(vo);
			}
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in method excelityExcelFileProcess for Data  {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in Excelity Excel file Processing : <br/>").append(e).append("<br/>");			
		}
		return vo;
	}

	private ExcelityReportVo excelityExcelFileProcessing(StringBuilder emailBody, File file, Integer fileId,
			Integer totalFileRowCount, ExcelityReportVo vo) throws IOException {
		Workbook workbook = null;
		FileInputStream fis = null;
		Integer fileRowIndex = vo.getFileRowIndex();
		int fileRowCount = 0;
		int processed = (fileRowIndex > 0) ?  fileRowIndex : 0;
		String fileName = null;
		try {
			DataFormatter dataFormatter = new DataFormatter();						
			fileName = file.toString();
			fileRowCount = totalFileRowCount;
			fis = new FileInputStream(file);
			workbook = WorkbookFactory.create(fis);
			FormulaEvaluator formulaEvaluator = workbook.getCreationHelper().createFormulaEvaluator();
			Sheet sheet = workbook.getSheetAt(0);
			if (sheet != null) {
				for (Row row : sheet) {
					if (row.getRowNum() == 0) {
						continue; // just skip the rows if row number is 0
					}
					AwardManpowerPayroll awardManpowerPayroll = new AwardManpowerPayroll();
					awardManpowerPayroll.setEmployeeNumber(dataFormatter.formatCellValue(row.getCell(0), formulaEvaluator));
					awardManpowerPayroll.setInternalOrderCode(dataFormatter.formatCellValue(row.getCell(1), formulaEvaluator));
					awardManpowerPayroll.setCostSharing((dataFormatter.formatCellValue(row.getCell(2), formulaEvaluator)) != null && ((dataFormatter.formatCellValue(row.getCell(2), formulaEvaluator)).length() != 0) ? new BigDecimal(dataFormatter.formatCellValue(row.getCell(2), formulaEvaluator)) : null);
					awardManpowerPayroll.setPayElementCode(dataFormatter.formatCellValue(row.getCell(3), formulaEvaluator));
					awardManpowerPayroll.setPayElement(dataFormatter.formatCellValue(row.getCell(4), formulaEvaluator));
					awardManpowerPayroll.setAmount(encryptAES(dataFormatter.formatCellValue(row.getCell(5), formulaEvaluator)));
					awardManpowerPayroll.setGlAccountCode(dataFormatter.formatCellValue(row.getCell(6), formulaEvaluator));
					awardManpowerPayroll.setPayrollPeriod(dataFormatter.formatCellValue(row.getCell(7), formulaEvaluator));
					saveExcelityData(emailBody, fileId, awardManpowerPayroll, vo);
					--totalFileRowCount;
					processed++;
//					logger.info("Remaining Rows to Process: {}",totalFileRowCount);
				}
			}
			--totalFileRowCount;
			if (totalFileRowCount < 0) {
				totalFileRowCount = 0;
			}
			vo.setTotalFileRowCount(totalFileRowCount);
		} catch (Exception ioe) {
			ioe.printStackTrace();
			logger.error("Error in method excelityExcelFileProcessing {}", ioe.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in parsing excelity excel file data : <br/>").append(ioe).append("<br/>")
			.append("Only ").append(processed).append(" are saved out of ").append(fileRowCount).append(" in file : ").append(fileName)
			.append("<br>").append("File ").append(fileName).append(" will be not moved to archive").append("<br>");
		} finally {
			workbook.close();
			fis.close();
		}
		return vo;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void getExcelityResDirectory(String sftpWorkingDir, String baseDirectory) {
		String MANPOWERSFTPHOST = getSftpConfigurationValueAsString(Constants.SFTP_HOST);
		int MANPOWERSFTPPORT = Integer.parseInt(getSftpConfigurationValueAsString(Constants.SFTP_PORT));
		String MANPOWERSFTPUSER = getSftpConfigurationValueAsString(Constants.SFTP_USER);
		com.jcraft.jsch.Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		logger.info("preparing the host information for sftp.");
		try {
			JSch jsch = new JSch();
			session = jsch.getSession(MANPOWERSFTPUSER, MANPOWERSFTPHOST, MANPOWERSFTPPORT);
			session.setPassword(getSftpConfigurationValueAsString(Constants.SFTP_PASSWORD));
			java.util.Properties config = new java.util.Properties();
			config.put("StrictHostKeyChecking", "no");
			session.setConfig(config);
			session.connect();
			logger.info("Host connected.");
			channel = session.openChannel("sftp");
			channel.connect();
			logger.info("sftp channel opened and connected.");
			channelSftp = (ChannelSftp) channel;
			channelSftp.cd(sftpWorkingDir);
			List<String> list = new ArrayList<>();
			Vector<LsEntry> files = channelSftp.ls(sftpWorkingDir);
			for (LsEntry entry : files) {
				if (!entry.getFilename().equals(".") && !entry.getFilename().equals("..")) {
					list.add(entry.getFilename());
					channelSftp.get(entry.getFilename(), baseDirectory);
				}
			}
		} catch (Exception ex) {
			logger.info("Exception found while tranfer file from sftp to local storage {}", ex.getMessage());
		} finally {
			channelSftp.exit();
			logger.info("sftp Channel exited.");
			channel.disconnect();
			logger.info("Channel disconnected.");
			session.disconnect();
			logger.info("Host Session disconnected.");
		}
	}

	public Integer getRowCount(String filename) throws IOException {
		Path path = Paths.get(filename);
		long lines = 0;
		try {
			lines = Files.lines(path).count();
			lines--;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return Math.toIntExact(lines);
	}

	@Override
	public void moveExcelityFile(File file, ExcelityReportVo excelityReportVo) {
		Integer fileId = excelityReportVo.getFileId();
		try {
			Path temp = Files.move(Paths.get(file.getPath()), Paths.get( getSftpConfigurationValueAsString(Constants.RISE_EXCELITY_OUTBOUND_ARCHIVE) + "/" + file.getName()),
					StandardCopyOption.REPLACE_EXISTING);
			if (temp != null) {
				if (getSftpConfigurationValueAsString(Constants.IS_EXCELITY_SFTP_ENABLED) != null) {
					logger.info("file successfully moved {} ", file.getPath());
					moveExcelityDirectorys(getSftpConfigurationValueAsString(Constants.SFTP_EXCELITY_OUTBOUND),
							file.getName(), getSftpConfigurationValueAsString(Constants.SFTP_EXCELITY_OUTBOUND_ARCHIVE),
							fileId);
				}
			} else {
				logger.info("File not moved : {} ", file.getPath());
			}
		} catch (Exception e) {
			logger.error("Exception in method moveExcelityFile : {} ", e);
			excelityReportVo.getEmailContent().getError().append("Exception in move Excelity file : <br/>").append(e)
			.append("<br/>").append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
		}
	}

	@SuppressWarnings("unchecked")
	public void moveExcelityDirectorys(String sourceDir, String fileName, String destinationDir, Integer fileId) {
		String sftpHost = getSftpConfigurationValueAsString(Constants.SFTP_HOST);
		int sftpPort = Integer.parseInt(getSftpConfigurationValueAsString(Constants.SFTP_PORT));
		String sftpUser = getSftpConfigurationValueAsString(Constants.SFTP_USER);
		com.jcraft.jsch.Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		logger.info("preparing the host information for move Excelity Directory.");
		try {
			JSch jsch = new JSch();
			session = jsch.getSession(sftpUser, sftpHost, sftpPort);
			session.setPassword(getSftpConfigurationValueAsString(Constants.SFTP_PASSWORD));
			java.util.Properties config = new java.util.Properties();
			config.put("StrictHostKeyChecking", "no");
			session.setConfig(config);
			session.connect();
			logger.info("Host connected.");
			channel = session.openChannel("sftp");
			channel.connect();
			logger.info("Excelity channel opened and connected.");
			channelSftp = (ChannelSftp) channel;
			channelSftp.cd(sourceDir);
			List<String> list = new ArrayList<>();
			Vector<LsEntry> files = channelSftp.ls(sourceDir);
			for (LsEntry entry : files) {
				if (!entry.getFilename().equals(".") && !entry.getFilename().equals("..")
						&& fileName.equals(entry.getFilename())) {
					list.add(entry.getFilename());
					String sourceFile = new StringBuilder(sourceDir).append(entry.getFilename()).toString();
					String destinationFile = new StringBuilder(destinationDir).append(entry.getFilename()).toString();
					channelSftp.rename(sourceFile, destinationFile);
					channelSftp.cd(destinationDir);
					if (checkFileInArchive(channelSftp, fileName)) {
						logger.info("file successfully moved from {} -> {} ", sourceFile, destinationFile);
					}
				}
			}
		} catch (Exception ex) {
			logger.info("Exception found while move to SFTP Excelity Directory.{}", ex);
		} finally {
			channelSftp.exit();
			logger.info("Excelity Channel exited.");
			channel.disconnect();
			logger.info("Channel disconnected.");
			session.disconnect();
			logger.info("Host Session disconnected.");
		}
	}

	private boolean checkFileInArchive(ChannelSftp channelSftp, String fileName) {
		try {
			channelSftp.lstat(fileName);
			return true;
		} catch (SftpException e) {
			logger.error("Error occured in checkFileInArchive due to : {}", e.getMessage());
			e.printStackTrace();
			return false;
		}
	}

	@Override
	public ExcelityReportVo saveExcelityFiles(File decrptdfile, String pgpFileName) {
		ExcelityReportVo vo = new ExcelityReportVo();
		try {
			String decryptedFile = null;			
			decryptedFile = decrptdfile.toString();
			ClaimFiles excelityClaimFiles = new ClaimFiles();
			excelityClaimFiles.setFileName(pgpFileName);
			excelityClaimFiles.setNoOfRecords(getRowCount(decryptedFile));
			excelityClaimFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			excelityClaimFiles.setUpdateUser(UPDATE_USER);
			excelityClaimFiles.setFileInterface(FILE_INTERFACE);
			excelityClaimFiles = excelityDao.saveManpowerSftpFiles(excelityClaimFiles);
			vo.setFileId(excelityClaimFiles.getFileId());
			vo.setTotalFileRowCount(getRowCount(decryptedFile));
			vo.setFileName(decryptedFile);
			vo.setFileRowIndex(0);
		} catch (Exception e) {
			logger.error("Error in method saveExcelityFile Information {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in saving Excelity file information : <br/>").append(e).append("<br/>");
		}
		return vo;
	}

	@Override
	public ExcelityReportVo excelityFileProcess(String fileName, BufferedReader fileReader, ExcelityReportVo vo) {
		try {
			Integer initialCount = 0;
			StringBuilder emailBody = new StringBuilder("");
			ExcelityReportVo manpowerSftpReportVo = excelityFileProcessing(fileName, fileReader, emailBody, vo);
			vo.setTotalFileRowCount(manpowerSftpReportVo.getTotalFileRowCount());
			vo.setFileRowIndex(manpowerSftpReportVo.getFileRowIndex());
			if (manpowerSftpReportVo.getTotalFileRowCount().equals(initialCount) && vo.getErrorOccured().equals(Boolean.FALSE)) {
				Set<String> accountCode = getaccountCodeByFileId(vo.getFileId());
				vo.setEmailBody(setExcelityEmailBody(accountCode, emailBody));
				sendExcelityReportMail(vo);
			}
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in method excelityFileProcess for Data  {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in Excelity file Process : <br/>").append(e).append("File ").append(fileName).append(" will be not moved to archive").append("<br/>");
		}
		return vo;
	}

	private void sendExcelityReportMail(ExcelityReportVo vo) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setBody(vo.getEmailBody().toString());
		emailServiceVO.setSubject("Integration status report for Excelity");
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String emailAddress = getSftpConfigurationValueAsString(Constants.MANPOWER_SFTP_EXCELITY_EMAIL_RECIPIENT);
		if (emailAddress != null && !emailAddress.isEmpty()) {
			String[] singleEmailAddress = emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
			if (singleEmailAddress.length > 0) {
				for (String recipeientMailAddress : singleEmailAddress) {
					commonService.setNotificationRecipientsforNonEmployees(recipeientMailAddress,
							Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				}
			}
			emailServiceVO.setRecipients(dynamicEmailRecipients);
			emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
			emailService.sendEmail(emailServiceVO);
		}
	}

	private Set<String> getaccountCodeByFileId(Integer fileId) {
		return excelityDao.getaccountCodeByFileId(fileId);
	}

	private ExcelityReportVo excelityFileProcessing(String fileName, BufferedReader fileReader, StringBuilder emailBody, ExcelityReportVo vo) {
		Integer fileId = vo.getFileId();
		Integer totalFileRowCount = vo.getTotalFileRowCount();
		Integer fileRowIndex = vo.getFileRowIndex();
		int fileRowCount = 0;
		int processed = (fileRowIndex > 0) ?  fileRowIndex : 0;
		try {
			final int EMP_ID_C = 0;
			final int COST_CENTRE_WBS = 1;
			final int COST_SHARING = 2;
			final int PAY_ELEMENT_CODE = 3;
			final int PAY_ELEMENT = 4;
			final int AMOUNT = 5;
			final int GL_ACCOUNT_CODE = 6;
			final int PAYROLL_PERIOD = 7;
			fileRowCount = getRowCount(fileName);
			String line = "";
			int minLineNumber = fileRowIndex;
			int maxLineNumber = 100;
			int lineNumberToRead = maxLineNumber + minLineNumber;
			while ((line = fileReader.readLine()) != null) {
				String[] tokens = line.split(",", -1);
				if (tokens.length > 0) {
					while (tokens.length < 8) {
						minLineNumber++;
						--totalFileRowCount;
						String tempLine = fileReader.readLine();
						if (tempLine == null) {
							break;
						}
						line = line + tempLine;
						tokens = line.split(",", -1);
					}
					tokens[EMP_ID_C] = tokens[EMP_ID_C].replaceAll(REGULAR_EXPRESSION, "");
					tokens[COST_CENTRE_WBS] = tokens[COST_CENTRE_WBS].replaceAll(REGULAR_EXPRESSION, "");
					tokens[COST_SHARING] = tokens[COST_SHARING].replaceAll(REGULAR_EXPRESSION, "");
					BigDecimal costSharing = (tokens[COST_SHARING] != null &&  tokens[COST_SHARING].length() != 0) ? new BigDecimal(tokens[COST_SHARING]) : null;
					tokens[PAY_ELEMENT_CODE] = tokens[PAY_ELEMENT_CODE].replaceAll(REGULAR_EXPRESSION, "");
					tokens[PAY_ELEMENT] = tokens[PAY_ELEMENT].replaceAll(REGULAR_EXPRESSION, "");
					try {
						tokens[AMOUNT] = encryptAES(tokens[AMOUNT]);
					} catch (Exception e) {
						logger.info("Error Occured in AES Encryption ", e);
					}
					tokens[GL_ACCOUNT_CODE] = tokens[GL_ACCOUNT_CODE].replaceAll(REGULAR_EXPRESSION, "");
					tokens[PAYROLL_PERIOD] = tokens[PAYROLL_PERIOD].replaceAll(REGULAR_EXPRESSION, "");
					AwardManpowerPayroll awardManpowerPayroll = new AwardManpowerPayroll(tokens[GL_ACCOUNT_CODE],
							tokens[EMP_ID_C], tokens[COST_CENTRE_WBS], costSharing ,
							tokens[PAY_ELEMENT_CODE], tokens[PAY_ELEMENT], tokens[AMOUNT], tokens[PAYROLL_PERIOD]);
					saveExcelityData(emailBody, fileId, awardManpowerPayroll, vo);					
					processed++;
//					logger.info("Number of Rows processed: {}", processed);
				}
				minLineNumber++;
				--totalFileRowCount;
				if (totalFileRowCount < 0 && minLineNumber == fileRowCount) {
					totalFileRowCount = 0;
				}
				if (minLineNumber == lineNumberToRead) {
					logger.info("Total row Inserted {} ", minLineNumber);
					break;
				}
			}
			if (totalFileRowCount < 0 || line == null ) {
				totalFileRowCount = 0;
			}
			vo.setTotalFileRowCount(totalFileRowCount);
			vo.setFileRowIndex(minLineNumber);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in method excelityFileProcessing", e);
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in parsing excelity file data : <br/>").append(e).append("<br/>")
			.append("Only ").append(processed).append(" are saved out of ").append(fileRowCount).append(" in file : ").append(fileName)
			.append("<br>").append("File ").append(fileName).append(" will be not moved to archive").append("<br>");
		}
		return vo;
	}

	private void saveExcelityData(StringBuilder emailBody, Integer fileId, AwardManpowerPayroll awardManpowerPayroll, ExcelityReportVo vo) {
		try {
			awardManpowerPayroll.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardManpowerPayroll.setFileId(fileId);
			awardManpowerPayroll.setUpdateUser(UPDATE_USER);
			if (!awardManpowerPayroll.getGlAccountCode().isEmpty()
					&& !awardManpowerPayroll.getEmployeeNumber().isEmpty()
					&& !awardManpowerPayroll.getInternalOrderCode().isEmpty()
					&& awardManpowerPayroll.getCostSharing() != null
					&& !awardManpowerPayroll.getPayElementCode().isEmpty()
					&& !awardManpowerPayroll.getPayElement().isEmpty() && !awardManpowerPayroll.getAmount().isEmpty()
					&& !awardManpowerPayroll.getPayrollPeriod().isEmpty()) {
				awardManpowerPayroll.setRemarks("Success");
			} else {
				awardManpowerPayroll.setRemarks("Failed");
			}
			excelityDao.saveManpowerSftp(awardManpowerPayroll);
		} catch (Exception e) {
			logger.error("Error in method saveExcelityData", e);
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Error in saving Excelity file Data : <br/>").append(e).append("<br/>");
		}
	}

	public String getDecryptedSecretKeyAES() throws Exception {
		if (secretKey.isEmpty()) {
			secretKey = decryptAESKey(getSftpConfigurationValueAsString(Constants.AES_SECRETE_KEY));
		}
		return secretKey;
	}

	public String encryptAES(String data) throws Exception {
		String secretKey = getDecryptedSecretKeyAES();
		// String secretKey = aesKey;
		try {
			byte[] iv = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			IvParameterSpec ivspec = new IvParameterSpec(iv);
			SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
			KeySpec spec = new PBEKeySpec(secretKey.toCharArray(), saltValue.getBytes(), 65536, 256);
			SecretKey tmp = factory.generateSecret(spec);
			SecretKeySpec seckey = new SecretKeySpec(tmp.getEncoded(), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
			cipher.init(Cipher.ENCRYPT_MODE, seckey, ivspec);
			return Base64.getEncoder().encodeToString(cipher.doFinal(data.getBytes("UTF-8")));
		} catch (Exception e) {
			logger.error("Error while AES encrypting:", e);
		}
		return null;
	}

	public String decryptAESData(String data) throws Exception {
		String secretKey = getDecryptedSecretKeyAES();
		try {
			byte[] iv = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			IvParameterSpec ivspec = new IvParameterSpec(iv);
			SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
			KeySpec spec = new PBEKeySpec(secretKey.toCharArray(), saltValue.getBytes(), 65536, 256);
			SecretKey tmp = factory.generateSecret(spec);
			SecretKeySpec secKey = new SecretKeySpec(tmp.getEncoded(), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
			cipher.init(Cipher.DECRYPT_MODE, secKey, ivspec);
			return new String(cipher.doFinal(Base64.getDecoder().decode(data)));
		} catch (Exception e) {
			logger.error("Error while AES Data decrypting:", e);
		}
		return null;
	}

	public String decryptAESKey(String secretKey) throws Exception {
		try {
			byte[] iv = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			IvParameterSpec ivspec = new IvParameterSpec(iv);
			SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
			KeySpec spec = new PBEKeySpec(aesKey.toCharArray(), saltValue.getBytes(), 65536, 256);
			SecretKey tmp = factory.generateSecret(spec);
			SecretKeySpec secKey = new SecretKeySpec(tmp.getEncoded(), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
			cipher.init(Cipher.DECRYPT_MODE, secKey, ivspec);
			return new String(cipher.doFinal(Base64.getDecoder().decode(secretKey)));
		} catch (Exception e) {
			logger.error("Error while AES Key decrypting:", e);
		}
		return null;
	}

	private StringBuilder setExcelityEmailBody(Set<String> accountNumbers, StringBuilder emailBody) {
		emailBody.append(EMAILBODY_STRUCTURE)
				.append(getSftpConfigurationValueAsString(Constants.RISE_EXCELITY_OUTBOUND_ARCHIVE))
				.append(EMAILBODY_FILE_LOC);
		emailBody.append("<br/>Successful Interface for: <br/>");
		if (accountNumbers != null && !accountNumbers.isEmpty()) {
			for (String accountNumber : accountNumbers) {
				emailBody.append(" Account Number : ").append(accountNumber).append("<br/><br/>");
			}
		}
		return emailBody;
	}

	@Override
	public String getSftpConfigurationValueAsString(String parameterName) {
		SftpConfigurationData sftpConfigurationData = sftpConfigurationService.getSFTPConfigurationValue(parameterName);
		return sftpConfigurationData.getConfigurationValue();
	}

	@Override
	public void pgpFileDecryption(File file, ExcelityReportVo excelityReportVo) throws Exception {
		excelityReportVo.setInputFile(file.getAbsolutePath());
		excelityReportVo.setKeyFile(getSftpConfigurationValueAsString(Constants.PGP_PRIVATE_KEY_PATH));
		String fileName = FilenameUtils.getBaseName(file.getName());
		excelityReportVo.setOutputFile(new StringBuilder(getSftpConfigurationValueAsString(Constants.PGP_DECRYPTED_FILE_PATH))
				.append("/").append(fileName).toString());
		String passPhrase = getSftpConfigurationValueAsString(Constants.EXCELITY_PASSPHRASE);
		excelityReportVo.setPassphrase(passPhrase);
		decrypt(excelityReportVo);
	}

	private boolean decrypt(ExcelityReportVo pgpVo) throws Exception {
		FileInputStream in = null;
		FileInputStream keyIn = null;
		FileOutputStream out = null;
		try {
			in = new FileInputStream(pgpVo.getInputFile());
			keyIn = new FileInputStream(pgpVo.getKeyFile());
			out = new FileOutputStream(pgpVo.getOutputFile());
			String passphrase = pgpVo.getPassphrase();
			decryptFile(in, out, keyIn, passphrase.toCharArray());
		} catch (Exception e) {
			logger.error("Error in method decrypt", e);
			pgpVo.setErrorOccured(Boolean.TRUE);
			pgpVo.getEmailContent().getError().append("Error in PGP file Decryption : <br/>").append(e).append("<br/>");
		} finally {
			in.close();
			out.close();
			keyIn.close();
		}
		return true;
	}

	@SuppressWarnings("unchecked")
	public static void decryptFile(InputStream in, OutputStream out, InputStream keyIn, char[] passwd)
			throws Exception {
		Security.addProvider(new BouncyCastleProvider());
		in = org.bouncycastle.openpgp.PGPUtil.getDecoderStream(in);
		PGPObjectFactory pgpF = new PGPObjectFactory(in);
		PGPEncryptedDataList enc;
		Object o = pgpF.nextObject();
		if (o instanceof PGPEncryptedDataList) {
			enc = (PGPEncryptedDataList) o;
		} else {
			enc = (PGPEncryptedDataList) pgpF.nextObject();
		}
		Iterator<PGPPublicKeyEncryptedData> it = enc.getEncryptedDataObjects();
		PGPPrivateKey sKey = null;
		PGPPublicKeyEncryptedData pbe = null;
		while (sKey == null && it.hasNext()) {
			pbe = it.next();

			sKey = findSecretKey(keyIn, pbe.getKeyID(), passwd);
		}
		if (sKey == null) {
			throw new IllegalArgumentException("Secret key for PGP File not found.");
		}
		InputStream clear = pbe.getDataStream(sKey, "BC");
		PGPObjectFactory plainFact = new PGPObjectFactory(clear);
		Object message = plainFact.nextObject();
		if (message instanceof PGPCompressedData) {
			PGPCompressedData cData = (PGPCompressedData) message;
			PGPObjectFactory pgpFact = new PGPObjectFactory(cData.getDataStream());

			message = pgpFact.nextObject();
		}
		if (message instanceof PGPLiteralData) {
			PGPLiteralData ld = (PGPLiteralData) message;
			InputStream unc = ld.getInputStream();
			int ch;
			while ((ch = unc.read()) >= 0) {
				out.write(ch);
			}
		} else if (message instanceof PGPOnePassSignatureList) {
			throw new PGPException("Encrypted PGP file contains a signed message - not literal data.");
		} else {
			throw new PGPException("Message is not a simple encrypted file - type unknown.");
		}

		// This is to check the integrity of the PGP Decrypted file
		/*
		 * if (pbe.isIntegrityProtected()) { if (!pbe.verify()) { throw new
		 * PGPException("Message failed integrity check"); } }
		 */
	}

	private static PGPPrivateKey findSecretKey(InputStream keyIn, long keyID, char[] pass)
			throws IOException, PGPException, NoSuchProviderException {
		PGPSecretKeyRingCollection pgpSec = new PGPSecretKeyRingCollection(
				org.bouncycastle.openpgp.PGPUtil.getDecoderStream(keyIn));
		PGPSecretKey pgpSecKey = pgpSec.getSecretKey(keyID);
		if (pgpSecKey == null) {
			return null;
		}
		return pgpSecKey.extractPrivateKey(pass, "BC");
	}

	@Override
	public void deleteDecryptedFile(File decrptdfile) {
		if (decrptdfile != null) {
			try {
				Files.deleteIfExists(Paths.get(decrptdfile.getAbsolutePath()));
			} catch (Exception e) {
				logger.error("error in deleteDecryptedFile {}", e.getMessage());
			}
		}
	}

}
