package com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.security.NoSuchProviderException;
import java.security.Security;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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
import org.springframework.stereotype.Service;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.SftpException;
import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.SftpConfigurationData;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.dao.BaseSalaryDao;
import com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.vo.BaseSalaryVo;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Service(value = "baseSalaryService")
public class BaseSalaryServiceImpl implements BaseSalaryService {

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public BaseSalaryDao baseSalaryDao;

	@Autowired
	public SftpConfigurationService sftpConfigurationService;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	public EmailService emailService;

	@Autowired
	public ExcelityService excelityService;

	protected static Logger logger = LogManager.getLogger(BaseSalaryServiceImpl.class.getName());

	private static final String EMAILBODY_STRUCTURE = "Please go to ";
	private static final String EMAILBODY_FILE_LOC = "  to view the files<br/>";
	private static final String REGULAR_EXPRESSION = "^\"|\"$";
	private static final String UPDATE_USER = "quickstart";
	private static final String FILE_INTERFACE = "BaseSalary";
	byte[] iv;

	@Override
	public void manpowerBaseSalary() {
		Integer initialCount = 0;
		File[] fileData = null;
		String rise_path = getSftpConfigurationValueAsString(Constants.RISE_BASE_SALARY_OUTBOUND);
		if (getSftpConfigurationValueAsString(Constants.IS_BASE_SALARY_ENABLED) != null) {
			getBaseSalaryResDirectory(getSftpConfigurationValueAsString(Constants.SFTP_BASE_SALARY_OUTBOUND), rise_path);
		}
		File directory = new File(rise_path);
		if (directory.isDirectory()) {
			fileData = directory.listFiles();
		}
		if (fileData != null) {
			for (File file : fileData) {
				if (file.isFile() && file.getName().endsWith(".pgp")) {
					BaseSalaryVo baseSalaryVo = new BaseSalaryVo();
					// Decrypt PGP file of Manpower Base Salary
					try {
						pgpFileDecryption(file, baseSalaryVo);
					} catch (Exception e) {
						logger.error("Error in method pgpFileDecryption {}", e.getMessage());
						baseSalaryVo.setErrorOccured(Boolean.TRUE);
					}
					File decFile = new File(getSftpConfigurationValueAsString(Constants.BASE_SALARY_PGP_DECRYPTED_FILE_PATH));
					if (decFile.isDirectory()) {
						fileData = decFile.listFiles();
					}
					if (fileData != null) {
						for (File decryptedFile : fileData) {
							if (baseSalaryVo.getErrorOccured().equals(Boolean.FALSE)) {
								if (decryptedFile.isFile() && decryptedFile.getName().endsWith(".csv")) {
									logger.info("Base Salary file is {}", decryptedFile.getAbsolutePath());
									String pgpFileName = file.getName();
									baseSalaryVo = saveBaseSalaryFiles(decryptedFile, pgpFileName);
									try (BufferedReader fileReader = new BufferedReader(new FileReader(decryptedFile.getAbsoluteFile()))) {
										fileReader.readLine();
										do {
											baseSalaryVo = baseSalaryFileProcess(baseSalaryVo.getFileName(), fileReader, baseSalaryVo);
										} while (baseSalaryVo.getTotalFileRowCount() != null
												&& !(baseSalaryVo.getTotalFileRowCount() == initialCount));
									} catch (IOException e) {
										e.printStackTrace();
										logger.error("Error in method manpowerBaseSalary {}", e.getMessage());
										baseSalaryVo.setErrorOccured(Boolean.TRUE);
									}
									deleteDecryptedFile(decryptedFile);
									if (baseSalaryVo.getErrorOccured().equals(Boolean.FALSE)) {
										moveBaseSalaryFile(file, baseSalaryVo.getEmailBody(), baseSalaryVo.getFileId());
									} else {
										deleteFileFromStaff3Rise(file);
									}
								}
							} else {
								deleteDecryptedFile(decryptedFile);
								deleteFileFromStaff3Rise(file);
							}
						}

					}
				}
			}
		}
	}

	private void deleteFileFromStaff3Rise(File file) {
		if (file != null) {
			try {
				Files.deleteIfExists(Paths.get(file.getAbsolutePath()));
			} catch (Exception e) {
				logger.error("error in deleteFileFromStaff3Rise {}", e.getMessage());
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void getBaseSalaryResDirectory(String sftpWorkingDir, String baseDirectory) {
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

	@Override
	public void moveBaseSalaryFile(File file, StringBuilder emailBody, Integer fileId) {
		try {
			Path temp = Files.move(Paths.get(file.getPath()),
					Paths.get(getSftpConfigurationValueAsString(Constants.RISE_BASE_SALARY_OUTBOUND_ARCHIVE) + "/"
							+ file.getName()), StandardCopyOption.REPLACE_EXISTING);
			if (temp != null) {
				if (getSftpConfigurationValueAsString(Constants.IS_BASE_SALARY_ENABLED) != null) {
					logger.info("file successfully moved {} ", file.getPath());
					moveBaseSalaryDirectorys(getSftpConfigurationValueAsString(Constants.SFTP_BASE_SALARY_OUTBOUND), file.getName(),
							getSftpConfigurationValueAsString(Constants.SFTP_BASE_SALARY_OUTBOUND_ARCHIVE), fileId);
				}
			} else {
				logger.info("File not moved : {} ", file.getPath());
			}
		} catch (Exception e) {
			logger.error("Exception in method moveBaseSalaryFile : {} ", e);
			emailBody.append("Exception in method moveBaseSalaryFile <br/><br/> ").append(e).append("<br/>");
		}
	}

	@SuppressWarnings("unchecked")
	public void moveBaseSalaryDirectorys(String sourceDir, String fileName, String destinationDir, Integer fileId) {
		String sftpHost = getSftpConfigurationValueAsString(Constants.SFTP_HOST);
		int sftpPort = Integer.parseInt(getSftpConfigurationValueAsString(Constants.SFTP_PORT));
		String sftpUser = getSftpConfigurationValueAsString(Constants.SFTP_USER);
		com.jcraft.jsch.Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		logger.info("preparing the host information for move moveBaseSalaryDirectorys.");
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
			logger.info("BaseSalary channel opened and connected.");
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
			logger.info("Exception found while move to SFTP Base Salary Directory.{}", ex);
		} finally {
			channelSftp.exit();
			logger.info("BaseSalary Channel exited.");
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
	public BaseSalaryVo saveBaseSalaryFiles(File decrypted, String pgpFileName) {
		BaseSalaryVo vo = new BaseSalaryVo();
		try {
			String decryptedFile = null;			
			decryptedFile = decrypted.toString();
			ClaimFiles baseSalaryclaimFiles = new ClaimFiles();
			baseSalaryclaimFiles.setFileName(pgpFileName);
			baseSalaryclaimFiles.setNoOfRecords(excelityService.getRowCount(decryptedFile));
			baseSalaryclaimFiles.setUpdateUser(UPDATE_USER);
			baseSalaryclaimFiles.setFileInterface(FILE_INTERFACE);
			baseSalaryclaimFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			baseSalaryclaimFiles = baseSalaryDao.saveManpowerSftpFiles(baseSalaryclaimFiles);
			vo.setFileId(baseSalaryclaimFiles.getFileId());
			vo.setTotalFileRowCount(excelityService.getRowCount(decryptedFile));
			vo.setFileName(decryptedFile);
			vo.setFileRowIndex(0);			
		} catch (Exception e) {
			logger.error("Error in method saveBaseSalaryFiles Information {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
		}
		return vo;
	}

	@Override
	public BaseSalaryVo baseSalaryFileProcess(String fileName, BufferedReader fileReader, BaseSalaryVo vo) {
		try {
			Integer initialCount = 0;
			StringBuilder emailBody = new StringBuilder("");
			BaseSalaryVo manpowerBaseSalaryVo = baseSalaryFileProcessing(fileName, fileReader, emailBody, vo);
			vo.setTotalFileRowCount(manpowerBaseSalaryVo.getTotalFileRowCount());
			vo.setFileRowIndex(manpowerBaseSalaryVo.getFileRowIndex());
			if (manpowerBaseSalaryVo.getTotalFileRowCount().equals(initialCount)) {
				Set<String> employeeId = getpersonIdByFileId(vo.getFileId());
				vo.setEmailBody(setBaseSalaryEmailBody(employeeId, emailBody));
				sendBaseSalaryReportMail(vo);
			}
		} catch (Exception e) {
			logger.error("Error in method baseSalaryFileProcess for Data  {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			return vo;
		}
		return vo;
	}

	private void sendBaseSalaryReportMail(BaseSalaryVo vo) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setBody(vo.getEmailBody().toString());
		emailServiceVO.setSubject("Integration status report for Manpower Base Salary");
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String emailAddress = getSftpConfigurationValueAsString(Constants.SFTP_BASE_SALARY_EMAIL_RECIPIENT);
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

	private Set<String> getpersonIdByFileId(Integer fileId) {
		return baseSalaryDao.getpersonIdByFileId(fileId);
	}

	private BaseSalaryVo baseSalaryFileProcessing(String fileName, BufferedReader fileReader, StringBuilder emailBody, BaseSalaryVo vo) {
		Integer fileId = vo.getFileId();
		Integer totalFileRowCount = vo.getTotalFileRowCount();
		Integer fileRowIndex = vo.getFileRowIndex();
		try {
			final int EMPLOYEE_ID = 0;
			final int BASE_SALARY = 1;
			int fileRowCount = excelityService.getRowCount(fileName);
			String line = "";
			int minLineNumber = fileRowIndex;
			int maxLineNumber = 100;
			int lineNumberToRead = maxLineNumber + minLineNumber;
			while ((line = fileReader.readLine()) != null) {
				String[] tokens = line.split(",", -1);			
				if (tokens.length > 0) {
					while (tokens.length < 2) {
						minLineNumber++;
						--totalFileRowCount;
						String tempLine = fileReader.readLine();
						if (tempLine == null) {
							break;
						}
						line = line + tempLine;
						tokens = line.split(",", -1);
					}					 
					tokens[EMPLOYEE_ID] = tokens[EMPLOYEE_ID].replaceAll(REGULAR_EXPRESSION, "");
					try {
						tokens[BASE_SALARY] = excelityService.encryptAES(tokens[BASE_SALARY]);
					} catch (Exception e) {
						logger.info("Error Occured in AES Encryption ", e);
					}
					Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(tokens[EMPLOYEE_ID]);
					if(manpower == null) {
						manpower = new Manpower();
						manpower.setManpowerPersonId(tokens[EMPLOYEE_ID]);
					}
					
					if(manpower.getManpowerPersonId() != null) {
						manpower.setBaseSalary(tokens[BASE_SALARY]);
					}
					saveBaseSalaryData(emailBody, fileId, manpower);					
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
			logger.error("Error in method baseSalaryFileProcessing", e);
			vo.setErrorOccured(Boolean.TRUE);
			emailBody.append("Error in baseSalaryFileProcessing : <br/>").append(e).append("<br/>");
		}
		return vo;
	}

	private void saveBaseSalaryData(StringBuilder emailBody, Integer fileId, Manpower manpower) {		
		try {
			manpower.setUpdateUser(UPDATE_USER);
			manpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			manpower.setFileId(fileId);
			manpowerIntegrationDao.saveOrUpdate(manpower);
		} catch (Exception e) {
			logger.error("Error in method saveBaseSalaryData", e);
			emailBody.append("Error in saveBaseSalaryData : <br/>").append(e).append("<br/>");
		}
	}

	private StringBuilder setBaseSalaryEmailBody(Set<String> accountNumbers, StringBuilder emailBody) {
		emailBody.append(EMAILBODY_STRUCTURE)
				.append(getSftpConfigurationValueAsString(Constants.RISE_BASE_SALARY_OUTBOUND_ARCHIVE))
				.append(EMAILBODY_FILE_LOC);
		emailBody.append("<br/>Successful Interface for: <br/>");
		if (accountNumbers != null && !accountNumbers.isEmpty()) {
			for (String accountNumber : accountNumbers) {
				emailBody.append(" Employee Id: ").append(accountNumber).append("<br/><br/>");
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
	public void pgpFileDecryption(File file, BaseSalaryVo baseSalaryVo) throws Exception {
		baseSalaryVo.setInputFile(file.getAbsolutePath());
		baseSalaryVo.setKeyFile(getSftpConfigurationValueAsString(Constants.PGP_BASE_SALARY_PRIVATE_KEY_PATH));
		String fileName = FilenameUtils.getBaseName(file.getName());
		baseSalaryVo.setOutputFile(new StringBuilder(getSftpConfigurationValueAsString(Constants.BASE_SALARY_PGP_DECRYPTED_FILE_PATH))
				.append("/").append(fileName).toString());
		String passPhrase = getSftpConfigurationValueAsString(Constants.BASE_SALARY_PASSPHRASE);
		baseSalaryVo.setPassphrase(passPhrase);
		decrypt(baseSalaryVo);
	}

	private boolean decrypt(BaseSalaryVo pgpVo) throws Exception {
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
