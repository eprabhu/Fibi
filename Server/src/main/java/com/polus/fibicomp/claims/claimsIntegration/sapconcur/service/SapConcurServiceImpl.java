package com.polus.fibicomp.claims.claimsIntegration.sapconcur.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.dao.SapConcurDao;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.pojo.ConcurStaffTravelDtls;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.vo.SapConcurVo;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Service(value = "sapConcurService")
public class SapConcurServiceImpl implements SapConcurService {

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public SapConcurDao sapConcurDao;

	@Autowired
	public SftpConfigurationService sftpConfigurationService;

	@Autowired
	private CommonService commonService;

	@Autowired
	public EmailService emailService;

	@Autowired
	public SapConcurService sapConcurService;
	
	@Autowired
	ExcelityService excelityService;

	protected static Logger logger = LogManager.getLogger(SapConcurServiceImpl.class.getName());

	private static final String EMAILBODY_STRUCTURE = "Please go to ";
	private static final String EMAILBODY_FILE_LOC = "  to view the files<br/>";
	private static final String REGULAR_EXPRESSION = "^\"|\"$";
	private static final String UPDATE_USER = "quickstart";
	private static final String FILE_INTERFACE = "SapConcur";

	@Override
	public void sapConcurSftp() {
		Integer initialCount = 0;
		File[] fileData = null;
		String riseOutbound = getSftpConfigurationValueAsString(Constants.RISE_SAP_CONCUR_OUTBOUND);
		if (getSftpConfigurationValueAsString(Constants.IS_SAP_CONCUR_ENABLED) != null) {
			getSapConcurResDirectory(getSftpConfigurationValueAsString(Constants.SFTP_SAP_CONCUR_OUTBOUND), riseOutbound, null);
		}
		File directory = new File(riseOutbound);
		if (directory.isDirectory()) {
			fileData = directory.listFiles();
		}
		if (fileData != null) {
			for (File file : fileData) {
				if (file.isFile()) {
					SapConcurVo sapConcurVo = new SapConcurVo();
					logger.info("Sap Concur file is {}", file.getAbsolutePath());
					sapConcurVo = saveSapConcurFiles(file, sapConcurVo);
					try (BufferedReader fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()))) {
						fileReader.readLine();
						do {
							sapConcurVo = sapConcurFileProcess(sapConcurVo.getFileName(), fileReader, sapConcurVo);
						} while (sapConcurVo.getTotalFileRowCount() != null
								&& !sapConcurVo.getTotalFileRowCount().equals(initialCount));
					} catch (IOException e) {
						logger.error("Error in method sapConcurSftp {}", e.getMessage());
						sapConcurVo.setErrorOccured(Boolean.TRUE);
					}
					if (sapConcurVo.getErrorOccured().equals(Boolean.FALSE)) {
						moveSapConcurFile(file, sapConcurVo.getEmailBody(), sapConcurVo.getFileId());
					} else {
						deleteFileFromStaff3Rise(file);
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
	public void getSapConcurResDirectory(String sftpWorkingDir, String baseDirectory, EmailContent emailContent) {
		String SAPCONCURSFTPHOST = getSftpConfigurationValueAsString(Constants.SFTP_HOST);
		int SAPCONCURSFTPPORT = Integer.parseInt(getSftpConfigurationValueAsString(Constants.SFTP_PORT));
		String SAPCONCURSFTPUSER = getSftpConfigurationValueAsString(Constants.SFTP_USER);
		com.jcraft.jsch.Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		logger.info("preparing the host information for sftp.");
		try {
			JSch jsch = new JSch();
			session = jsch.getSession(SAPCONCURSFTPUSER, SAPCONCURSFTPHOST, SAPCONCURSFTPPORT);
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
			logger.info("Exception found while transfer file from sftp to local storage {}", ex.getMessage());
			if (emailContent != null) {
				emailContent.getError().append("Exception found while transfer file from sftp to local storage {}").append(" : <br/>").append(ex.getCause()).append("<br/>");
			}
		} finally {
			channelSftp.exit();
			logger.info("sftp Channel exited.");
			channel.disconnect();
			logger.info("Channel disconnected.");
			session.disconnect();
			logger.info("Host Session disconnected.");
		}
	}

	public void moveSapConcurFile(File file, StringBuilder emailBody, Integer fileId) {
		try {
			Path temp = Files.move(Paths.get(file.getPath()),
					Paths.get(getSftpConfigurationValueAsString(Constants.RISE_SAP_CONCUR_OUTBOUND_ARCHIVE) + "/"
							+ file.getName()),
					StandardCopyOption.REPLACE_EXISTING);
			if (temp != null) {
				if (getSftpConfigurationValueAsString(Constants.IS_SAP_CONCUR_ENABLED) != null) {
					logger.info("file successfully moved {} ", file.getPath());
					moveSapConcurDirectorys(getSftpConfigurationValueAsString(Constants.SFTP_SAP_CONCUR_OUTBOUND), file.getName(),
							getSftpConfigurationValueAsString(Constants.SFTP_SAP_CONCUR_OUTBOUND_ARCHIVE), fileId);
				}
			} else {
				logger.info("File not moved : {} ", file.getPath());
			} 
		} catch (Exception e) {
			logger.error("Exception in method moveSapConcurFile : {} ", e);
			emailBody.append("Exception in method moveSapConcurFile <br/><br/> ").append(e).append("<br/>");
		}
	}

	@SuppressWarnings("unchecked")
	public void moveSapConcurDirectorys(String sourceDir, String fileName, String destinationDir, Integer fileId) {
		String sftpHost = getSftpConfigurationValueAsString(Constants.SFTP_HOST);
		int sftpPort = Integer.parseInt(getSftpConfigurationValueAsString(Constants.SFTP_PORT));
		String sftpUser = getSftpConfigurationValueAsString(Constants.SFTP_USER);
		com.jcraft.jsch.Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		logger.info("preparing the host information for move SapConcur Directorys.");
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
			logger.info("Sap Concur SFTP Channel opened and connected.");
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
			logger.info("Exception found while move to SFTP Sap Concur Directorys.{}", ex);
		} finally {
			channelSftp.exit();
			logger.info("Sap Concur SFTP Channel exited.");
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
	public SapConcurVo saveSapConcurFiles(File file, SapConcurVo sapConcurVo) {
		try {
			String fileName = null;			
			fileName = file.toString();
			ClaimFiles concurClaimFiles = new ClaimFiles();
			concurClaimFiles.setFileName(file.getName());
			concurClaimFiles.setNoOfRecords(excelityService.getRowCount(fileName));
			concurClaimFiles.setUpdateUser(UPDATE_USER);
			concurClaimFiles.setFileInterface(FILE_INTERFACE);
			concurClaimFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			concurClaimFiles = sapConcurDao.saveSapConcurSftpFiles(concurClaimFiles);
			sapConcurVo.setFileId(concurClaimFiles.getFileId());
			sapConcurVo.setTotalFileRowCount(excelityService.getRowCount(fileName));
			sapConcurVo.setFileName(fileName);
			sapConcurVo.setFileRowIndex(0);
		} catch (Exception e) {
			logger.error("Error in method saveSapConcurFiles Information {}", e.getMessage());
			sapConcurVo.setErrorOccured(Boolean.TRUE);
		}
		return sapConcurVo;
	}

	@Override
	public SapConcurVo sapConcurFileProcess(String fileName, BufferedReader fileReader, SapConcurVo vo) {
		try {
			Integer initialCount = 0;
			StringBuilder emailBody = new StringBuilder("");
			SapConcurVo sapConcurtVo = sapConcurFileProcessing(fileName, fileReader, emailBody, vo);
			vo.setTotalFileRowCount(sapConcurtVo.getTotalFileRowCount());
			vo.setFileRowIndex(sapConcurtVo.getFileRowIndex());
			if (sapConcurtVo.getTotalFileRowCount().equals(initialCount)) {
				List<String> referenceNumbers = getReferenceNumberByFileId(vo.getFileId());
				vo.setEmailBody(setSapConcurEmailBody(referenceNumbers, emailBody));
				sendSapConcurReportMail(vo);
			}
		} catch (Exception e) {
			logger.error("Error in method sapConcurFileProcess for Data  {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			return vo;
		}
		return vo;
	}

	private void sendSapConcurReportMail(SapConcurVo vo) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setBody(vo.getEmailBody().toString());
		emailServiceVO.setSubject("Integration status report for SapConcur");
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String emailAddress = getSftpConfigurationValueAsString(Constants.SFTP_SAP_CONCUR_EMAIL_RECIPIENT);
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

	private List<String> getReferenceNumberByFileId(Integer fileId) {
		return sapConcurDao.getaccountCodeByFileId(fileId);
	}

	private SapConcurVo sapConcurFileProcessing(String fileName, BufferedReader fileReader, StringBuilder emailBody, SapConcurVo vo) {
		Integer fileId = vo.getFileId();
		Integer totalFileRowCount = vo.getTotalFileRowCount();
		Integer fileRowIndex = vo.getFileRowIndex();
		try {
			final int CONCUR_REFERENCE_NUMBER = 0;
			final int PERSON_ID = 1;
			final int DESTINATION_CITY = 2;
			final int DESTINATION_COUNTRY = 3;
			final int PARENT_EXPENSE_TYPE = 4;
			final int PURPOSE_OF_TRIP = 5;
			final int BUSINESS_PURPOSE = 6;
			final int COMMENT = 7;
			final int VISIT_START_DATE = 8;
			final int VISIT_END_DATE = 9;
			final int EVENT_START_DATE = 10;
			final int EVENT_END_DATE = 11;
			final int TOTAL_COLA = 12;
			final int DURATION = 13;
			int fileRowCount = excelityService.getRowCount(fileName);
			DateFormat fromDate = new SimpleDateFormat("dd-MM-yyyy"); // current format
			DateFormat toDate = new SimpleDateFormat("yyyy-MM-dd"); // wanted format
			String line = "";
			int minLineNumber = fileRowIndex;
			int maxLineNumber = 100;
			Pattern pattern = Pattern.compile("(0[1-9]|[1-2][0-9]|3[0-1])-(0[1-9]|1[0-2])-[0-9]{4} (2[0-3]|[01][0-9]):[0-5][0-9]");
			Pattern patternWithoutTime = Pattern.compile("\\d{2}-\\d{2}-\\d{4}");
			Matcher match;
			Matcher matchWithoutTime;
			int lineNumberToRead = maxLineNumber + minLineNumber;
			while ((line = fileReader.readLine()) != null) {
				String[] tokens = line.split(",", -1);
				if (tokens.length > 0) {
					while (tokens.length < 14) {
						minLineNumber++;
						--totalFileRowCount;
						String tempLine = fileReader.readLine();
						if (tempLine == null) {
							break;
						}
						line = line + tempLine;
						tokens = line.split(",", -1);
					}
					ConcurStaffTravelDtls concurStaffTravelDtls = new ConcurStaffTravelDtls();
					concurStaffTravelDtls.setConcurReferenceNumber(tokens[CONCUR_REFERENCE_NUMBER].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setPersonId(tokens[PERSON_ID].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setDestinationCity(tokens[DESTINATION_CITY].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setDestinationCountry(tokens[DESTINATION_COUNTRY].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setParentExpenseType(tokens[PARENT_EXPENSE_TYPE].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setPurposeOfTrip(tokens[PURPOSE_OF_TRIP].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setBusinessPurpose(tokens[BUSINESS_PURPOSE].replaceAll(REGULAR_EXPRESSION, ""));
					if (!tokens[COMMENT].equals("")) {
					concurStaffTravelDtls.setComment(tokens[COMMENT].replaceAll(REGULAR_EXPRESSION, ""));
					} else {
						concurStaffTravelDtls.setComment("");
					}
					if (!tokens[VISIT_START_DATE].equals("")) {
						match = pattern.matcher(tokens[VISIT_START_DATE]);
						matchWithoutTime = patternWithoutTime.matcher(tokens[VISIT_START_DATE]);
						if (match.find() || matchWithoutTime.find()) {
							concurStaffTravelDtls.setVisitStartDate(toDate.format(fromDate.parse(tokens[VISIT_START_DATE].replaceAll(REGULAR_EXPRESSION, ""))));
						} else {
							concurStaffTravelDtls.setVisitStartDate(tokens[VISIT_START_DATE].replaceAll(REGULAR_EXPRESSION, ""));
						}
					} else {
						concurStaffTravelDtls.setVisitStartDate(null);
					}
					if (!tokens[VISIT_END_DATE].equals("")) {
						match = pattern.matcher(tokens[VISIT_END_DATE]);
						matchWithoutTime = patternWithoutTime.matcher(tokens[VISIT_END_DATE]);
						if (match.find() || matchWithoutTime.find()) {
							concurStaffTravelDtls.setVisitEndDate(toDate.format(fromDate.parse(tokens[VISIT_END_DATE].replaceAll(REGULAR_EXPRESSION, ""))));
						} else {
							concurStaffTravelDtls.setVisitEndDate(tokens[VISIT_END_DATE].replaceAll(REGULAR_EXPRESSION, ""));
						}
					} else {
						concurStaffTravelDtls.setVisitEndDate(null);
					}
					if (!tokens[EVENT_START_DATE].equals("")) {
						match = pattern.matcher(tokens[EVENT_START_DATE]);
						matchWithoutTime = patternWithoutTime.matcher(tokens[EVENT_START_DATE]);
						if (match.find() || matchWithoutTime.find()) {
							concurStaffTravelDtls.setEventStartDate(toDate.format(fromDate.parse(tokens[EVENT_START_DATE].replaceAll(REGULAR_EXPRESSION, ""))));
						} else {
							concurStaffTravelDtls.setEventStartDate(tokens[EVENT_START_DATE].replaceAll(REGULAR_EXPRESSION, ""));
						}
					} else {
						concurStaffTravelDtls.setEventStartDate(null);
					}
					if (!tokens[EVENT_END_DATE].equals("")) {
						match = pattern.matcher(tokens[EVENT_END_DATE]);
						matchWithoutTime = patternWithoutTime.matcher(tokens[EVENT_END_DATE]);
						if (match.find() || matchWithoutTime.find()) {
							concurStaffTravelDtls.setEventEndDate(toDate.format(fromDate.parse(tokens[EVENT_END_DATE].replaceAll(REGULAR_EXPRESSION, ""))));
						} else {
							concurStaffTravelDtls.setEventEndDate(tokens[EVENT_END_DATE].replaceAll(REGULAR_EXPRESSION, ""));
						}
					} else {
						concurStaffTravelDtls.setEventEndDate(null);
					}
					concurStaffTravelDtls.setTotalCola(tokens[TOTAL_COLA].replaceAll(REGULAR_EXPRESSION, ""));
					concurStaffTravelDtls.setDuration(tokens[DURATION].replaceAll(REGULAR_EXPRESSION, ""));
					saveSapConcurData(emailBody, fileId, concurStaffTravelDtls, vo);
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
			if (totalFileRowCount < 0 || line == null) {
				totalFileRowCount = 0;
			}
			vo.setTotalFileRowCount(totalFileRowCount);
			vo.setFileRowIndex(minLineNumber);
		} catch (Exception e) {
			logger.error("Error in method sapConcurFileProcessing", e);
			vo.setErrorOccured(Boolean.TRUE);
			emailBody.append("Error in sapConcurFileProcessing : <br/>").append(e).append("<br/>");
		}
		return vo;
	}

	private void saveSapConcurData(StringBuilder emailBody, Integer fileId, ConcurStaffTravelDtls concurStaffTravelDtls, SapConcurVo vo) {
		try {
			concurStaffTravelDtls.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			concurStaffTravelDtls.setFileId(fileId);
			concurStaffTravelDtls.setUpdateUser(UPDATE_USER);			
			concurStaffTravelDtls.setRemarks("Success");			
			sapConcurDao.saveSapConcurSftp(concurStaffTravelDtls);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in method saveSapConcurData", e);
			vo.setErrorOccured(Boolean.TRUE);
			emailBody.append("Error in saveSapConcurData : <br/>").append(e).append("<br/>");
		}
	}

	private StringBuilder setSapConcurEmailBody(List<String> referenceNumbers, StringBuilder emailBody) {
		emailBody.append(EMAILBODY_STRUCTURE)
				.append(getSftpConfigurationValueAsString(Constants.RISE_SAP_CONCUR_OUTBOUND_ARCHIVE))
				.append(EMAILBODY_FILE_LOC);
		emailBody.append("<br/>Successful Interface for: <br/>");
		if (referenceNumbers != null && !referenceNumbers.isEmpty()) {
			for (String accountNumber : referenceNumbers) {
				emailBody.append(" Concur Reference Number : ").append(accountNumber).append("<br/><br/>");
			}
		}
		return emailBody;
	}

	@Override
	public String getSftpConfigurationValueAsString(String parameterName) {
		SftpConfigurationData sftpConfigurationData = sftpConfigurationService.getSFTPConfigurationValue(parameterName);
		return sftpConfigurationData.getConfigurationValue();
	}

}
