package com.polus.fibicomp.externaluser.service;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externaluser.dao.ExternalUserDao;
import com.polus.fibicomp.externaluser.pojo.ExternalUser;
import com.polus.fibicomp.externaluser.pojo.ExternalUserFeed;
import com.polus.fibicomp.externaluser.vo.HomeVo;
import com.polus.fibicomp.login.dao.LoginDao;
import com.polus.fibicomp.login.pojo.PersonLoginDetail;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "externalUserService")
public class ExternalUserServiceImpl implements ExternalUserService {

	protected static Logger logger = LogManager.getLogger(ExternalUserServiceImpl.class.getName());

	@Autowired
	private ExternalUserDao externalUserDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private DateTimeService dateTimeService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonService commonService;

	@Autowired
	private SftpConfigurationService sftpConfigurationService;

	@Autowired
	private ExcelityService excelityService;
	
	@Autowired
	private LoginDao loginDao;
	
	@Value("${oracledb}")
	private String oracledb;

	private static final String EMAILBODY_STRUCTURE = "Please go to ";
	private static final String EMAILBODY_FILE_LOC = "  to view the files<br/>";

	@Override
	public String fetchExternalUserDetails(HomeVo vo) {
		List<ExternalUser> externalUser = null;
		String personId = AuthenticatedUser.getLoginPersonId();
		List<String> units = externalUserDao.getUnitsListByPersonIdAndRights(personId);
		if (units != null && !units.isEmpty()) {
			externalUser = externalUserDao.fetchAllExternalUserDetails(units);
			if (externalUser != null && !externalUser.isEmpty()) {
				for (ExternalUser user : externalUser) {
					user.setFundingOfficeName(commonDao.getUnitByUnitNumber(user.getFundingOffice()).getUnitName());
					user.setOrgnanizationName(commonDao.loadOrganizationDetails(user.getOrganizationId()).getOrganizationName());
				}
			}
		}
		return commonDao.convertObjectToJSON(externalUser);
	}

	@Override
	public String updateFlagByPersonId(HomeVo vo, String userName) {
		ExternalUser externalUser = externalUserDao.getExternalUserByPersonId(vo.getPersonId());
		if (externalUser != null && vo.getVerifiedFlag() != null) {
			externalUser.setVerifiedBy(userName);
			externalUser.setVerifiedFlag(vo.getVerifiedFlag());
			externalUser.setVerifiedAt(dateTimeService.getCurrentTimestamp());
			externalUser.setUpdatedTimestamp(dateTimeService.getCurrentTimestamp());
			externalUser.setAdminComment(vo.getAdminComments());
			externalUserDao.updateApproveReject(externalUser);
		}
		if (vo.getVerifiedFlag().equals(Constants.APPROVED_USER)) {
			Person person = new Person();
			String nextSequenceId = getNextSeqMySql( "SELECT IFNULL(MAX(CONVERT(PERSON_ID, SIGNED INTEGER)),100)+1 FROM PERSON");
			logger.info("getNextSeq : " + nextSequenceId);
			person.setPersonId(nextSequenceId);
			person.setFullName(externalUser.getFullName());
			person.setPrincipalName(externalUser.getUserName());
			person.setEmailAddress(externalUser.getEmailAddress());
			String homeUnit = externalUserDao.getHomeUnitFromOrganizationId(externalUser.getOrganizationId());
			if (homeUnit != null) {
				person.setHomeUnit(homeUnit);
			} else {
				person.setHomeUnit(Constants.EXTERNAL_USER_HOME_UNIT);
			}
			try {
				person.setPassword(commonService.hash(externalUser.getPassword()));
			} catch (GeneralSecurityException e) {
				logger.error("Exception in Hasing: {}", e);
				e.printStackTrace();
			}
			person.setUpdateTimestamp(dateTimeService.getCurrentTimestamp());
			person.setIsExternalUser(true);
			person.setUpdateUser(userName);
			person.setStatus(Constants.APPROVED_USER);
			if (externalUser.getIsSGAFUser() != null && externalUser.getIsSGAFUser()) {
				sendApprovedSGAFUserNotification(externalUser);
			} else {
				saveExternalUserFeed(externalUser, nextSequenceId, person.getStatus(), userName);
			}
			personDao.saveOrUpdatePerson(person);
			externalUserDao.assignPersonRole(person, Constants.EXTERNAL_USER_ACCESS);
			externalUserDao.assignPersonRoleRT(person, Constants.EXTERNAL_USER_ACCESS);
		} else if (vo.getVerifiedFlag().equals(Constants.REJECTED_USER)) {
			sendRejectedNotificatoinExternalUser(externalUser);
		} else if (vo.getVerifiedFlag().equals(Constants.DEACTIVATED_USER)) {
			Person person = personDao.getPersonDetailByPrincipalName(vo.getUserName());
			if (person != null) {
				person.setStatus(Constants.INACTIVE_USER);
				saveExternalUserFeed(externalUser, person.getPersonId(), person.getStatus(), userName);
				personDao.saveOrUpdatePerson(person);
			}
		}
		externalUser.setFundingOfficeName(commonDao.getUnitByUnitNumber(externalUser.getFundingOffice()).getUnitName());
		externalUser.setOrgnanizationName(commonDao.loadOrganizationDetails(externalUser.getOrganizationId()).getOrganizationName());
		return commonDao.convertObjectToJSON(externalUser);
	}

	private void sendApprovedSGAFUserNotification(ExternalUser externalUser) {
		logger.info("Requesting for send Approved SFAG User Notification");
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.EXTERNAL_SGAF_USER_APPROVED_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.EXTERNAL_USER_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.EXTERNAL_USER_SUB_MODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(externalUser.getPersonId().toString());
		commonService.setNotificationRecipientsforNonEmployees(externalUser.getEmailAddress(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		emailServiceVO.setRecipients(dynamicEmailrecipients);
		emailService.sendEmail(emailServiceVO);		
	}

	private void saveExternalUserFeed(ExternalUser externalUser, String nextSequenceId, String status, String userName) {
		ExternalUserFeed userFeed = new ExternalUserFeed();
		userFeed.setPersonId(nextSequenceId);
		if (status.equals(Constants.APPROVED_USER)) {
			userFeed.setAction(Constants.ADD);
		} else if (status.equals(Constants.INACTIVE_USER)) {
			userFeed.setAction(Constants.DELETE);
		}
		userFeed.setUserName(externalUser.getUserName());
		userFeed.setFullName(externalUser.getFullName());
		userFeed.setPassword(externalUser.getPassword());
		userFeed.setEmailAddress(externalUser.getEmailAddress()); 
		userFeed.setOrganizationName(commonDao.loadOrganizationDetails(externalUser.getOrganizationId()).getOrganizationName());
		userFeed.setType("STAFF3");
		userFeed.setIsSent(Constants.NO);
		userFeed.setUpdateTimeStamp(dateTimeService.getCurrentTimestamp());
		userFeed.setUpdateUser(userName);
		externalUserDao.saveExternalUserFeed(userFeed);
	}

	private void sendRejectedNotificatoinExternalUser(ExternalUser externalUser) {
		logger.info("Requesting for send Rejected Notification ExternalUser");
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.EXTERNAL_USER_REJECTED_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.EXTERNAL_USER_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.EXTERNAL_USER_SUB_MODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(externalUser.getPersonId().toString());
		commonService.setNotificationRecipientsforNonEmployees(externalUser.getEmailAddress(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		emailServiceVO.setRecipients(dynamicEmailrecipients);
		emailService.sendEmail(emailServiceVO);
	}

	private String getNextSeqMySql(String sql) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		Statement statement;
		String nextSeq = null;
		try {
			statement = connection.createStatement();
			ResultSet rs = statement.executeQuery(sql);
			if (rs.next()) {
				nextSeq = rs.getString(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return nextSeq;
	}

	@Scheduled(cron = "${external.user.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void getExternalUserFeedDetails() {
		logger.info("Requesting for Scheduler External User Feed");
		HomeVo homeVo = new HomeVo();
		String externalFeedAdd = "ADD-" + DateTime.now().toString("yyyyMMdd");
		String externalFeedDelete = "REMOVE-" + DateTime.now().toString("yyyyMMdd");
		StringBuilder emailBodyAdd = new StringBuilder("");
		StringBuilder emailBodyDelete = new StringBuilder("");
		StringBuilder emailBody = new StringBuilder("");
		externalUserFeedCSVForAdd(externalFeedAdd, emailBodyAdd, homeVo);
		externalUserFeedCSVForDelete(externalFeedDelete, emailBodyDelete, homeVo);
		if (!homeVo.getErrorOccured()) {
			emailBody.append(EMAILBODY_STRUCTURE).append(excelityService.getSftpConfigurationValueAsString(Constants.RISE_EXTERNAL_USER_FEED)).append(EMAILBODY_FILE_LOC);
			emailBody.append("<br/>Successful Interface for: <br/>");
			if (emailBodyAdd.length() != 0) {
				emailBody.append("Add External User Details").append("<br/>");
				emailBody.append(emailBodyAdd).append("<br/>");
			}
			if (emailBodyDelete.length() != 0) {
				emailBody.append("Remove External User Details").append("<br/>");
				emailBody.append(emailBodyDelete).append("<br/>");
			}
			if (emailBodyAdd.length() != 0 || emailBodyDelete.length() != 0) {
				sendExternaUserFeedMail(emailBody);
			}
		} else {
			sendExternaUserFeedMail(homeVo.getEmailContent().getError());
		}
	}

	private void sendExternaUserFeedMail(StringBuilder emailBody) {
		logger.info("Requesting for sendExternaUserFeedMail");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setSubject("External User Feed - Report");
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		emailServiceVO.setBody(emailBody.toString());
		String emailAddress = excelityService.getSftpConfigurationValueAsString(Constants.SFTP_EXTERNAL_USER_FEED_EMAIL_RECIPIENT);
		if (emailAddress != null && !emailAddress.isEmpty()) {
			String[] singleEmailAddress = emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
			if (singleEmailAddress.length > 0) {
				for (String recipeientMailAddress : singleEmailAddress) {
					commonService.setNotificationRecipientsforNonEmployees(recipeientMailAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
				}
			}
			emailServiceVO.setRecipients(dynamicEmailrecipients);
			emailService.sendEmail(emailServiceVO);
		}
	}

	private void externalUserFeedCSVForDelete(String externalFeedDelete, StringBuilder emailBodyDelete, HomeVo homeVo) {
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			XSSFSheet sheet = workbook.createSheet("DELETE");
			List<ExternalUserFeed> userFeedDetails = externalUserDao.getExternalUserFeedDetailsForDelete();
			if (userFeedDetails != null && !userFeedDetails.isEmpty()) {
				createExternalUserFeedReportForDelete(externalFeedDelete, userFeedDetails, workbook, sheet, emailBodyDelete);
				ByteArrayOutputStream bos = new ByteArrayOutputStream();
				workbook.write(bos);
				byte[] byteArray = null;
				byteArray = bos.toByteArray();
				byteArray = convertXlsxToCsvFile(byteArray);
				File file = createFile(externalFeedDelete);
				try (OutputStream os = new FileOutputStream(file)) {
					os.write(byteArray);
				} catch (Exception e) {
					logger.info("error occured while writing file to staff3 folder for Delete", e.getMessage());
					homeVo.getEmailContent().getError().append("Error while writing file to staff3 folder for Delete : <br/>").append(e).append("<br/>");
					homeVo.setErrorOccured(Boolean.TRUE);
				}
				rename(file);
			}
		} catch (Exception e) {
			logger.info("error in externalUserFeedToExcel", e.getMessage());
			homeVo.getEmailContent().getError().append("Error in External User feed file generation for Delete : <br/>").append(e).append("<br/>");
			homeVo.setErrorOccured(Boolean.TRUE);
		}
	}

	private void externalUserFeedCSVForAdd(String externalFeedName, StringBuilder emailBodyAdd, HomeVo homeVo) {
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			XSSFSheet sheetAdd = workbook.createSheet("ADD");
			List<ExternalUserFeed> userFeedAdd = externalUserDao.getExternalUserFeedDetailsForAdd();
			if (userFeedAdd != null && !userFeedAdd.isEmpty()) {
				createExternalUserFeedReportForAdd(externalFeedName, userFeedAdd, workbook, sheetAdd, emailBodyAdd);
				ByteArrayOutputStream bos = new ByteArrayOutputStream();
				workbook.write(bos);
				byte[] byteArray = null;
				byteArray = bos.toByteArray();
				byteArray = convertXlsxToCsvFile(byteArray);
				File file = createFile(externalFeedName);
				try (OutputStream os = new FileOutputStream(file)) {
					os.write(byteArray);
				} catch (Exception e) {
					logger.info("error occured while writing file to staff3 folder for Add", e.getMessage());
					homeVo.getEmailContent().getError().append("Error while writing file to staff3 folder for Add : <br/>").append(e).append("<br/>");
					homeVo.setErrorOccured(Boolean.TRUE);
				}
				rename(file);
			}
		} catch (Exception e) {
			logger.info("error in externalUserFeedCSVForAdd", e.getMessage());
			homeVo.getEmailContent().getError().append("Error in External User Feed file generation for Add : <br/>").append(e).append("<br/>");
			homeVo.setErrorOccured(Boolean.TRUE);
		}
	}

	private String rename(File file) throws IOException {
		String files = file.getAbsolutePath();
		String newfiles = files.substring(0, files.lastIndexOf("."));
		newfiles = newfiles + ".csv";
		String str = newfiles;
		if (file.renameTo(new File(str))) {
			return str;
		} else {
			return files;
		}
	}

	private File createFile(String externalFeedName) {
		BufferedWriter fileOutputStream = null;
		try {
			String filepath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_EXTERNAL_USER_FEED);
			String fileNameWithPath = new StringBuilder(filepath).append(File.separator).append(externalFeedName).append(".tmp").toString();
			File file = new File(fileNameWithPath);
			fileOutputStream = new BufferedWriter(new FileWriter(fileNameWithPath));
			fileOutputStream.close();
			return file;
		} catch (Exception e) {
			logger.error("Exception in method createFile of ExternalUserServiceImpl: {} ", e);
			e.printStackTrace();
			return null;
		} finally {
			try {
				fileOutputStream.close();
			} catch (IOException e) {
				logger.error("Exception in method createFile while closing the stream : {} ", e);
				e.printStackTrace();
			}
		}
	}

	private byte[] convertXlsxToCsvFile(byte[] data) {
		StringBuilder byteArray = new StringBuilder();
		InputStream inputStream = new ByteArrayInputStream(data);
		try {
			Workbook workbook = WorkbookFactory.create(inputStream);
			Sheet sheet = workbook.getSheetAt(0);
			Row row = null;
			for (int i = 0; i < sheet.getLastRowNum() + 1; i++) {
				row = sheet.getRow(i);
				StringBuilder rowDetail = new StringBuilder();
				if (sheet.getRow(i).getPhysicalNumberOfCells() > 0) {
					for (int j = 0; j < sheet.getRow(i).getPhysicalNumberOfCells(); j++) {
						String newData = null;
						if (row.getCell(j) != null)
							newData = escapeSpecialCharactersFromData(row.getCell(j).toString());
						if (i == 0 && newData != null) {
							rowDetail = rowDetail.append(newData).append(";");
						} else {
							if (row.getCell(j) == null) {
								rowDetail = rowDetail.append('"').append('"').append(';');
							} else {
								rowDetail = rowDetail.append(newData).append(";");
							}
						}
					}
					byteArray = byteArray.append(rowDetail.substring(0, rowDetail.length() - 1)).append("\n");
				}
			}
			workbook.close();
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in convertXlsxToCsvFile : {}", e.getMessage());
		}
		return byteArray.toString().getBytes(StandardCharsets.UTF_8);
	}

	public String escapeSpecialCharactersFromData(String data) {
		String escapedData = data.replaceAll("\\R", " ");
		escapedData = escapedData.replaceAll("[\\r\\n]+", " ");
		if (escapedData.contains(",") || escapedData.contains("\"") || escapedData.contains("'") || escapedData.equals("")) {
			escapedData = escapedData.replace("\"", "\"\"");
			escapedData = "\"" + escapedData + "\"";
		}
		return escapedData;
	}

	private XSSFSheet createExternalUserFeedReportForDelete(String externalFeedName, List<ExternalUserFeed> userFeed,
			XSSFWorkbook workbook, XSSFSheet sheetDelete, StringBuilder emailBodyDelete) {
		commonService.addDetailsInHeader(workbook, sheetDelete);
		Object[] tableHeadingRowsForDelete = { "Action", "Username", "FullName" };
		int rowNumber = 0;
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		prepareExcelSheetHeader(rowNumber, sheetDelete, tableHeadingRowsForDelete, null, externalFeedName, workbook, tableBodyStyle);
		prepareExcelDataForDelete(++rowNumber, userFeed, workbook, sheetDelete, tableBodyStyle, emailBodyDelete);
		return sheetDelete;
	}

	private void prepareExcelDataForDelete(int rowNumber, List<ExternalUserFeed> userFeed, XSSFWorkbook workbook,
			XSSFSheet sheetDelete, XSSFCellStyle tableBodyStyle, StringBuilder emailBodyDelete) {
		if (userFeed != null && !userFeed.isEmpty()) {
			for (ExternalUserFeed userDetails : userFeed) {
				Row row = sheetDelete.createRow(rowNumber++);
				int cellNumber = 0;
				Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getAction() != null)
					cell1.setCellValue(userDetails.getAction());
				else
					cell1.setCellValue(" ");
				cellNumber++;

				Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getUserName() != null) {
					cell2.setCellValue(userDetails.getUserName());
					emailBodyDelete.append("UserName: ").append(userDetails.getUserName()).append("<br/>");
				} else {
					cell2.setCellValue(" ");
				}
				cellNumber++;

				Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getFullName() != null)
					cell3.setCellValue(userDetails.getFullName());
				else
					cell3.setCellValue(" ");
				cellNumber++;
				userDetails.setIsSent(Constants.EXTERNAL_USER_FEED_STATUS);
				userDetails.setUpdateTimeStamp(dateTimeService.getCurrentTimestamp());
				externalUserDao.saveOrUpdate(userDetails);
			}
		}
	}

	private XSSFSheet createExternalUserFeedReportForAdd(String externalFeedName, List<ExternalUserFeed> userFeed,
			XSSFWorkbook workbook, XSSFSheet sheet, StringBuilder emailBodyAdd) {
		commonService.addDetailsInHeader(workbook, sheet);
		Object[] tableHeadingRowsForAdd = { "Action", "Username", "FullName", "Password", "Email", "Company", "ForceChangePassword", "Type" };
		int rowNumber = 0;
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		prepareExcelSheetHeader(rowNumber, sheet, tableHeadingRowsForAdd, null, externalFeedName, workbook, tableBodyStyle);
		prepareExcelData(++rowNumber, userFeed, workbook, sheet, tableBodyStyle, emailBodyAdd);
		return sheet;
	}

	private void prepareExcelData(int rowNumber, List<ExternalUserFeed> userFeed, XSSFWorkbook workbook,
			XSSFSheet sheet, XSSFCellStyle tableBodyStyle, StringBuilder emailBodyAdd) {
		if (userFeed != null && !userFeed.isEmpty()) {
			for (ExternalUserFeed userDetails : userFeed) {
				Row row = sheet.createRow(rowNumber++);
				int cellNumber = 0;
				Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getAction() != null)
					cell1.setCellValue(userDetails.getAction());
				else
					cell1.setCellValue(" ");
				cellNumber++;

				Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getUserName() != null) {
					cell2.setCellValue(userDetails.getUserName());
					emailBodyAdd.append("UserName: ").append(userDetails.getUserName()).append("<br/>");
				} else {
					cell2.setCellValue(" ");
				}
				cellNumber++;

				Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getFullName() != null)
					cell3.setCellValue(userDetails.getFullName());
				else
					cell3.setCellValue(" ");
				cellNumber++;

				Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getPassword() != null)
					cell4.setCellValue(userDetails.getPassword());
				else
					cell4.setCellValue(" ");
				cellNumber++;

				Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getEmailAddress() != null)
					cell5.setCellValue(userDetails.getEmailAddress());
				else
					cell5.setCellValue(" ");
				cellNumber++;

				Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getOrganizationName() != null) {
					cell6.setCellValue(userDetails.getOrganizationName());
				} else {
					cell6.setCellValue(" ");
				}
				cellNumber++;

				Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
				cell7.setCellValue("TRUE");
				cellNumber++;

				Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
				if (userDetails.getType() != null) {
					cell8.setCellValue(userDetails.getType());
				} else {
					cell8.setCellValue(" ");
				}
				cellNumber++;
				userDetails.setIsSent(Constants.EXTERNAL_USER_FEED_STATUS);
				userDetails.setUpdateTimeStamp(dateTimeService.getCurrentTimestamp());
				externalUserDao.saveOrUpdate(userDetails);
			}
		}
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	private XSSFWorkbook prepareExcelSheetHeader(int rowNumber, XSSFSheet sheet, Object[] tableHeadingRowsForAdd,
			String externalFeedName, String documentHeading, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle) {
		int headingCellNumber = 0;
		Row tableHeadRow = sheet.createRow(rowNumber++);
		Cell headingCell = tableHeadRow.createCell(0);
		headingCell.setCellValue((String) documentHeading);
		// Set table head data to each column.
		for (Object heading : tableHeadingRowsForAdd) {
			Cell cell = tableHeadRow.createCell(headingCellNumber++);
			cell.setCellValue((String) heading);
		}
		return workbook;
	}

	@Scheduled(cron = "${external.user.approvedmail.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void externalUserApprovedMail() {
		logger.info("Requesting Scheduler for Approved External User mail");
		List<ExternalUserFeed> userFeedDetails = externalUserDao.getExternalUserFeeds();
		if (userFeedDetails != null && !userFeedDetails.isEmpty()) {
			for (ExternalUserFeed externalFeed : userFeedDetails) {
				logger.info("Approved Mail user name : {}", externalFeed.getUserName());
				ExternalUser externalUser = externalUserDao.getExternalUserByUserName(externalFeed.getUserName());
				if (externalUser != null && externalUser.getPersonId() != null) {
					sendExternalUserApprovedMail(externalUser);
					sendExternalUserPasswordMail(externalUser);
				}
				externalFeed.setIsSent(Constants.EXTERNAL_USER_MAIL_SENT);
				externalFeed.setUpdateTimeStamp(dateTimeService.getCurrentTimestamp());
				externalUserDao.saveExternalUserFeed(externalFeed);
			}
		}
	}

	private void sendExternalUserPasswordMail(ExternalUser externalUser) {
		logger.info("Requesting for send ExternalUser Password Mail");
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.EXTERNAL_USER_PASSWORD_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.EXTERNAL_USER_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.EXTERNAL_USER_SUB_MODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(externalUser.getPersonId().toString());
		commonService.setNotificationRecipientsforNonEmployees(externalUser.getEmailAddress(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		emailServiceVO.setRecipients(dynamicEmailrecipients);
		emailService.sendEmail(emailServiceVO);
	}

	private void sendExternalUserApprovedMail(ExternalUser externalUser) {
		logger.info("Requesting for send ExternalUser Approved Mail");
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.EXTERNAL_USER_APPROVED_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.EXTERNAL_USER_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.EXTERNAL_USER_SUB_MODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(externalUser.getPersonId().toString());
		commonService.setNotificationRecipientsforNonEmployees(externalUser.getEmailAddress(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
		emailServiceVO.setRecipients(dynamicEmailrecipients);
		emailService.sendEmail(emailServiceVO);
	}

	@Override
	public String fetchExternalUserLoginDetails(HomeVo vo) {
		String login = "";
		PersonLoginDetail loginDetails = loginDao.getRecentPersonLoginDetailByUserName(vo.getUserName());
		if (loginDetails != null) {
			return commonDao.convertObjectToJSON(loginDetails);
		}
		return commonDao.convertObjectToJSON(login);
	}

	@Scheduled(cron = "${external.registeredUser.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendRegisteredExternalUser() {
		logger.info("Requesting Scheduler for Registered External User List to GM");
		Set<String> fundingUnits = new HashSet<>();
		List<ExternalUser> externalUserList = externalUserDao.getPendingExternalUserList();
		if (externalUserList != null && !externalUserList.isEmpty()) {
			for (ExternalUser externalUser : externalUserList) {
				fundingUnits.add(externalUser.getFundingOffice());
			}
			for (String unit : fundingUnits) {
				if (unit != null && !unit.isEmpty()) {
					StringBuilder mailContent = new StringBuilder("");
					Integer personId = null;
					for (ExternalUser externalUser : externalUserList) {
						if (unit.equals(externalUser.getFundingOffice())) {
							mailContent.append("User Name: ").append(externalUser.getUserName() != null ? externalUser.getUserName() : "")
									.append("<br/>").append("Email Address: ").append(externalUser.getEmailAddress() != null ? externalUser.getEmailAddress() : "")
									.append("<br/><br/>");
							externalUser.setIsMailSent("Y");
							externalUser.setUpdatedTimestamp(dateTimeService.getCurrentTimestamp());
							externalUserDao.updateApproveReject(externalUser);
							if (personId == null) {
								personId = externalUser.getPersonId();
							}
						}
					}
					sendMailToGrantManager(mailContent.toString(), personId);
				}
			}
		}
	}

	private void sendMailToGrantManager(String mailContent, Integer personId) {
		logger.info("Requesting for send Mail To GrantManager");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		Map<String, String> placeHolder = new HashMap<>();
		emailServiceVO.setNotificationTypeId(Constants.EXTERNAL_USER_GRANT_MANAGER_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.EXTERNAL_USER_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.EXTERNAL_USER_SUB_MODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(personId.toString());
		placeHolder.put("{EXTERNAL_USER_LIST}", mailContent.toString());
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
	}

}
