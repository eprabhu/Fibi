package com.polus.fibicomp.claims.claimsIntegration.ics.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import com.polus.fibicomp.claims.claimsIntegration.ics.dao.IcsDao;
import com.polus.fibicomp.claims.claimsIntegration.ics.pojo.IcsStudentTravelDetail;
import com.polus.fibicomp.codetable.dao.JSONParser;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Service(value = "icsService")
public class IcsServiceImpl implements IcsService {

	protected static Logger logger = LogManager.getLogger(IcsServiceImpl.class.getName());

	@Autowired
	private IcsDao icsDao;
	
	@Autowired
	private CommonDao commonDao;

	@Value("${claims.student.travel.api}")
	private String claimStudentTravelApi;
	
	@Value("${claims.student.travel.api.token}")
	private String claimStudentTravelAccessToken;
	
	@Value("${claims.student.travel.api.token.type}")
	private String claimStudentTravelAccessTokenType;

	@Value("${system.timezone}")
	private String timezone;

	@Value("${log.filepath}")
	private String filePath;

	@Value("${claim.student.travel.retrycount}")
	private Integer claimStudentTravelRetryCount;

	@Autowired
	private EmailService emailService;

	private static final String UPDATE_USER = "quickstart";

	@Override
	public void claimStudentTravelIntegration(String dateValue, String endDate) {
		int count = 0;
		Integer responseCode = 0;
		Integer totalClaimTravelCount = 0;
		StringBuilder failedMessage = new StringBuilder();
		StringBuilder message = new StringBuilder();
		StringBuilder logMessage = new StringBuilder();
		try {
			logger.info("Claim student travel API execution starts at : {}", commonDao.getCurrentTimestamp());
			logDetailsInFile("Claim student travel API call starts at");
			HttpURLConnection connection = null;
			prepareIcsData(dateValue, endDate, connection, count, responseCode, totalClaimTravelCount, failedMessage, message, logMessage);
			logDetailsInFile("Processing of claim student travel records to Fibi completed at");
			logger.info("Processing of claim student travel records to Fibi completed at: {} ",
					commonDao.getCurrentTimestamp());
		} catch (Exception e) {
			logDetailsInFile(logMessage.append("Exception occured:").append(e.toString()).append("at").toString());
			message.append("<br/> Exception occured : " + e);
			logger.info("failed {}", e.getMessage());
		}
		sendICSIntegrationNotification(message, totalClaimTravelCount);
	}

	private void prepareIcsData(String dateProcessed, String endDate, HttpURLConnection connection, int count, Integer responseCode, Integer totalClaimTravelCount, StringBuilder failedMessage, StringBuilder message, StringBuilder logMessage) {
	 try {
		do {
			try {
				failedMessage.append("<br/> API call failed due to :");
				logger.info("API calling count : {}", count);
				connection = icsApiConnection(connection, message, dateProcessed, endDate);
				responseCode = connection.getResponseCode();
				icsIntegrationFailedMessage(logMessage, responseCode, count, message, failedMessage);
			} catch (Exception e) {
				count = count + 1;
				message.append(e);
			}
		} while ((responseCode != 200) && (count < claimStudentTravelRetryCount));

		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader((connection.getInputStream())));
		String inputLine = "";
		StringBuilder response = new StringBuilder();
		while ((inputLine = bufferedReader.readLine()) != null) {
			response.append(inputLine);
		}
		bufferedReader.close();
		String apiResponseData = response.toString();
		JSONArray jsonData = new JSONArray(apiResponseData);
		List<Object> claimsDetail = JSONParser.toList(jsonData);
		int totalCountofData = claimsDetail.size();
		logger.info("Total count of data {}", totalCountofData);
		logDetailsInFile(logMessage.append("Total count of claim student travel student from API: ").append(totalCountofData).append(" at").toString());
		logger.info("Processing of claim student travel API records to Fibi starts at: {}", commonDao.getCurrentTimestamp());
		totalClaimTravelCount = saveIcsIntegrationDetails(totalCountofData, totalClaimTravelCount, claimsDetail);
		logger.info("totalClaimTravelCount {}", + totalClaimTravelCount);
		connection.disconnect();
	   } catch (MalformedURLException e) {
			logger.info("failed {}", e.getMessage());
			logDetailsInFile(logMessage.append("MalformedURLException").append(e.toString()).append("at").toString());
			message.append(("<br/> MalformedURLException : ").concat(e.toString()));
	   } catch (Exception e) {
		   logger.info("failed {}", e.getMessage());
			logDetailsInFile(logMessage.append("Exception in prepareIcsData").append(e.toString()).append("at").toString());
			message.append(("<br/> Exception in prepareIcsData: ").concat(e.toString()));
	   }
	}

	private void sendICSIntegrationNotification(StringBuilder message, Integer totalClaimTravelCount) {
		logger.info("sending ICS Integration Notification");
		sendClaimIntegrationNotification(totalClaimTravelCount, message, Constants.ICS_INTEGRATION_NOTIFICATION_CODE, new HashSet<>());
	}

	private void sendClaimIntegrationNotification(Integer totalClaimTravelCount, StringBuilder message, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.CLAIM_MODULE_CODE);
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(message.toString(), totalClaimTravelCount));
		emailServiceVO.setSubModuleCode(Constants.CLAIM_SUBMODULE_CODE.toString());
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getDynamicPlaceholders(String message, Integer totalClaimTravelCount) {
		Map<String, String> placeHolder = new HashMap<>();
		if(totalClaimTravelCount != 0) {
		  message = message.concat("<br/> Claim student travel integration processed");	
		}
		placeHolder.put("{MESSAGE}", message);
		placeHolder.put("{SUCCESS_COUNT}", totalClaimTravelCount != 0 ? totalClaimTravelCount.toString() : "0");
		return placeHolder;
	}

	@SuppressWarnings("unchecked")
	private Integer saveIcsIntegrationDetails(Integer totalCountofData, Integer totalClaimTravelCount, List<Object> claimsDetail) {
		for (int index = 0; index < totalCountofData; index++) {
			IcsStudentTravelDetail claimStudentTravelDetail = new IcsStudentTravelDetail();
			Map<String, Object> hmResult = (HashMap<String, Object>) claimsDetail.get(index);
			String personsId = hmResult.get("requesteR_ID").toString();
			if (personsId != null && !personsId.equalsIgnoreCase("null") && !personsId.isEmpty()) {
				claimStudentTravelDetail.setPersonId(personsId);
			}
			String icsReferenceNumber = hmResult.get("referencE_NO").toString();
			if (icsReferenceNumber != null && !icsReferenceNumber.equalsIgnoreCase("null")
					&& !icsReferenceNumber.isEmpty()) {
				claimStudentTravelDetail.setIcsReferenceNumber(icsReferenceNumber);
			}
			String country = hmResult.get("country").toString();
			if (country != null && !country.equalsIgnoreCase("null") && !country.isEmpty()) {
				claimStudentTravelDetail.setCountry(country);
			}
			String purposeHeader = hmResult.get("purpose_Header").toString();
			if (purposeHeader != null && !purposeHeader.equalsIgnoreCase("null") && !purposeHeader.isEmpty()) {
				claimStudentTravelDetail.setPurposeHeader(purposeHeader);
			}
			String purposeOfTrip = hmResult.get("purposE_OF_TRIP").toString();
			if (purposeOfTrip != null && !purposeOfTrip.equalsIgnoreCase("null") && !purposeOfTrip.isEmpty()) {
				claimStudentTravelDetail.setPurposeOfTrip(purposeOfTrip);
			}
			String title = hmResult.get("title").toString();
			if (title != null && !title.equalsIgnoreCase("null") && !title.isEmpty()) {
				claimStudentTravelDetail.setTitle(title);
			}
			String visitStartDate = hmResult.get("froM_DATE").toString();
			if (visitStartDate != null && !visitStartDate.equalsIgnoreCase("null") && !visitStartDate.isEmpty()) {
				claimStudentTravelDetail.setVisitStartDate(Timestamp.valueOf(formatTimestamp(visitStartDate)));
			}
			String visitEndDate = hmResult.get("tO_DATE").toString();
			if (visitEndDate != null && !visitEndDate.equalsIgnoreCase("null") && !visitEndDate.isEmpty()) {
				claimStudentTravelDetail.setVisitEndDate(Timestamp.valueOf(formatTimestamp(visitEndDate)));
			}
			String eventStartDate = hmResult.get("officiaL_DUTIES_FROM_DATE").toString();
			if (eventStartDate != null && !eventStartDate.equalsIgnoreCase("null") && !eventStartDate.isEmpty()) {
				claimStudentTravelDetail.setEventStartDate(Timestamp.valueOf(formatTimestamp(eventStartDate)));
			}
			String eventEndDate = hmResult.get("officiaL_DUTIES_TO_DATE").toString();
			if (eventEndDate != null && !eventEndDate.equalsIgnoreCase("null") && !eventEndDate.isEmpty()) {
				claimStudentTravelDetail.setEventEndDate(Timestamp.valueOf(formatTimestamp(eventEndDate)));
			}
			String saRate = hmResult.get("sA_RATE").toString();
			if (saRate != null && !saRate.equalsIgnoreCase("null") && !saRate.isEmpty()) {
				claimStudentTravelDetail.setSaRate(new BigDecimal(saRate));
			}
			String receiptAmount = hmResult.get("receipT_AMT_SGD").toString();
			if (receiptAmount != null && !receiptAmount.equalsIgnoreCase("null") && !receiptAmount.isEmpty()) {
				claimStudentTravelDetail.setReceiptAmount(new BigDecimal(receiptAmount));
			}
			String claimItemDescription = hmResult.get("claiM_ITEM_DESC").toString();
			if (claimItemDescription != null && !claimItemDescription.equalsIgnoreCase("null")
					&& !claimItemDescription.isEmpty()) {
				claimStudentTravelDetail.setClaimItemDescription(claimItemDescription);
			}
			String claimType = hmResult.get("claiM_TYPE").toString();
			if (claimType != null && !claimType.equalsIgnoreCase("null") && !claimType.isEmpty()) {
				claimStudentTravelDetail.setClaimType(claimType);
			}
			claimStudentTravelDetail.setUpdateUser(UPDATE_USER);
			claimStudentTravelDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			icsDao.saveOrUpdateClaimStudentTravelDetail(claimStudentTravelDetail);
			if (claimStudentTravelDetail.getIcsReferenceNumber() != null) {
				totalClaimTravelCount++;
			}
		}
		return totalClaimTravelCount;
	}

	private HttpURLConnection icsApiConnection(HttpURLConnection connection, StringBuilder message, String dateValue, String endDate) {
		try {
		URL urls = new URL(claimStudentTravelApi.concat("?DateProcessedFr=").concat(dateValue).concat("&DateProcessedTo=").concat(endDate));
		connection = (HttpURLConnection) urls.openConnection();
		connection.setRequestMethod("GET");
		connection.setRequestProperty("Accept", "application/json");
		connection.setRequestProperty(claimStudentTravelAccessTokenType, claimStudentTravelAccessToken);
		connection.setReadTimeout(60000);
		connection.setConnectTimeout(60000);
		connection.setDoInput(true);
		} catch(Exception e) {
			message.append("failed connection due to :").append(e.toString());
		}
		return connection;
	}

	private void icsIntegrationFailedMessage(StringBuilder logMessage, Integer responseCode, Integer count, StringBuilder message, StringBuilder failedMessage) {
		if (responseCode != 200) {
			logDetailsInFile(logMessage.append("API call failed with : HTTP error code : ").append(responseCode).append(" at").toString());
			if (responseCode == 400) {// Bad request
				if (count == 0) {
					message.append(failedMessage.append("Bad request found"));
				}
			} else if (responseCode == 401) {// Unauthorized
				if (count == 0) {
					message.append(failedMessage.append("Unauthorized Service"));
				}
			} else if (responseCode == 404) {// Not found
				if (count == 0) {
					message.append(failedMessage.append("Service Not found"));
				}
			} else if (responseCode == 408) {// Request Timeout
				if (count == 0) {
					message.append(failedMessage.append("Request Timeout"));
				}
			} else if (responseCode == 500) {// internal server error
				if (count == 0) {
					message.append(failedMessage.append("Internal server error"));
				}
			} else if (responseCode == 503) {// service unavailable
				if (count == 0) {
					message.append(failedMessage.append("Service unavailable"));
				}
			} else if (responseCode == 403) {// Forbidden Issue
				if (count == 0) {
					message.append(failedMessage.append("Forbidden Issue"));
				}
			}
			throw new RuntimeException(logMessage.append("Failed : HTTP error code : ").append(responseCode).toString());
		}
	}

	private String formatTimestamp(String claimDate) {
	 String formattedDate ="";
		try {
		SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		SimpleDateFormat outputFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		Date date = inputFormat.parse(claimDate);
		formattedDate = outputFormat.format(date);
		} catch (Exception e) {
			logger.error("error in converting date {}", e.getMessage());
		}
		return formattedDate;
	}

	public void logDetailsInFile(String fileContent) {
		FileOutputStream fileOutputStream = null;
		try {
			String fileName = "claim_student_travel_logs";
			String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(), Constants.LOG_DATE_FORMAT);
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
			Date currentDate = new Date(commonDao.getCurrentTimestamp().getTime());
			dateFormat.setTimeZone(TimeZone.getTimeZone(timezone));
			String fileNameWithPath = filePath + File.separator + fileName + "_" + date + ".log";
			File file = new File(fileNameWithPath);
			if (file.exists()) {
				fileOutputStream = new FileOutputStream(fileNameWithPath, true);
			} else {
				fileOutputStream = new FileOutputStream(fileNameWithPath);
			}
			fileContent = fileContent + " - " + dateFormat.format(currentDate) + "\n";
			fileOutputStream.write(fileContent.getBytes());
			fileOutputStream.close();
		} catch (Exception e) {
			logger.error("Exception in method logDetailsInFile : {} ", e.getMessage());
		} finally {
			try {
				if (fileOutputStream != null) {
					fileOutputStream.close();
				}
			} catch (IOException e) {
				logger.error("Error in logDetailsInFile while closing the BufferedWriter : <br/> ");
			}
		}
	}

	public String convertDateFormatBasedOnTimeZone(Long dateValue,String dateFormat) {
		Date date = new Date(dateValue);
		return new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
	}

}
