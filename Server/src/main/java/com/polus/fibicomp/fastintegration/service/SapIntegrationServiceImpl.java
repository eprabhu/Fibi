package com.polus.fibicomp.fastintegration.service;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.tomcat.util.codec.binary.Base64;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.dao.SapIntegrationDao;
import com.polus.fibicomp.fastintegration.pojo.CostCenter;
import com.polus.fibicomp.fastintegration.pojo.CostCenterDetails;
import com.polus.fibicomp.fastintegration.pojo.FundCenter;
import com.polus.fibicomp.fastintegration.pojo.GrantCode;
import com.polus.fibicomp.fastintegration.pojo.GrantCodeDetails;
import com.polus.fibicomp.fastintegration.pojo.ProfitCenter;
import com.polus.fibicomp.fastintegration.pojo.SapCostCenter;
import com.polus.fibicomp.fastintegration.pojo.SapFundCenter;
import com.polus.fibicomp.fastintegration.pojo.SapFundCenterDetails;
import com.polus.fibicomp.fastintegration.pojo.SapGrantCode;
import com.polus.fibicomp.fastintegration.pojo.SapIntegrationLog;
import com.polus.fibicomp.fastintegration.pojo.SapProfitCenter;
import com.polus.fibicomp.fastintegration.pojo.SapProfitCenterDetails;
import com.polus.fibicomp.fastintegration.pojo.TokenResponse;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.sun.xml.ws.client.sei.ResponseBuilder.Body;

@Transactional
@Service(value = "sapIntegrationService")
public class SapIntegrationServiceImpl implements SapIntegrationService {

	protected static Logger logger = LogManager.getLogger(SapIntegrationServiceImpl.class.getName());

	private static final String MESSAGE_TYPE_SUCCESS = "SUCCESS";
	private static final String MESSAGE_TYPE_ERROR = "ERROR";
	private static final String PROFIT_CENTER = "PROFIT_CENTER";
	private static final String COST_CENTER = "COST_CENTER";
	private static final String GRANT_CODE = "GRANT_CODE";
	private static final String FUND_CENTER = "FUND_CENTER";
	private static final String QUICK_START = "quickstart";
	String sapAccessToken = "";
	private Instant tokenExpires;

	@Autowired
	@Qualifier(value = "sapIntegrationDao")
	private SapIntegrationDao sapIntegrationDao;

	@Autowired
	private DateTimeService dateTimeService;

	@Autowired
	private ExcelityService excelityService;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private EmailService emailService;

	@Override
	public List<SapProfitCenter> findProfitCenter(String searchString) {
		return sapIntegrationDao.findProfitCenter(searchString);
	}

	@Override
	public List<SapFundCenter> findFundCenter(String searchString) {
		return sapIntegrationDao.findFundCenter(searchString);
	}

	@Override
	public List<SapCostCenter> findCostCenter(String searchString) {
		return sapIntegrationDao.findCostCenter(searchString);
	}

	@Override
	public List<SapGrantCode> findGrantCode(String searchString) {
		return sapIntegrationDao.findGrantCode(searchString);
	}

	@Override
	public void getAllProfitCenterAPI() {
		int rowCount = 0;
		String errorMessage = "";
		int totalCount = 0;
		String dateValue = "";
		IntegrationReportVO vo = new IntegrationReportVO();
		try {
			ResponseEntity<String> response = getResponseFromAPI(Constants.PROFIT_CENTER_API, errorMessage, vo);
			ObjectMapper objectMapper = new ObjectMapper();
			ProfitCenter profitCenter = objectMapper.readValue(response.getBody(), ProfitCenter.class);
			String successMessage = "";
			if (profitCenter != null && profitCenter.getProfitCenterResponse() != null && profitCenter.getProfitCenterResponse().getProfitCenterMasterResponse() != null
					&& profitCenter.getProfitCenterResponse().getProfitCenterMasterResponse().getSapProfitCenterDetails() != null) {
				totalCount = profitCenter.getProfitCenterResponse().getProfitCenterMasterResponse().getSapProfitCenterDetails().size();
				logger.info("Total Json List count: {}", totalCount);
				Timestamp currentTime = commonDao.getCurrentTimestamp();
				for (SapProfitCenterDetails sapProfitCenterDetails : profitCenter.getProfitCenterResponse().getProfitCenterMasterResponse().getSapProfitCenterDetails()) {
					SapProfitCenter sapProfitCenter = null;
					String profitCenterCode = sapProfitCenterDetails.getProfitCenter();
					if (profitCenterCode != null) {
						sapProfitCenter = sapIntegrationDao.getSapProfitCenter(profitCenterCode);
						if (sapProfitCenter == null) {
							sapProfitCenter = new SapProfitCenter();
							sapProfitCenter.setProfitCenterCode(profitCenterCode);
						}
						if (sapProfitCenterDetails.getValidToDate() != null) {
							sapProfitCenter.setValidTo(dateTimeService.stringToTimestamp(sapProfitCenterDetails.getValidToDate()));
						}
						sapProfitCenter.setControllingArea(sapProfitCenterDetails.getControllingArea());

						if (sapProfitCenterDetails.getValidFromDate() != null) {
							sapProfitCenter.setValidFrom(dateTimeService.stringToTimestamp(sapProfitCenterDetails.getValidFromDate()));
						}
						sapProfitCenter.setEnteredOn(sapProfitCenterDetails.getEnteredOn());
						sapProfitCenter.setEnteredBy(sapProfitCenterDetails.getEnteredBy());
						sapProfitCenter.setResponsibleUser(sapProfitCenterDetails.getFieldOfCoPa());
						sapProfitCenter.setCampus(sapProfitCenterDetails.getDepartment());
						sapProfitCenter.setResponsiblePerson(sapProfitCenterDetails.getPersonResponsible());
						sapProfitCenter.setProfitCenterGroup(sapProfitCenterDetails.getProfitCenterArea());
						sapProfitCenter.setCompanyCode(sapProfitCenterDetails.getCompanyCode());
						sapProfitCenter.setSegment(sapProfitCenterDetails.getSegment());
						sapProfitCenter.setProfitCenterName(sapProfitCenterDetails.getName());
						sapProfitCenter.setDescription(sapProfitCenterDetails.getLongText());
						sapProfitCenter.setProfitCenterDetails(sapProfitCenterDetails.getEnteredBy());
						sapProfitCenter.setSearchTerm(sapProfitCenterDetails.getSearchTerm());
						sapProfitCenter.setIsActive(Boolean.TRUE);
						sapProfitCenter.setDateWhenFeedInactive(null);
						rowCount = rowCount + 1;
						sapProfitCenter.setUpdateTimestamp(currentTime);
						sapProfitCenter.setUpdateUser(QUICK_START);
						sapIntegrationDao.saveOrUpdateSapProfitCenter(sapProfitCenter);
					}
				}
				logger.info("Data added/Updated successfully");
				Integer inactiveProfitCenter = sapIntegrationDao.callToInactiveFeed(currentTime, PROFIT_CENTER);
				logger.info("Inactive feed records: {}", inactiveProfitCenter);
				successMessage = new StringBuilder("Profit Center Entries found for ").append(rowCount).append(" row").toString();
				saveSapIntegrationLog(successMessage, MESSAGE_TYPE_SUCCESS, PROFIT_CENTER);
			} else {
				errorMessage = new StringBuilder("Json data is null").toString();
				saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, PROFIT_CENTER);
			}			
			dateValue = commonService.convertDateFormatBasedOnTimeZone(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE).getTime(), Constants.REQUIRED_DATE_FORMAT);
		} catch (RestClientResponseException ex) {
			logger.error("RestClient exception : {}", ex.getMessage());
			errorMessage = new StringBuilder("RestClient exception :").append(ex.getMessage()).toString();
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, PROFIT_CENTER);
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("RestClient exception: ").append(ex.getMessage());
		} catch (Exception e) {
			logger.error("Exception in getAllProfitCenterAPI : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getAllProfitCenterAPI :").append(e.getMessage()).toString();
			vo.setErrorOccured(Boolean.TRUE);
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, PROFIT_CENTER);
			vo.getEmailContent().getError().append("Exception in saving API contents to RISE: ").append(e);
		}
		if (vo.getErrorOccured()) {
			sendSapIntegrationMailNotification("Profit Center integration API Summary on " + dateValue + " - Failure", vo, PROFIT_CENTER);
		} else {
			setEmailSuccessEmailContent(vo, totalCount, rowCount, "Profit Center");
			sendSapIntegrationMailNotification("Profit Center integration API Summary on " + dateValue + " - Success", vo, PROFIT_CENTER);
		}
	}

	private ResponseEntity<String> getResponseFromAPI(String apiEndPoint, String errorMessage, IntegrationReportVO vo) {
		String accessToken = getAccesToken(vo);
		if (!accessToken.isEmpty() && accessToken != null) {
			logger.info("Access Token : {}", accessToken);
			String profitCenterAPI = excelityService.getSftpConfigurationValueAsString(Constants.SAP_INTEGRATION_API_BASE);
			String apiUrl = profitCenterAPI + apiEndPoint;
			return callExternalAPIs(apiUrl, accessToken);
		} else {
			errorMessage = new StringBuilder("Access Token is null").toString();
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, PROFIT_CENTER);
		}
		return null;
	}

	private String getAccesToken(IntegrationReportVO vo) {
	 if(sapAccessToken.isEmpty() || (tokenExpires != null && tokenExpires.isBefore(Instant.now()))) {
			try {
				logger.info("Requesting for new access token");
				sapAccessToken = getAccessTokenFromAPI(vo);
			} catch (IOException e) {
				logger.error("Exception in getAccesToken: {}", e.getMessage());
				e.printStackTrace();
			}
		}
		return sapAccessToken;
	}

	private void setEmailSuccessEmailContent(IntegrationReportVO vo, int totalCount, int rowCount, String sapIntegrationType) {
		vo.getEmailContent().getSuccess().append(sapIntegrationType).append(" API Summary :").append("<br><br>")
				.append("&emsp;Total count of ").append(sapIntegrationType).append(" records:").append(totalCount)
				.append("<br>").append("&emsp;Success Count: ").append(rowCount).append("<br>")
				.append("&emsp;Failed Count : ").append(totalCount - rowCount);
	}

	private void sendSapIntegrationMailNotification(String subject, IntegrationReportVO vo, String integrationType) {
		logger.info("Requesting for sendSapIntegrationMailNotification for: {}", integrationType);
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setSubject(subject);
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		if (vo.getErrorOccured()) {
			emailServiceVO.setBody(vo.getEmailContent().getError().toString());
		} else {
			emailServiceVO.setBody(vo.getEmailContent().getSuccess().toString());
		}
		String emailAddress = excelityService.getSftpConfigurationValueAsString(Constants.SAP_INTEGRATION_EMAIL_RECIPIENT);
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

	private void saveSapIntegrationLog(String successMessage, String messageTypeSuccess, String interfaceType) {
		SapIntegrationLog integrationLog = new SapIntegrationLog();
		integrationLog.setMessage(successMessage);
		integrationLog.setMessageType(messageTypeSuccess);
		integrationLog.setInterfaceType(interfaceType);
		integrationLog.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		integrationLog.setUpdateUser(QUICK_START);
		sapIntegrationDao.saveSapIntergrationLog(integrationLog);
	}

	private ResponseEntity<String> callExternalAPIs(String apiUrl, String accessToken) {
		RestTemplate restTemplate = new RestTemplate();
		logger.info("Requesting for api url  : {}", apiUrl);
		HttpHeaders headers = new HttpHeaders();
		headers = setSapIntegrationHttpHeaders(headers, accessToken);
		HttpEntity<String> entity = new HttpEntity<>("parameters", headers);
		ResponseEntity<String> response = restTemplate.exchange(apiUrl, HttpMethod.GET, entity, String.class);
		logger.info("Response Status {}", response.getStatusCode());
		return response;
	}

	private HttpHeaders setSapIntegrationHttpHeaders(HttpHeaders headers, String accessToken) {
		headers.set(Constants.HEADER_STRING, Constants.TOKEN_PREFIX + accessToken);
		headers.set("Accept", MediaType.ALL_VALUE);
		headers.set("Content-Type", MediaType.APPLICATION_JSON_VALUE);
		return headers;
	}

	private String getAccessTokenFromAPI(IntegrationReportVO vo) throws IOException {
		RestTemplate restTemplate = new RestTemplate();
		HttpHeaders headers = new HttpHeaders();
		String token = "";
		try {
			String userName = excelityService.getSftpConfigurationValueAsString(Constants.SAP_ACCESS_TOKEN_USERNAME);
			String password = excelityService.getSftpConfigurationValueAsString(Constants.SAP_ACCESS_TOKEN_PASSWORD);
			headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
			headers.add("Authorization", createAuthHeaderString(userName, password));
			String oAuthurl = excelityService.getSftpConfigurationValueAsString(Constants.OAUTH_API);
			MultiValueMap<String, String> map = new LinkedMultiValueMap<>();
			map.add("username", userName);
			map.add("password", password);
			HttpEntity<MultiValueMap<String, String>> entity = new HttpEntity<>(map, headers);
			ResponseEntity<TokenResponse> response = restTemplate.exchange(oAuthurl, HttpMethod.POST, entity, TokenResponse.class);
			long seconds = response.getBody().getExpiresIn().getEpochSecond();
			tokenExpires = Instant.now().plusSeconds(seconds);
			token = response.getBody().getAccessToken();
		} catch (Exception e) {
			logger.error("Exception in getAccessTokenFromAPI : {}", e.getMessage());
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("Exception in get Access token from API: ").append(e.getMessage()).append(" ");
		}
		return token;
	}

	private String createAuthHeaderString(String username, String password) {
		String auth = username + ":" + password;
		byte[] encodedAuth = Base64.encodeBase64(auth.getBytes(StandardCharsets.US_ASCII));
		String authHeader = "Basic " + new String(encodedAuth);
		return authHeader;
	}

	@Override
	public void getAllFundCenterAPI() {
		int rowCount = 0;
		String errorMessage = "";
		int totalCount = 0;
		String dateValue = "";
		IntegrationReportVO vo = new IntegrationReportVO();
		try {
			ResponseEntity<String> response = getResponseFromAPI(Constants.FUND_CENTER_API, errorMessage, vo);
			ObjectMapper objectMapper = new ObjectMapper();
			FundCenter fundCenter = objectMapper.readValue(response.getBody(), FundCenter.class);
			String successMessage = "";
			if (fundCenter != null && fundCenter.getFundCenterResponse() != null && fundCenter.getFundCenterResponse().getFundCenterMasterResponse() != null 
					&& fundCenter.getFundCenterResponse().getFundCenterMasterResponse().getSapFundCenterDetails() != null) {
				totalCount = fundCenter.getFundCenterResponse().getFundCenterMasterResponse().getSapFundCenterDetails().size();
				logger.info("Total Json List count: {}", totalCount);
				Timestamp currentTime = new Timestamp(System.currentTimeMillis());
				logger.info("Current Time: {}", currentTime);
				for (SapFundCenterDetails sapFundCenterDetails : fundCenter.getFundCenterResponse().getFundCenterMasterResponse().getSapFundCenterDetails()) {
					SapFundCenter sapFundCenter = null;
					String sapFundCenterCode = sapFundCenterDetails.getFundCentre();
					if (sapFundCenterCode != null) {
						sapFundCenter = sapIntegrationDao.getSapFundCenter(sapFundCenterCode);
						if (sapFundCenter == null) {
							sapFundCenter = new SapFundCenter();
							sapFundCenter.setFundCenterCode(sapFundCenterCode);
						}
						sapFundCenter.setFundCenterCode(sapFundCenterDetails.getFundCentre());
						sapFundCenter.setDescription(sapFundCenterDetails.getFundCentreLongDescription());
						sapFundCenter.setCompanyCode(sapFundCenterDetails.getCompanyCode());
						sapFundCenter.setFmArea(sapFundCenterDetails.getFunctionalArea());
						if (sapFundCenterDetails.getValidFrom() != null) {
							sapFundCenter.setValidFrom(dateTimeService.stringToTimestamp(sapFundCenterDetails.getValidFrom()));
						}
						if (sapFundCenterDetails.getValidTo() != null) {
							sapFundCenter.setValidTo(dateTimeService.stringToTimestamp(sapFundCenterDetails.getValidTo()));
						}
						sapFundCenter.setIsActive(Boolean.TRUE);
						sapFundCenter.setDateWhenFeedInactive(null);
						rowCount = rowCount + 1;
						sapFundCenter.setUpdateTimestamp(currentTime);
						sapFundCenter.setUpdateUser(QUICK_START);
						sapIntegrationDao.saveOrUpdateSapFundCenter(sapFundCenter);
					}
				}
				logger.info("Data added/Updated successfully");
				Integer inactiveFundCenter = sapIntegrationDao.callToInactiveFeed(currentTime, FUND_CENTER);
				logger.info("Inactive feed records: {}", inactiveFundCenter);
				successMessage = new StringBuilder("Fund Center Entries found for ").append(rowCount).append(" row").toString();
				saveSapIntegrationLog(successMessage, MESSAGE_TYPE_SUCCESS, FUND_CENTER);
			} else {
				errorMessage = new StringBuilder("Json data is null").toString();
				saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, FUND_CENTER);
			}			
			dateValue = commonService.convertDateFormatBasedOnTimeZone(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE).getTime(), Constants.REQUIRED_DATE_FORMAT);
		} catch (RestClientResponseException ex) {
			logger.error("RestClient exception : {}", ex.getMessage());
			errorMessage = new StringBuilder("RestClient exception :").append(ex.getMessage()).toString();
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, FUND_CENTER);
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("RestClient exception: ").append(ex.getMessage());
		} catch (Exception e) {
			logger.error("Exception in getAllFundCenterAPI : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getAllFundCenterAPI :").append(e.getMessage()).toString();
			vo.setErrorOccured(Boolean.TRUE);
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, FUND_CENTER);
			vo.getEmailContent().getError().append("Exception in saving API contents to RISE: ").append(e);
		}
		if (vo.getErrorOccured()) {
			sendSapIntegrationMailNotification("Fund Center Sap Integration API Summary on " + dateValue + " - Failure", vo, FUND_CENTER);
		} else {
			setEmailSuccessEmailContent(vo, totalCount, rowCount, "Fund Center");
			sendSapIntegrationMailNotification("Fund Center Sap Integration API Summary on " + dateValue + " - Success", vo, FUND_CENTER);
		}
	}

	@Override
	public void getAllCostCenterAPI() {
		int rowCount = 0;
		String errorMessage = "";
		int totalCount = 0;
		String dateValue = "";
		IntegrationReportVO vo = new IntegrationReportVO();
		try {
			ResponseEntity<String> response = getResponseFromAPI(Constants.COST_CENTER_API, errorMessage, vo);
			ObjectMapper objectMapper = new ObjectMapper();
			CostCenter costCenter = objectMapper.readValue(response.getBody(), CostCenter.class);
			String successMessage = "";
			if (costCenter != null && costCenter.getCostCenterResponse() != null && costCenter.getCostCenterResponse().getCostCenterMasterResponse() != null 
					&& costCenter.getCostCenterResponse().getCostCenterMasterResponse().getCostCenterDetails() != null) {
				totalCount = costCenter.getCostCenterResponse().getCostCenterMasterResponse().getCostCenterDetails().size();
				logger.info("Total Json List count: {}", totalCount);
				Timestamp currentTime = commonDao.getCurrentTimestamp();
				for (CostCenterDetails costCenterDetails : costCenter.getCostCenterResponse().getCostCenterMasterResponse().getCostCenterDetails()) {
					SapCostCenter sapCostCenter = null;
					String costCenterCode = costCenterDetails.getCostCenter();
					if (costCenterCode != null) {
						sapCostCenter = sapIntegrationDao.getSapCostCenter(costCenterCode);
						if (sapCostCenter == null) {
							sapCostCenter = new SapCostCenter();
							sapCostCenter.setCostCenterCode(costCenterCode);
						}
						if (costCenterDetails.getValidFromDate() != null) {
							sapCostCenter.setValidFrom(dateTimeService.stringToTimestamp(costCenterDetails.getValidFromDate()));
						}
						if (costCenterDetails.getValidToDate() != null) {
							sapCostCenter.setValidTo(dateTimeService.stringToTimestamp(costCenterDetails.getValidToDate()));
						}
						sapCostCenter.setControllingArea(costCenterDetails.getControllingArea());
						sapCostCenter.setDescription(costCenterDetails.getDescription());
						sapCostCenter.setCostCenterName(costCenterDetails.getName());
						sapCostCenter.setResponsiblePerson(costCenterDetails.getPersonInCharge());
						sapCostCenter.setHierarchyArea(costCenterDetails.getBusinessArea());
						sapCostCenter.setCompanyCode(costCenterDetails.getCompanyCode());
						sapCostCenter.setCurrency(costCenterDetails.getCurrency());
						sapCostCenter.setCampus(costCenterDetails.getDepartment());
						sapCostCenter.setCostCenterCategory(costCenterDetails.getCostctrHierGrp());
						sapCostCenter.setCostCenterType(costCenterDetails.getCostcenterType());
						sapCostCenter.setProfitCenter(costCenterDetails.getProfitCenter());
						sapCostCenter.setIsActive(Boolean.TRUE);
						sapCostCenter.setDateWhenFeedInactive(null);
						rowCount = rowCount + 1;
						sapCostCenter.setUpdateTimestamp(currentTime);
						sapCostCenter.setUpdateUser(QUICK_START);
						sapIntegrationDao.saveOrUpdateSapCostCenter(sapCostCenter);
					}
				}
				logger.info("Data added/Updated successfully");
				Integer inactiveCostCenter = sapIntegrationDao.callToInactiveFeed(currentTime, COST_CENTER);
				logger.info("Inactive feed records: {}", inactiveCostCenter);
				successMessage = new StringBuilder("Cost Center Entries found for ").append(rowCount).append(" row").toString();
				saveSapIntegrationLog(successMessage, MESSAGE_TYPE_SUCCESS, COST_CENTER);
			} else {
				errorMessage = new StringBuilder("Json data is null").toString();
				saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, COST_CENTER);
				}			
			dateValue = commonService.convertDateFormatBasedOnTimeZone(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE).getTime(), Constants.REQUIRED_DATE_FORMAT);
		} catch (RestClientResponseException ex) {
			logger.error("RestClient exception : {}", ex.getMessage());
			errorMessage = new StringBuilder("RestClient exception :").append(ex.getMessage()).toString();
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, COST_CENTER);
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("RestClient exception: ").append(ex.getMessage());
		} catch (Exception e) {
			logger.error("Exception in getAllCostCenterAPI : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getAllCostCenterAPI :").append(e.getMessage()).toString();
			vo.setErrorOccured(Boolean.TRUE);
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, COST_CENTER);
			vo.getEmailContent().getError().append("Exception in saving API contents to RISE: ").append(e);
		}
		if (vo.getErrorOccured()) {
			sendSapIntegrationMailNotification("Cost Center integration API Summary on " + dateValue + " - Failure", vo, COST_CENTER);
		} else {
			setEmailSuccessEmailContent(vo, totalCount, rowCount, "Cost Center");
			sendSapIntegrationMailNotification("Cost Center integration API Summary on " + dateValue + " - Success", vo, COST_CENTER);
		}
	}

	@Override
	public void getAllGrantCodeFromAPI() {
		int rowCount = 0;
		String errorMessage = "";
		int totalCount = 0;
		String dateValue = "";
		IntegrationReportVO vo = new IntegrationReportVO();
		try {
			ResponseEntity<String> response = getResponseFromAPI(Constants.GRANT_CODE_API, errorMessage, vo);
			ObjectMapper objectMapper = new ObjectMapper();
			GrantCode grantCode = objectMapper.readValue(response.getBody(), GrantCode.class);
			String successMessage = "";
			if (grantCode != null && grantCode.getGrantCodeResponse() != null && grantCode.getGrantCodeResponse().getGrantCodeMaster() != null
					&& grantCode.getGrantCodeResponse().getGrantCodeMaster().getGrantCodeDetails() != null) {
				totalCount = grantCode.getGrantCodeResponse().getGrantCodeMaster().getGrantCodeDetails().size();
				logger.info("Total Json List count: {}", totalCount);
				Timestamp currentTime = commonDao.getCurrentTimestamp();
				for (GrantCodeDetails grantCodeDetails : grantCode.getGrantCodeResponse().getGrantCodeMaster().getGrantCodeDetails()) {
					SapGrantCode sapGrantCode = null;
					String grantNumber = grantCodeDetails.getGrantNumber();
					if (grantNumber != null) {
						sapGrantCode = sapIntegrationDao.getGrantCode(grantNumber);
						if (sapGrantCode == null) {
							sapGrantCode = new SapGrantCode();
							sapGrantCode.setGrantCode(grantNumber);
						}
						sapGrantCode.setCompanyCode(grantCodeDetails.getCompanyCode());
						sapGrantCode.setGrantType(grantCodeDetails.getGrantType());
						sapGrantCode.setGrantCodeName(grantCodeDetails.getShortDesc());
						sapGrantCode.setGmSponsor(grantCodeDetails.getGmSponsor());
						sapGrantCode.setRemarks(grantCodeDetails.getDescription());
						if (grantCodeDetails.getValidFrom() != null) {
							sapGrantCode.setValidFrom(dateTimeService.stringToTimestamp(grantCodeDetails.getValidFrom()));
						}
						if (grantCodeDetails.getValidTo() != null) {
							sapGrantCode.setValidTo(dateTimeService.stringToTimestamp(grantCodeDetails.getValidTo()));
						}
						sapGrantCode.setIsActive(Boolean.TRUE);
						sapGrantCode.setDateWhenFeedInactive(null);
						rowCount = rowCount + 1;
						sapGrantCode.setUpdateTimestamp(currentTime);
						sapGrantCode.setUpdateUser(QUICK_START);
						sapIntegrationDao.saveOrUpdateGrantCode(sapGrantCode);
					}
				}
				logger.info("Data added/Updated successfully");
				Integer inactiveCostCenter = sapIntegrationDao.callToInactiveFeed(currentTime, GRANT_CODE);
				logger.info("Inactive feed records: {}", inactiveCostCenter);
				successMessage = new StringBuilder("Grade Code Entries found for ").append(rowCount).append(" row").toString();
				saveSapIntegrationLog(successMessage, MESSAGE_TYPE_SUCCESS, GRANT_CODE);
			} else {
				errorMessage = new StringBuilder("Json data is null").toString();
				saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, GRANT_CODE);
				}			
			dateValue = commonService.convertDateFormatBasedOnTimeZone(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE).getTime(), Constants.REQUIRED_DATE_FORMAT);
		} catch (RestClientResponseException ex) {
			logger.error("RestClient exception in getAllGrantCodeFromAPI : {}", ex.getMessage());
			errorMessage = new StringBuilder("RestClient exception :").append(ex.getMessage()).toString();
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, GRANT_CODE);
			vo.setErrorOccured(Boolean.TRUE);
			vo.getEmailContent().getError().append("RestClient exception: ").append(ex.getMessage());
		} catch (Exception e) {
			logger.error("Exception in getAllGrantCodeFromAPI : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getAllGrantCodeFromAPI :").append(e.getMessage()).toString();
			vo.setErrorOccured(Boolean.TRUE);
			saveSapIntegrationLog(errorMessage, MESSAGE_TYPE_ERROR, GRANT_CODE);
			vo.getEmailContent().getError().append("Exception in saving API contents to RISE: ").append(e);
		}
		if (vo.getErrorOccured()) {
			sendSapIntegrationMailNotification("Grant Code integration API Summary on " + dateValue + " - Failure", vo, GRANT_CODE);
		} else {
			setEmailSuccessEmailContent(vo, totalCount, rowCount, "Grant Code");
			sendSapIntegrationMailNotification("Grant Code integration API Summary on " + dateValue + " - Success", vo, GRANT_CODE);
		}
	}

	@Scheduled(cron = "${sap.rise.integration.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void callSapIntegrationScheduler() {
		try {
			logger.info("Sap Integration Scheduler Starts at: {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			getAllProfitCenterAPI();
			getAllFundCenterAPI();
			getAllCostCenterAPI();
			getAllGrantCodeFromAPI();
			logger.info("Sap Integration Scheduler Finished at: {}", commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
		} catch (Exception e) {
			logger.error("Exception in callSapIntegrationScheduler: {}", e.getMessage());
		}
	}

}
