package com.polus.fibicomp.manpowerintegration.service;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.spec.KeySpec;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.Period;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import javax.xml.bind.JAXBElement;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Source;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.oxm.XmlMappingException;
import org.springframework.oxm.jaxb.Jaxb2Marshaller;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;
import org.springframework.ws.client.core.WebServiceTemplate;
import org.springframework.ws.client.support.interceptor.ClientInterceptor;
import org.springframework.ws.soap.SoapFaultDetail;
import org.springframework.ws.soap.SoapFaultDetailElement;
import org.springframework.ws.soap.client.SoapFaultClientException;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.dao.CustomDataElementDao;
import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerTemp;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.manpower.vo.ManpowerVO;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.manpowerintegration.dto.ManpowerLookUpSyncDto;
import com.polus.fibicomp.manpowerintegration.dto.WorkdayInterfaceLogDto;
import com.polus.fibicomp.manpowerintegration.dto.citizenship.CitizenReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.citizenship.CitizenshipNationality;
import com.polus.fibicomp.manpowerintegration.dto.costingallocationreconciliation.CostAllocationDetailsGroup;
import com.polus.fibicomp.manpowerintegration.dto.costingallocationreconciliation.CostAllocationReconciliation;
import com.polus.fibicomp.manpowerintegration.dto.costingallocationreconciliation.CostReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner.HRBPClusterLead;
import com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner.HRBPReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner.HRBusinessPartnerResearch;
import com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner.WorkdayHRBusinessPartner;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.ExternalIntegrationIDDataType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.IDType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.LocationObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.LocationObjectType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.ObjectFactory;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationAddUpdateType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationDataType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationObjectType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationReferenceRootType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationSubtypeReferenceDataType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationTypeReferenceDataType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.OrganizationVisibilityReferenceDataType;
import com.polus.fibicomp.manpowerintegration.dto.humanresources.ValidationErrorType;
import com.polus.fibicomp.manpowerintegration.dto.jobprofile.JobProfileReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.jobprofile.WorkdayJobProfile;
import com.polus.fibicomp.manpowerintegration.dto.jobprofilechanges.JobProfileChange;
import com.polus.fibicomp.manpowerintegration.dto.jobprofilechanges.JobProfileChangeReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.longleave.LongLeaveReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.longleave.WorkdayLongLeave;
import com.polus.fibicomp.manpowerintegration.dto.manpowerdetails.ManpowerDetails;
import com.polus.fibicomp.manpowerintegration.dto.manpowerdetails.ManpowerReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.payroll.AssignCostingAllocationRequestType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.AssignCostingAllocationResponseType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.CostingAllocationDataType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.CostingAllocationDetailReplacementDataType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.CostingAllocationIntervalDataType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.CostingIntervalUpdateKeyType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.TenantedPayrollWorktagObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.payroll.TenantedPayrollWorktagObjectType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.CreatePositionDataType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.CreatePositionRequestType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.CreatePositionResponseType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.JobProfileObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.JobProfileObjectType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.PositionDefinitionDataType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.PositionGroupRestrictionDataType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.SupervisoryOrganizationObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.recruiting.SupervisoryOrganizationObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.AssignRolesEventDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.AssignRolesRequestType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.AssignRolesResponseType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.AssignRolesRoleAssignmentDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.AssignableRoleObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.AssignableRoleObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.BusinessProcessCommentDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.BusinessProcessParametersType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.ClosePositionDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.ClosePositionEventDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.ClosePositionRequestType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.ClosePositionResponseType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.EventClassificationSubcategoryObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.EventClassificationSubcategoryObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.FreezePositionRequestType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.FreezePositionResponseType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.MoveWorkersByOrganizationDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.MoveWorkersByOrganizationPositionDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.MoveWorkersByOrganizationRequestType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.MoveWorkersByOrganizationResponseType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.PositionGroupFreezeDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.PositionGroupFreezeEventDataType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.PositionGroupObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.PositionRestrictionsObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.PositionRestrictionsObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.RoleAssignerObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.RoleAssignerObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.RoleeSelectorObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.RoleeSelectorObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.StaffObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.StaffObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.StaffingInterfaceObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.StaffingInterfaceObjectType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationFaultType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.WorkerObjectIDType;
import com.polus.fibicomp.manpowerintegration.dto.staffing.WorkerObjectType;
import com.polus.fibicomp.manpowerintegration.dto.supervisoryorganization.SupervisoryOrganization;
import com.polus.fibicomp.manpowerintegration.dto.terminations.TerminationsReportEntry;
import com.polus.fibicomp.manpowerintegration.dto.terminations.WorkdayTerminations;
import com.polus.fibicomp.manpowerintegration.pojo.AwardManpowerBaseSalaryHistory;
import com.polus.fibicomp.manpowerintegration.pojo.AwardSupOrgMapping;
import com.polus.fibicomp.manpowerintegration.pojo.ManpowerLog;
import com.polus.fibicomp.manpowerintegration.pojo.RiseErrorAllocations;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayConfigurationData;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayJobProfileChange;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayLongLeaveDetails;
import com.polus.fibicomp.manpowerintegration.pojo.WorkdayTerminationDetails;
import com.polus.fibicomp.manpowerintegration.util.CustomClientInterceptor;
import com.polus.fibicomp.manpowerintegration.vo.AwardPersonVo;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "manpowerIntegrationService")
public class ManpowerIntegrationServiceImpl implements ManpowerIntegrationService {

	protected static Logger logger = LogManager.getLogger(ManpowerIntegrationServiceImpl.class.getName());

	private Jaxb2Marshaller marshaller;

	private WebServiceTemplate template;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private EmailService emailService;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private CustomDataElementDao customDataElementDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private ExcelityService excelityService;

	@Autowired
	private ManpowerService manpowerService;

	@Autowired
	private DateTimeService dateTimeService;

	@Autowired
	private ManpowerDao manpowerDao;

	@Value("${manpower.excelity.saltValue}")
	private String saltValue;

	@Value("${appserver.ip}")
	private String appServerIp;

	private static final String UPDATE_USER = "quickstart";

	private static final String MESSAGE_TYPE_SUCCESS = "SUCCESS";

	private static final String MESSAGE_TYPE_ERROR = "ERROR";

	private static final String WORKDAY_API_VERSION = "v35.1";

	private static final String FORMAT_JSON = "format=json";

	private static final String MESSAGE_TYPE_FIBI_ERROR = "FIBI_ERROR";

	private static final String MESSAGE_TYPE_INTERFACE_PENDING = "RESP_PENDING";

	private static final String MESSAGE_TYPE_TECHNICAL_ERROR = "TECHNICAL_ERROR";

	@Autowired
	private DashboardService dashboardService;

	@Value("${system.timezone}")
	private String timezone;

	@SuppressWarnings({ "unchecked" })
	@Override
	public String createWorkdayPosition(AwardManpowerResource awardManpowerResource, String pISuprgId, Map<String, String> workdayConfigDetails, Integer awardId, Integer workdayManpowerInterfaceId) throws XmlMappingException, IOException {
		String positionId = "";
		StringBuilder requestMessage = new StringBuilder("");
		String errorMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] {new CustomClientInterceptor(workdayConfigDetails)};
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.recruiting");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			com.polus.fibicomp.manpowerintegration.dto.recruiting.ObjectFactory objectFactory = new com.polus.fibicomp.manpowerintegration.dto.recruiting.ObjectFactory();
			CreatePositionRequestType request = new CreatePositionRequestType();
			request = prepareCreatePositionObject(request, awardManpowerResource, pISuprgId, requestMessage);
			JAXBElement<CreatePositionRequestType> jaxBRequest = objectFactory.createCreatePositionRequest(request);
			CreatePositionResponseType response = new CreatePositionResponseType();
			JAXBElement<CreatePositionResponseType> jaxBResponse = objectFactory.createCreatePositionResponse(response);
			logger.info("Requesting for createWorkdayPosition for {}", requestMessage);
			String workdayApiUrl =  new StringBuilder(workdayConfigDetails.get(Constants.WORKDAY_API)).append("Create_Positions/").toString();
			logger.info("Requesting for {} ", workdayApiUrl);
			jaxBResponse = (JAXBElement<CreatePositionResponseType>) template.marshalSendAndReceive(workdayApiUrl, jaxBRequest);
			if (jaxBResponse != null) {
				response = jaxBResponse.getValue();
				if (response != null) {
					if (response.getPositionReference() != null) {
						com.polus.fibicomp.manpowerintegration.dto.recruiting.PositionGroupObjectType positionReference = response.getPositionReference();
						if (positionReference.getID() != null && !positionReference.getID().isEmpty()) {
							for (com.polus.fibicomp.manpowerintegration.dto.recruiting.PositionGroupObjectIDType positionReferenceId : positionReference.getID()) {
								if (positionReferenceId.getType().equals("Position_ID") && positionReferenceId.getValue() != null) {
									positionId = positionReferenceId.getValue();
									logger.info("PositionId :{} ", positionId);
									saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_SUCCESS, "Position_ID = " + positionId, 200, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
								}
							}
						}
					}
				} else {
					logger.info("Empty response for createWorkdayPosition for {}", requestMessage);
					saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_ERROR, "Empty response for createWorkdayPosition ", 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
				}
			} else {
				logger.info("Empty body response for createWorkdayPosition for {}", requestMessage);
				saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_ERROR, "Empty body response for createWorkdayPosition ", 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
			}
		} catch(SoapFaultClientException ex) {
			logger.error("SoapFaultClientError in createWorkdayPosition {} ", ex.getMessage());
			SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
			if (soapFaultDetail != null) {
				SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
			    Source detailSource = detailElementChild.getSource();
			    Object detail = template.getUnmarshaller().unmarshal(detailSource);
			    JAXBElement<com.polus.fibicomp.manpowerintegration.dto.recruiting.ValidationFaultType> source = (JAXBElement<com.polus.fibicomp.manpowerintegration.dto.recruiting.ValidationFaultType>)detail;
			    if (source.getValue() != null && (source.getValue().getValidationError() != null && !source.getValue().getValidationError().isEmpty())) {
			    	for (com.polus.fibicomp.manpowerintegration.dto.recruiting.ValidationErrorType error : source.getValue().getValidationError()) {
						saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_ERROR, error.getMessage(), 500, requestMessage.toString(), error.getXpath(), error.getDetailMessage(), awardId, workdayManpowerInterfaceId);
			    	}
			    }
		    } else {
		    	errorMessage = new StringBuilder().append("SoapFaultClientError in createWorkdayPosition : ").append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
				saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
		    }
		} catch(Exception e) {
			logger.error("Error in createWorkdayPosition {} ", e.getMessage());
	    	errorMessage = new StringBuilder().append("Error in createWorkdayPosition : ").append(e.getMessage()).toString();
			saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
		}
		return positionId;
	}

	@SuppressWarnings({ "unchecked" })
	private String createSupervisoryOrganization(String superiorSupOrgId, String pIPersonId, String awardNumber, Map<String, String> workdayConfigDetails, Integer awardId, WorkdayManpowerInterface workdayManpowerInterface) throws XmlMappingException, IOException {
		String iIdValue = "";
		String requestMessage = new StringBuilder("SuperiorSupOrgId=").append(superiorSupOrgId).append("PIPersonId=").append(pIPersonId).toString();
		String errorMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] { new CustomClientInterceptor(workdayConfigDetails)};
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.humanresources");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			ObjectFactory factory = new ObjectFactory();
			OrganizationAddUpdateType request = new OrganizationAddUpdateType();
			request = prepareAddOrUpdateOrganizationObject(request, superiorSupOrgId, pIPersonId);
			JAXBElement<OrganizationAddUpdateType> jaxBRequest = factory.createOrganizationAddUpdate(request);
			OrganizationReferenceRootType response = new OrganizationReferenceRootType();
			JAXBElement<OrganizationReferenceRootType> jaxbResponse = factory.createOrganizationReference(response);
			logger.info("Requesting for createSupervisoryOrganization for {} ", requestMessage);
			String workdayApiUrl =  new StringBuilder(workdayConfigDetails.get(Constants.WORKDAY_API)).append("SupervisoryOrganization/").toString();
			logger.info("Requesting for {} ", workdayApiUrl);
			jaxbResponse = (JAXBElement<OrganizationReferenceRootType>) template.marshalSendAndReceive(workdayApiUrl, jaxBRequest);
			if (jaxbResponse != null) {
				response = jaxbResponse.getValue();
				if (response != null) {
					if (response.getIntegrationIDReference().getID() != null) {
						if (response.getIntegrationIDReference().getID().getValue() != null) {
							iIdValue = response.getIntegrationIDReference().getID().getValue();
							logger.info("IID Value : {}", response.getIntegrationIDReference().getID().getValue());
							saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_SUCCESS, "IID Value= " + iIdValue, 200, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
						}
					}
				} else {
					workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																															 
					logger.info("Empty response for createSupervisoryOrganization for {}", requestMessage);
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_ERROR, "Empty response for createSupervisoryOrganization ", 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
				}
			} else {
				workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																
				logger.info("Empty response for createSupervisoryOrganization for {} ", requestMessage);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_ERROR, "Empty body response for createSupervisoryOrganization ", 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (SoapFaultClientException ex) {
			logger.error("SoapFaultClientError in createSupervisoryOrganization {}", ex.getMessage());
			SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
			if (soapFaultDetail != null) {
				SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
				Source detailSource = detailElementChild.getSource();
				Object detail = template.getUnmarshaller().unmarshal(detailSource);
				JAXBElement<com.polus.fibicomp.manpowerintegration.dto.humanresources.ValidationFaultType> source = (JAXBElement<com.polus.fibicomp.manpowerintegration.dto.humanresources.ValidationFaultType>) detail;
				if (source.getValue() != null && (source.getValue().getValidationError() != null && !source.getValue().getValidationError().isEmpty())) {
					workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																														 
					for (ValidationErrorType error : source.getValue().getValidationError()) {
						saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_ERROR, error.getMessage(), 500, requestMessage, error.getXpath(), error.getDetailMessage(), awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
					}
				}
			} else {
				workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																
		    	errorMessage = new StringBuilder().append("SoapFaultClientError in createSupervisoryOrganization : ").append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (Exception e) {
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																 
			logger.error("Error in createSupervisoryOrganization {}", e.getMessage());
	    	errorMessage = new StringBuilder().append("Error in createSupervisoryOrganization : ").append(e.getMessage()).toString();
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
		return iIdValue;
	}

	private OrganizationAddUpdateType prepareAddOrUpdateOrganizationObject(OrganizationAddUpdateType request, String superiorSupOrgId, String pIPersonId) throws DatatypeConfigurationException, ParseException {
		request.setVersion(WORKDAY_API_VERSION);
		Timestamp currentTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		XMLGregorianCalendar currentDateXmlCalendar  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(currentTime));
		currentDateXmlCalendar.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		OrganizationDataType organizationData = new OrganizationDataType();
		organizationData.setOrganizationReferenceID("");
		organizationData.setEffectiveDate(currentDateXmlCalendar);
		ExternalIntegrationIDDataType integrationId = new ExternalIntegrationIDDataType();
		IDType idType = new IDType();
		idType.setSystemID("RISE");
		String uniqueId = pIPersonId + superiorSupOrgId + currentTime.toString();
		idType.setValue(uniqueId);
		integrationId.getID().add(idType);
		organizationData.setIntegrationIDData(integrationId);
		organizationData.setOrganizationName("Research Projects");
		organizationData.setAvailabilityDate(currentDateXmlCalendar);
		organizationData.setIncludeLeaderInName(true);
		organizationData.setPositionManagementEnabled(true);
		OrganizationObjectType organization = new OrganizationObjectType();
		OrganizationObjectIDType organizationId = new OrganizationObjectIDType();
		organizationId.setValue(superiorSupOrgId);
		organizationId.setType("Organization_Reference_ID");
		organization.getID().add(organizationId);
		organizationData.setSuperiorOrganizationReference(organization);
		OrganizationTypeReferenceDataType organizationTypeReference = new OrganizationTypeReferenceDataType();
		organizationTypeReference.setOrganizationTypeName("Supervisory");
		organizationData.setOrganizationTypeReference(organizationTypeReference);
		OrganizationSubtypeReferenceDataType organizationSubTypeReference = new OrganizationSubtypeReferenceDataType();
		organizationSubTypeReference.setOrganizationSubtypeName("Section");
		organizationData.setOrganizationSubtypeReference(organizationSubTypeReference);
		OrganizationVisibilityReferenceDataType organizationVisibility = new OrganizationVisibilityReferenceDataType();
		organizationVisibility.setOrganizationVisibilityName("Everyone");
		LocationObjectType location = new LocationObjectType();
		LocationObjectIDType locationId = new LocationObjectIDType();
		locationId.setValue("ADM");
		locationId.setType("Location_ID");
		location.getID().add(locationId);
		organizationData.setPrimaryBusinessSiteReference(location);
		request.setOrganizationData(organizationData);
		return request;
	}

	public GregorianCalendar getXMLGregorianCalendarDateFromTimestamp(Timestamp tS) {
		LocalDate dt = tS.toLocalDateTime().toLocalDate();
		return new GregorianCalendar(dt.getYear(), dt.getMonthValue() - 1, dt.getDayOfMonth());
	}

	@SuppressWarnings("unchecked")
	private Boolean assignManagerRoleToPI(String piPersonId, String iIdValue, String awardNumber, Map<String, String> configDetails, Integer awardId, WorkdayManpowerInterface workdayManpowerInterface) throws XmlMappingException, IOException {
		Boolean assignSuccess = false;
		String requestMessage = new StringBuilder("IIDValue=").append(iIdValue).append("PIPersonId=").append(piPersonId).toString();
		String errorMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] { new CustomClientInterceptor(configDetails) };
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.staffing");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			AssignRolesRequestType request = new AssignRolesRequestType();
			request = prepareAssignRolesRequestObject(request, piPersonId, iIdValue);
			com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory factory = new com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory();
			JAXBElement<AssignRolesRequestType> jaxbRequest = factory.createAssignRolesRequest(request);
			AssignRolesResponseType response = new AssignRolesResponseType();
			JAXBElement<AssignRolesResponseType> jaxbResponse = factory.createAssignRolesResponse(response);
			logger.info("Requesting for assignManagerRoleToPI for {}", requestMessage);
			String workdayApiUrl =  new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("AssignRoles/").toString();
			logger.info("Requesting for {} ", workdayApiUrl);
			jaxbResponse = (JAXBElement<AssignRolesResponseType>) template.marshalSendAndReceive(workdayApiUrl, jaxbRequest);
			if (jaxbResponse != null) {
				response = jaxbResponse.getValue();
				if (response != null) {
					if (response.getAssignRolesEventReference() != null) {
						assignSuccess = true;
						saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_ASSIGN_ROLES, MESSAGE_TYPE_SUCCESS, "AssignRoles Success", 200, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
					}
				} else {
					workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																															 
					logger.info("Empty response for assignManagerRoleToPI for {}", requestMessage);
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_ASSIGN_ROLES, MESSAGE_TYPE_ERROR, "Empty response for assignManagerRoleToPI ", 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
				}
			} else {
				workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																
				logger.info("Empty body response for assignManagerRoleToPI for {}", requestMessage);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_ASSIGN_ROLES, MESSAGE_TYPE_ERROR, "Empty body response for assignManagerRoleToPI ", 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		}
		catch (SoapFaultClientException ex) {
			logger.error("SoapFaultClientError in assignManagerRoleToPI {}", ex.getMessage());
			SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
			if (soapFaultDetail != null) {
				SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
				Source detailSource = detailElementChild.getSource();
				Object detail = template.getUnmarshaller().unmarshal(detailSource);
				JAXBElement<com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationFaultType> source = (JAXBElement<com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationFaultType>) detail;
				if (source.getValue() != null && (source.getValue().getValidationError() != null && !source.getValue().getValidationError().isEmpty())) {
					workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																															 
					for (com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationErrorType error : source.getValue().getValidationError()) {
						saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_ASSIGN_ROLES, MESSAGE_TYPE_ERROR, error.getMessage(), 500, requestMessage, error.getXpath(), error.getDetailMessage(), awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
					}
				}
			} else {
				workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																												  
		    	errorMessage = new StringBuilder().append("SoapFaultClientError in assignManagerRoleToPI : ").append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_ASSIGN_ROLES, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		}
		catch (Exception e) {
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																															 
			logger.error("Error in assignManagerRoleToPI {}", e.getMessage());
	    	errorMessage = new StringBuilder().append("Error in assignManagerRoleToPI : ").append(e.getMessage()).toString();
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_ASSIGN_ROLES, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
		return assignSuccess;
	}

	private AssignRolesRequestType prepareAssignRolesRequestObject(AssignRolesRequestType request, String piPersonId, String iIdValue) throws DatatypeConfigurationException, ParseException {
		BusinessProcessParametersType parameter = new BusinessProcessParametersType();
		parameter.setAutoComplete(true);
		parameter.setRunNow(true);
		BusinessProcessCommentDataType commentData = new BusinessProcessCommentDataType();
		commentData.setComment("RISE Testing");
		parameter.setCommentData(commentData);
		request.setBusinessProcessParameters(parameter);
		request.setVersion(WORKDAY_API_VERSION);
		XMLGregorianCalendar currentDateXmlCalendar  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)));
		currentDateXmlCalendar.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		AssignRolesEventDataType rolesEventData = new AssignRolesEventDataType();
		rolesEventData.setEffectiveDate(currentDateXmlCalendar);
		RoleeSelectorObjectType roleeSelectorReference = new RoleeSelectorObjectType();
		roleeSelectorReference.setDescriptor("");
		RoleeSelectorObjectIDType roleeSelectorId = new RoleeSelectorObjectIDType();
		roleeSelectorId.setType("Employee_ID");
		roleeSelectorId.setValue(piPersonId);
		roleeSelectorReference.getID().add(roleeSelectorId);
		rolesEventData.setEventTargetAssigneeReference(roleeSelectorReference);
		AssignRolesRoleAssignmentDataType roleAssignmentData = new AssignRolesRoleAssignmentDataType();
		RoleAssignerObjectType roleAssignerReference = new RoleAssignerObjectType();
		roleAssignerReference.setDescriptor("");
		RoleAssignerObjectIDType roleAssignerId = new RoleAssignerObjectIDType();
		roleAssignerId.setType("IID");
		roleAssignerId.setValue(iIdValue);
		roleAssignerReference.getID().add(roleAssignerId);
		roleAssignmentData.setRoleAssignerReference(roleAssignerReference);
		AssignableRoleObjectType assignableRoleReference = new AssignableRoleObjectType();
		assignableRoleReference.setDescriptor("");
		AssignableRoleObjectIDType assignableRoleReferenceId = new AssignableRoleObjectIDType();
		assignableRoleReferenceId.setType("Organization_Role_ID");
		assignableRoleReferenceId.setValue("Manager");
		assignableRoleReference.getID().add(assignableRoleReferenceId);
		roleAssignmentData.setAssignableRoleReference(assignableRoleReference);
		RoleeSelectorObjectType assigneesAddReference = new RoleeSelectorObjectType();
		assigneesAddReference.setDescriptor("");
		RoleeSelectorObjectIDType assigneesAddReferenceId = new RoleeSelectorObjectIDType();
		assigneesAddReferenceId.setType("Employee_ID");
		assigneesAddReferenceId.setValue(piPersonId);
		assigneesAddReference.getID().add(assigneesAddReferenceId);
		roleAssignmentData.getAssigneesToAddReference().add(assigneesAddReference);
		rolesEventData.getAssignRolesRoleAssignmentData().add(roleAssignmentData);
		request.setAssignRolesEventData(rolesEventData);
		return request;
	}

	private CreatePositionRequestType prepareCreatePositionObject(CreatePositionRequestType request, AwardManpowerResource resource, String pISuprgId, StringBuilder requestMessage) throws DatatypeConfigurationException, ParseException {
		request.setVersion(WORKDAY_API_VERSION);
		CreatePositionDataType createPositionData = new CreatePositionDataType();
		SupervisoryOrganizationObjectType supervisoryOrganization = new SupervisoryOrganizationObjectType();
		supervisoryOrganization.setDescriptor("");
		SupervisoryOrganizationObjectIDType supervisoryOrganizationId = new SupervisoryOrganizationObjectIDType();
		supervisoryOrganizationId.setValue(pISuprgId);
		supervisoryOrganizationId.setType("Organization_Reference_ID");
		supervisoryOrganization.getID().add(supervisoryOrganizationId);
		createPositionData.setSupervisoryOrganizationReference(supervisoryOrganization);
		com.polus.fibicomp.manpowerintegration.dto.recruiting.EventClassificationSubcategoryObjectType positionRequestReason = new com.polus.fibicomp.manpowerintegration.dto.recruiting.EventClassificationSubcategoryObjectType();
		positionRequestReason.setDescriptor("");
		com.polus.fibicomp.manpowerintegration.dto.recruiting.EventClassificationSubcategoryObjectIDType positionRequestGeneralId = new com.polus.fibicomp.manpowerintegration.dto.recruiting.EventClassificationSubcategoryObjectIDType();
		positionRequestGeneralId.setType("General_Event_Subcategory_ID");
		positionRequestGeneralId.setValue("GENERAL_EVENT_SUBCATEGORY-3-199");
		positionRequestReason.getID().add(positionRequestGeneralId);
		com.polus.fibicomp.manpowerintegration.dto.recruiting.EventClassificationSubcategoryObjectIDType positionRequestClassificationId = new com.polus.fibicomp.manpowerintegration.dto.recruiting.EventClassificationSubcategoryObjectIDType();
		positionRequestClassificationId.setType("Event_Classification_Subcategory_ID");
		positionRequestClassificationId.setValue("GENERAL_EVENT_SUBCATEGORY-3-199");
		positionRequestReason.getID().add(positionRequestClassificationId);
		createPositionData.setPositionRequestReasonReference(positionRequestReason);
		PositionDefinitionDataType positionDefinitionData = new PositionDefinitionDataType();
		positionDefinitionData.setPositionID("");
		if (resource.getPlanJobProfileTypeCode() != null) {
			positionDefinitionData.setJobPostingTitle(resource.getManpowerPlanJobProfileType().getDefaultJobTitle());
		} else {
			positionDefinitionData.setJobPostingTitle(resource.getManpowerJobProfileType().getDefaultJobTitle());
		}
		createPositionData.setPositionData(positionDefinitionData);
		Timestamp availabilityDate = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		Timestamp earliestHireDate = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		if (resource.getPlanStartDate().after(earliestHireDate)) {
			earliestHireDate = resource.getPlanStartDate();
		}
		XMLGregorianCalendar xmlCalendarAvailabilityDate  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(availabilityDate));
		xmlCalendarAvailabilityDate.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		XMLGregorianCalendar xmlCalendarEarliestHireDate  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(earliestHireDate));
		xmlCalendarEarliestHireDate.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		PositionGroupRestrictionDataType positionGroupRestriction = new PositionGroupRestrictionDataType();
		positionGroupRestriction.setAvailabilityDate(xmlCalendarAvailabilityDate);
		positionGroupRestriction.setEarliestHireDate(xmlCalendarEarliestHireDate);
		JobProfileObjectType jobProfile = new JobProfileObjectType();
		jobProfile.setDescriptor("");
		JobProfileObjectIDType jobProfileId = new JobProfileObjectIDType();
		jobProfileId.setType("Job_Profile_ID");
		if (resource.getPlanJobProfileTypeCode() != null) {
			jobProfileId.setValue(resource.getPlanJobProfileTypeCode());
		} else {
			jobProfileId.setValue(resource.getJobProfileTypeCode());
		}
		jobProfile.getID().add(jobProfileId);
		positionGroupRestriction.getJobProfileReference().add(jobProfile);
		createPositionData.setPositionGroupRestrictionsData(positionGroupRestriction);
		request.setCreatePositionData(createPositionData);
		requestMessage.append(pISuprgId).append("JobProfile= ").append(jobProfileId.getValue() == null ? "" : jobProfileId.getValue()).append("EarliestHireDate=").append(xmlCalendarEarliestHireDate.toString()).append("AvailabilityDate=").append(xmlCalendarAvailabilityDate.toString()).append("Default Job Title=").append(positionDefinitionData.getJobPostingTitle() == null ? "" : positionDefinitionData.getJobPostingTitle());
		return request;
	}

	@SuppressWarnings("unchecked")
	public Boolean assignCostAllocationInWorkday(WorkdayManpowerInterface manpowerInterface, Map<String, String> configDetails) throws XmlMappingException, IOException {
		Boolean success = false;
		String awardNumber = manpowerInterface.getAwardNumber();
		StringBuilder requestMessage = new StringBuilder("");
		String errorMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] {new CustomClientInterceptor(configDetails)};
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.payroll");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			AssignCostingAllocationRequestType request = new AssignCostingAllocationRequestType();
			request = prepareAssignCostingAllocationObject(request, manpowerInterface, requestMessage);
			com.polus.fibicomp.manpowerintegration.dto.payroll.ObjectFactory factory = new com.polus.fibicomp.manpowerintegration.dto.payroll.ObjectFactory();
			JAXBElement<AssignCostingAllocationRequestType> jaxbRequest = factory.createAssignCostingAllocationRequest(request);
			AssignCostingAllocationResponseType response = new AssignCostingAllocationResponseType();
			JAXBElement<AssignCostingAllocationResponseType> jaxbResponse = factory.createAssignCostingAllocationResponse(response);
			logger.info("Requesting for assignCostAllocationInWorkday {}", requestMessage);
			String apiUrl =  new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("CostingAllocation/").toString();
			logger.info("Requesting for {} ", apiUrl);
			jaxbResponse = (JAXBElement<AssignCostingAllocationResponseType>) template.marshalSendAndReceive(apiUrl, jaxbRequest);
			if (jaxbResponse != null) {
				response = jaxbResponse.getValue();
				if (response != null) {
					success = true;
					logger.info("Assign Costing allocation for {} ", requestMessage);
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_SUCCESS, "Assign Costing allocation success", 200, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				} else {
					logger.info("Empty response for Assign Costing allocation for {} ", requestMessage.toString());
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_ERROR, "Empty response for assignCostAllocationInWorkday ", 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				}
			} else {
				logger.info("Empty body response for Assign Costing allocation for {}", requestMessage.toString());
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_ERROR, "Empty body response for assignCostAllocationInWorkday ", 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
			} 
			} catch (SoapFaultClientException ex) {
				logger.error("SoapFaultClientError in assignCostAllocationInWorkday {}", ex.getMessage());
				SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
				if (soapFaultDetail != null) {
					SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
					Source detailSource = detailElementChild.getSource();
					Object detail = template.getUnmarshaller().unmarshal(detailSource);
					JAXBElement<com.polus.fibicomp.manpowerintegration.dto.payroll.ValidationFaultType> source = (JAXBElement<com.polus.fibicomp.manpowerintegration.dto.payroll.ValidationFaultType>) detail;
					if (source.getValue() != null && (source.getValue().getValidationError() != null && !source.getValue().getValidationError().isEmpty())) {
						for (com.polus.fibicomp.manpowerintegration.dto.payroll.ValidationErrorType error : source.getValue().getValidationError()) {
							saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_ERROR, error.getMessage(), 500, requestMessage.toString(), error.getXpath(), error.getDetailMessage(), manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
						}
					}
				} else {
					errorMessage = new StringBuilder().append("SoapFaultClientError in assignCostAllocationInWorkday : ").append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				}
			}
			catch (Exception e) {
				logger.error("Error in assignCostAllocationInWorkday {} ", e.getMessage());
				errorMessage = new StringBuilder().append("Error in assignCostAllocationInWorkday : ").append(e.getMessage()).toString();
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
			}
		return success;
	}

	private AssignCostingAllocationRequestType prepareAssignCostingAllocationObject(AssignCostingAllocationRequestType request, WorkdayManpowerInterface manpowerInterface, StringBuilder requestMessage) throws DatatypeConfigurationException {
		Timestamp allocationStartDate = manpowerInterface.getAwardManpowerResource().getChargeStartDate() == null ? manpowerInterface.getAwardManpowerResource().getPlanStartDate() : manpowerInterface.getAwardManpowerResource().getChargeStartDate();
		Timestamp allocationEndDate = manpowerInterface.getAwardManpowerResource().getChargeEndDate() == null ? manpowerInterface.getAwardManpowerResource().getPlanEndDate() : manpowerInterface.getAwardManpowerResource().getChargeEndDate();
		request.setVersion(WORKDAY_API_VERSION);
		CostingAllocationDataType costingAllocationData = new CostingAllocationDataType();
		com.polus.fibicomp.manpowerintegration.dto.payroll.WorkerObjectType worker = new com.polus.fibicomp.manpowerintegration.dto.payroll.WorkerObjectType();
		worker.setDescriptor("");
		com.polus.fibicomp.manpowerintegration.dto.payroll.WorkerObjectIDType workerId = new com.polus.fibicomp.manpowerintegration.dto.payroll.WorkerObjectIDType();
		workerId.setType("Employee_ID");
		workerId.setValue(manpowerInterface.getAwardManpowerResource().getPersonId());
		worker.getID().add(workerId);
		costingAllocationData.setWorkerReference(worker);
		CostingAllocationIntervalDataType costAllocationIntervalData = new CostingAllocationIntervalDataType();
		if (manpowerInterface.getIsCostAllocationCreate() != null && manpowerInterface.getIsCostAllocationCreate().equals("N")) {
			logger.info("Cost allocation Updation");
			CostingIntervalUpdateKeyType costingIntervalUpdateKey = new CostingIntervalUpdateKeyType();
			costingIntervalUpdateKey.setDelete(false);
			XMLGregorianCalendar xmlCalendarChargeStartDateUpdateKey  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(allocationStartDate));
			xmlCalendarChargeStartDateUpdateKey.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
			costingIntervalUpdateKey.setStartDateUpdateKey(xmlCalendarChargeStartDateUpdateKey);
			costAllocationIntervalData.getCostingIntervalUpdateKey().add(costingIntervalUpdateKey);
			requestMessage.append("ChargeStartDateUpdateKey=").append(xmlCalendarChargeStartDateUpdateKey.toString());
		}
		XMLGregorianCalendar xmlCalendarChargeStartDate  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(allocationStartDate));
		xmlCalendarChargeStartDate.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		costAllocationIntervalData.setStartDate(xmlCalendarChargeStartDate);
		XMLGregorianCalendar xmlCalendarChargeEndDate  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(allocationEndDate));
		xmlCalendarChargeEndDate.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		costAllocationIntervalData.setEndDate(xmlCalendarChargeEndDate);
		CostingAllocationDetailReplacementDataType costAllocationDetailData = new CostingAllocationDetailReplacementDataType();
		costAllocationDetailData.setOrder("a");
		costAllocationDetailData.setDefaultFromOrganizationAssignment(false);
		TenantedPayrollWorktagObjectType costWorktag = new TenantedPayrollWorktagObjectType();
		costWorktag.setDescriptor("");
		TenantedPayrollWorktagObjectIDType costWorktagId = new TenantedPayrollWorktagObjectIDType();
		costWorktagId.setType("Project_ID");
		costWorktagId.setValue(manpowerInterface.getAwardManpower().getBudgetReferenceNumber());
		costWorktag.getID().add(costWorktagId);
		costAllocationDetailData.getCostingOverrideWorktagReference().add(costWorktag);
		BigDecimal one = BigDecimal.ONE;
		costAllocationDetailData.setDistributionPercent(one);
		costAllocationIntervalData.getCostingAllocationDetailData().add(costAllocationDetailData);
		costingAllocationData.getCostingAllocationIntervalData().add(costAllocationIntervalData);
		request.setCostingAllocationData(costingAllocationData);
		requestMessage.append("EmployeeId=").append(workerId.getValue()).append("ChargeStartDate=").append(xmlCalendarChargeStartDate.toString()).append("ChargeEndDate=").append(xmlCalendarChargeEndDate.toString()).append("ProjectId/L2WBSNumber=").append(costWorktagId.getValue()).toString();
		return request;
	}

	@SuppressWarnings("unchecked")
	public Boolean freezeWorkdayPosition(String positionId, Timestamp awardEndDate, String awardNumber, Map<String, String> configDetails, StringBuilder freezeErrorMailContent, Timestamp oldFreezeDate, Integer awardId, Integer workdayManpowerInterfaceId) throws XmlMappingException, IOException {
		Boolean success = false;
		StringBuilder requestMessage = new StringBuilder("");
		StringBuilder freezeDate = oldFreezeDate == null ? null : new StringBuilder().append(", Freeze Date : ").append(commonService.convertDateFormatBasedOnTimeZone(oldFreezeDate.getTime(), Constants.DEFAULT_DATE_FORMAT));
		String errorMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] {new CustomClientInterceptor(configDetails)};
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.staffing");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			FreezePositionRequestType request = new FreezePositionRequestType();
			request = prepareFreezePositionObject(request, positionId, awardEndDate, requestMessage);
			com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory factory = new com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory();
			JAXBElement<FreezePositionRequestType> jaxBRequest = factory.createFreezePositionRequest(request);
			FreezePositionResponseType response = new FreezePositionResponseType();
			JAXBElement<FreezePositionResponseType> jaxbResponse = factory.createFreezePositionResponse(response);
			logger.info("Requesting for freezeWorkdayPosition for {}", requestMessage);
			String apiUrl =  new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("FreezePositions/").toString();
			logger.info("Requesting for {} ", apiUrl);
			jaxbResponse = (JAXBElement<FreezePositionResponseType>) template.marshalSendAndReceive(apiUrl, jaxBRequest);
			if (jaxbResponse != null) {
				response = jaxbResponse.getValue();
				if (response != null) {
					success = true;
					logger.info("Freeze position success {}", requestMessage.toString());
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_SUCCESS, "Freeze position Success", 200, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
				} else {
					logger.info("Empty response for freezeWorkdayPosition for {}", requestMessage.toString());
					freezeErrorMailContent.append(positionId).append(freezeDate == null ? "" : freezeDate).append("<br>").append("Empty response for freezeWorkdayPosition").append("<br>");
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_ERROR, "Empty response for freezeWorkdayPosition ", 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
				}
			} else {
				logger.info("Empty body response for freezeWorkdayPosition for {}", requestMessage.toString());
				freezeErrorMailContent.append(positionId).append(freezeDate == null ? "" : freezeDate).append("<br>").append("Empty body response for freezeWorkdayPosition").append("<br>");
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_ERROR, "Empty body response for freezeWorkdayPosition ", 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
			} 
		} catch (SoapFaultClientException ex) {
			logger.error("SoapFaultClientError in freezeWorkdayPosition {} ", ex.getMessage());
			SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
			if (soapFaultDetail != null) {
				SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
				Source detailSource = detailElementChild.getSource();
				Object detail = template.getUnmarshaller().unmarshal(detailSource);
				JAXBElement<ValidationFaultType> source = (JAXBElement<ValidationFaultType>) detail;
				if (source.getValue() != null && (source.getValue().getValidationError() != null && !source.getValue().getValidationError().isEmpty())) {
					freezeErrorMailContent.append(positionId).append(freezeDate == null ? "" : freezeDate).append("<br>");
					for (com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationErrorType error : source.getValue().getValidationError()) {
						freezeErrorMailContent.append(error.getMessage()).append("<br>");
						saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_ERROR, error.getMessage(), 500, requestMessage.toString(), error.getXpath(), error.getDetailMessage(), awardId, workdayManpowerInterfaceId);
					}
				}
			} else {
				errorMessage = new StringBuilder().append("SoapFaultClientError in freezeWorkdayPosition : ").append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
				freezeErrorMailContent.append(positionId).append(freezeDate == null ? "" : freezeDate).append("<br>").append(ex.getFaultStringOrReason()).append("<br>");
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
			}
		} catch (Exception e) {
			logger.error("Error in freezeWorkdayPosition {} ", e.getMessage());
			errorMessage = new StringBuilder().append("Error in freezeWorkdayPosition : ").append(e.getMessage()).toString();
			freezeErrorMailContent.append(positionId).append(freezeDate == null ? "" : freezeDate).append("<br>").append(e.getMessage()).append("<br>");
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage.toString(), null, null, awardId, workdayManpowerInterfaceId);
		}
		return success;
	}

	private FreezePositionRequestType prepareFreezePositionObject(FreezePositionRequestType request, String positionId, Timestamp awardEndDate, StringBuilder requestMessage) throws DatatypeConfigurationException {
		request.setVersion(WORKDAY_API_VERSION);
		PositionGroupFreezeDataType freezePositionData = new PositionGroupFreezeDataType();
		PositionRestrictionsObjectType positionRestriction = new PositionRestrictionsObjectType();
		positionRestriction.setDescriptor("");
		PositionRestrictionsObjectIDType positionRestrictionId = new PositionRestrictionsObjectIDType();
		positionRestrictionId.setValue(positionId);
		positionRestrictionId.setType("Position_ID");
		positionRestriction.getID().add(positionRestrictionId);
		freezePositionData.setPositionReference(positionRestriction);
		PositionGroupFreezeEventDataType positionFreezeEventdata = new PositionGroupFreezeEventDataType();
		EventClassificationSubcategoryObjectType eventSubCategory = new EventClassificationSubcategoryObjectType();
		eventSubCategory.setDescriptor("");
		EventClassificationSubcategoryObjectIDType eventSubCategoryId = new EventClassificationSubcategoryObjectIDType();
		eventSubCategoryId.setValue("GENERAL_EVENT_SUBCATEGORY-3-175");
		eventSubCategoryId.setType("Event_Classification_Subcategory_ID");
		eventSubCategory.getID().add(eventSubCategoryId);
		EventClassificationSubcategoryObjectIDType generalEventSubCategoryId = new EventClassificationSubcategoryObjectIDType();
		generalEventSubCategoryId.setValue("GENERAL_EVENT_SUBCATEGORY-3-175");
		generalEventSubCategoryId.setType("General_Event_Subcategory_ID");
		eventSubCategory.getID().add(generalEventSubCategoryId);
		positionFreezeEventdata.setReasonReference(eventSubCategory);
		XMLGregorianCalendar awardEndDateXml  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(awardEndDate));
		awardEndDateXml.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		positionFreezeEventdata.setFreezeDate(awardEndDateXml);
		positionFreezeEventdata.setFrozenPosition(true);
		freezePositionData.setFreezeEventData(positionFreezeEventdata);
		request.setFreezePositionData(freezePositionData);
		requestMessage.append("PositionId=").append(positionId).append("FreezeDate/AwardEndDate=").append(awardEndDateXml.toString()).toString();
		return request;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Boolean closeWorkdayPosition(String positionId, String awardNumber, StringBuilder errorMessage, Map<String, String> configDetails, Integer awardId) throws XmlMappingException, IOException {
		WorkdayManpowerInterface manpowerInterface = new WorkdayManpowerInterface();
		manpowerInterface.setInterfaceTypeCode(Constants.MANPOWER_INTERFACE_CLOSE_POSITION);
		manpowerInterface.setCreateTimestamp(commonDao.getCurrentTimestamp());
		manpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		manpowerInterface.setAwardId(awardId);
		manpowerInterface.setAwardNumber(awardNumber);
		manpowerIntegrationDao.saveOrUpdateManpowerInterface(manpowerInterface);
		AwardManpowerResource resource = manpowerIntegrationDao.getClosePositionResourceByPositionId(positionId);
		if (resource != null) {
			manpowerInterface.setAwardManpowerResourceId(resource.getManpowerResourceId());
			manpowerInterface.setAwardManpowerId(resource.getAwardManpowerId());
			manpowerInterface.setResourceUniqueId(resource.getResourceUniqueId());
		}
		Boolean success = Boolean.FALSE;
		StringBuilder requestMessage = new StringBuilder("");
		String errorLogMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] {new CustomClientInterceptor(configDetails)};
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.staffing");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			ClosePositionRequestType request = new ClosePositionRequestType();
			request = prepareClosePositionRequestObject(request, positionId, requestMessage);
			com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory factory = new com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory();
			JAXBElement<ClosePositionRequestType> jaxBRequest = factory.createClosePositionRequest(request);
			ClosePositionResponseType response = new ClosePositionResponseType();
			JAXBElement<ClosePositionResponseType> jaxbResponse = factory.createClosePositionResponse(response);
			logger.info("Requesting for closeWorkdayPosition for {}", requestMessage);
			String apiUrl = new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("ClosePositions/").toString();
			logger.info("Requesting for {} ", apiUrl);
			jaxbResponse = (JAXBElement<ClosePositionResponseType>) template.marshalSendAndReceive(apiUrl, jaxBRequest);
			if (jaxbResponse != null) {
				response = jaxbResponse.getValue();
				if (response != null) {
					String message = "";
					if (response.getPositionOrHeadcountReference() != null) {
						PositionGroupObjectType positionOrHeadCountReference = response.getPositionOrHeadcountReference();
						if (positionOrHeadCountReference.getDescriptor() != null) {
							message = positionOrHeadCountReference.getDescriptor();
						}
					}
					if (message.equals("")) {
						message = new StringBuilder("Close position Success for ").append(requestMessage).toString();
					}
					success = Boolean.TRUE;
					manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
					logger.info("Close position success for {}", requestMessage);
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CLOSE_POSITION, MESSAGE_TYPE_SUCCESS, message, 200, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				} else {
					manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);
					logger.info("Empty response for closeWorkdayPosition for {}", requestMessage);
					errorMessage.append(positionId).append(" : ").append("Empty response for closeWorkdayPosition").append("<br>");
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CLOSE_POSITION, MESSAGE_TYPE_ERROR, "Empty response for ClosePosition ", 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				}
			} else {
				manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);
				errorMessage.append(positionId).append(" : ").append("Empty body response for closeWorkdayPosition").append("<br>");
				logger.info("Empty body response for closeWorkdayPosition for {}", requestMessage);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CLOSE_POSITION, MESSAGE_TYPE_ERROR, "Empty body response for closeWorkdayPosition ", 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
			} 
		}  catch (SoapFaultClientException ex) {
			manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);
			logger.error("SoapFaultClientError in closeWorkdayPosition : {}", ex.getMessage());
			SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
			if (soapFaultDetail != null) {
				SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
				Source detailSource = detailElementChild.getSource();
				Object detail = template.getUnmarshaller().unmarshal(detailSource);
				JAXBElement<ValidationFaultType> source = (JAXBElement<ValidationFaultType>) detail;
				if (source.getValue() != null && (source.getValue().getValidationError() != null && !source.getValue().getValidationError().isEmpty())) {
					for (com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationErrorType error : source.getValue().getValidationError()) {
						errorMessage.append(positionId).append(" : ").append(error.getMessage()).append("<br>");
						logger.error("SoapFaultClientError in closeWorkdayPosition for PositionId =" + positionId + "AwardNumber = " + awardNumber);
						saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CLOSE_POSITION, MESSAGE_TYPE_ERROR, error.getMessage(), 500, requestMessage.toString(), error.getXpath(), error.getDetailMessage(), manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
					}
				}
				success = Boolean.TRUE;
			} else {
				errorLogMessage = new StringBuilder("SoapFaultClientError in closeWorkdayPosition ").append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
				logger.error(errorLogMessage);
				errorMessage.append(positionId).append(" : ").append(ex.getFaultStringOrReason()).append("<br>");
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CLOSE_POSITION, MESSAGE_TYPE_TECHNICAL_ERROR, errorLogMessage, 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (Exception e) {
			manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);
			logger.error("Error in closeWorkdayPosition : {}", e.getMessage());
			errorLogMessage = new StringBuilder("Error in closeWorkdayPosition : ").append(e.getMessage()).toString();
			errorMessage.append(positionId).append(" : ").append(e.getMessage()).append("<br>");
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CLOSE_POSITION, MESSAGE_TYPE_TECHNICAL_ERROR, errorLogMessage, 500, requestMessage.toString(), null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
		}
		manpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		manpowerInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
		manpowerIntegrationDao.saveOrUpdateManpowerInterface(manpowerInterface);
		return success;
	}

	private ClosePositionRequestType prepareClosePositionRequestObject(ClosePositionRequestType request, String positionId, StringBuilder requestMessage) throws DatatypeConfigurationException, ParseException {
		request.setVersion(WORKDAY_API_VERSION);
		ClosePositionDataType closePositionData = new ClosePositionDataType();
		PositionRestrictionsObjectType positionRestrictions = new PositionRestrictionsObjectType();
		positionRestrictions.setDescriptor("");
		PositionRestrictionsObjectIDType positionRestrictionsId = new PositionRestrictionsObjectIDType();
		positionRestrictionsId.setValue(positionId);
		positionRestrictionsId.setType("Position_ID");
		positionRestrictions.getID().add(positionRestrictionsId);
		closePositionData.setPositionReference(positionRestrictions);
		ClosePositionEventDataType closePositionEventData = new ClosePositionEventDataType();
		EventClassificationSubcategoryObjectType eventSubCategory = new EventClassificationSubcategoryObjectType();
		eventSubCategory.setDescriptor("");
		EventClassificationSubcategoryObjectIDType eventSubCategoryId = new EventClassificationSubcategoryObjectIDType();
		eventSubCategoryId.setValue("GENERAL_EVENT_SUBCATEGORY-3-187");
		eventSubCategoryId.setType("Event_Classification_Subcategory_ID");
		eventSubCategory.getID().add(eventSubCategoryId);
		EventClassificationSubcategoryObjectIDType generalEventSubCategoryId = new EventClassificationSubcategoryObjectIDType();
		generalEventSubCategoryId.setValue("GENERAL_EVENT_SUBCATEGORY-3-187");
		generalEventSubCategoryId.setType("General_Event_Subcategory_ID");
		eventSubCategory.getID().add(generalEventSubCategoryId);
		closePositionEventData.setReasonReference(eventSubCategory);
		XMLGregorianCalendar closeDateXml  = DatatypeFactory.newInstance().newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)));
		closeDateXml.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		closePositionEventData.setCloseDate(closeDateXml);
		closePositionData.getCloseEventData().add(closePositionEventData);
		request.setCloseRestrictionData(closePositionData);
		requestMessage.append("PositionId=").append(positionId).append("CloseDate=").append(closeDateXml.toString()).toString();
		return request;
	}

	@Override
	public void getWorkdayLongLeave(AtomicInteger rowCount) throws ParseException {
		String errorMessage = null;
		Map<String, String> configDetails = getWorkdayConfigDetails();
		Timestamp toDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		Timestamp fromDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		String dayDifference = "";
		dayDifference = manpowerIntegrationDao.getManpowerConfigurationValue("LONG_LEAVE_API_DAY_DIFFERENCE");
		if (dayDifference == null || dayDifference.equals("")) {
			dayDifference = "-6";
		}
		Calendar c1 = Calendar.getInstance();
		c1.setTime(toDateTime);
		c1.add(Calendar.DAY_OF_MONTH, 1);
		toDateTime.setTime(c1.getTime().getTime());
		fromDateTime = prepareFromDateTime(toDateTime, fromDateTime, dayDifference);
		String fromDate = convertTimestampToDate(fromDateTime);
		String toDate = convertTimestampToDate(toDateTime);
		List<String> personIds = new ArrayList<>();
		String requestMessage = new StringBuilder("Initiated_after_date=").append(fromDate)
				.append(" Initiated_before_date=").append(toDate).toString();
		String apiUrl = new StringBuilder(configDetails.get(Constants.WORKDAY_API))
				.append("LongLeave/?Initiated_after_date=").append(fromDate).append("T00:00:00.000-08:00")
				.append("&Initiated_before_date=").append(toDate).append("T00:00:00.000-08:00").append("&")
				.append(FORMAT_JSON).toString();
		logger.info("Requesting for getWorkdayLongLeave {} ", apiUrl);
		try {
			logger.info("Requesting for {}", apiUrl);
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			logger.info("Response Status {}", response.getStatusCode());
			ObjectMapper mapper = new ObjectMapper();
			WorkdayLongLeave longLeave = mapper.readValue(response.getBody(), WorkdayLongLeave.class);
			String successMessage = "";
			if (longLeave != null && (longLeave.getLongLeaveReportEntries() != null
					&& !longLeave.getLongLeaveReportEntries().isEmpty())) {
				Integer personCount = 0;
				for (LongLeaveReportEntry longLeaveRE : longLeave.getLongLeaveReportEntries()) {
					WorkdayLongLeaveDetails longLeaveData = null;
					if (longLeaveRE.getEmployeeId() != null && longLeaveRE.getInitiated() != null) {
						longLeaveData = manpowerIntegrationDao.getLongLeaveDataByPersonIdAndUniqueInitiated(longLeaveRE.getEmployeeId(), longLeaveRE.getInitiated());
					}
					if (longLeaveData == null && longLeaveRE.getEmployeeId() != null) {
						longLeaveData = new WorkdayLongLeaveDetails();
						personCount = personCount + 1;
						longLeaveData.setPersonId(longLeaveRE.getEmployeeId());
					}
					if (longLeaveRE.getName() != null) {
						longLeaveData.setName(longLeaveRE.getName());
					}
					if (longLeaveRE.getFirstDayOfLeave() != null) {
						longLeaveData.setFirstDayOfLeave(dateTimeService.stringToTimestamp(longLeaveRE.getFirstDayOfLeave()));
					}
					if (longLeaveRE.getLastDayOfLeaveEstimated() != null) {
						longLeaveData.setLastDayOfLeaveEstimated(dateTimeService.stringToTimestamp(longLeaveRE.getLastDayOfLeaveEstimated()));
					}
					if (longLeaveRE.getLastDayOfLeaveActual() != null) {
						longLeaveData.setLastDayOfLeaveActual(dateTimeService.stringToTimestamp(longLeaveRE.getLastDayOfLeaveActual()));
					}
					if (longLeaveRE.getJobFamily() != null) {
						longLeaveData.setJobFamily(longLeaveRE.getJobFamily());
					}
					if (longLeaveRE.getStatus() != null) {
						longLeaveData.setStatus(longLeaveRE.getStatus());
					}
					if (longLeaveRE.getInitiated() != null) {
						longLeaveData.setInitiated(dateTimeService.stringToTimestampWithTime(longLeaveRE.getInitiated()));
						longLeaveData.setUniqueInitiated(longLeaveRE.getInitiated());
					}
					if (longLeaveRE.getTitle() != null) {
						longLeaveData.setTitle(longLeaveRE.getTitle());
					}
					if (longLeaveRE.getnTUDepartment() != null) {
						longLeaveData.setnTUDepartment(longLeaveRE.getnTUDepartment());
					}
					if (longLeaveRE.getStatus() != null) {
						longLeaveData.setStatus(longLeaveRE.getStatus());
					}
					if (longLeaveRE.getaU() != null) {
						longLeaveData.setaU(longLeaveRE.getaU());
					}
					if (longLeaveRE.getBusinessTitle() != null) {
						longLeaveData.setBusinessTitle(longLeaveRE.getBusinessTitle());
					}
					if (longLeaveRE.getJobProfile() != null) {
						longLeaveData.setJobProfile(longLeaveRE.getJobProfile());
					}
					if (longLeaveRE.getLeaveTypeExcludingFamily() != null) {
						longLeaveData.setLeaveTypeExcludingFamily(longLeaveRE.getLeaveTypeExcludingFamily());
					}
					if (longLeaveRE.getSupervisoryOrganization() != null) {
						longLeaveData.setSupervisoryOrganization(longLeaveRE.getSupervisoryOrganization());
					}
					if (longLeaveRE.getApprovalDate() != null) {
						longLeaveData.setApprovalDate(dateTimeService.stringToTimestamp(longLeaveRE.getApprovalDate()));
					}
					rowCount.getAndIncrement();
					saveLongLeave(longLeaveData);
					personIds.add(longLeaveRE.getEmployeeId());
				}
				successMessage = new StringBuilder("LongLeave ReportEntries found for ").append(personCount)
						.append(" persons.").toString();
			} else {
				logger.info("No LongLeave ReportEntries found");
				successMessage = new StringBuilder("No LongLeave ReportEntries found").toString();
			}
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_LONG_LEAVE, MESSAGE_TYPE_SUCCESS, successMessage, 200, requestMessage, null, null, null, null);
			logger.info("personIds : {}", personIds);
			if (personIds != null && !personIds.isEmpty()) {
				prepareWorkdayLongLeaveDetailsAndSendMail(personIds);
			}
		} catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getWorkdayLongLeave : ").append(ex.getStatusText())
					.append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_LONG_LEAVE, MESSAGE_TYPE_ERROR, errorMessage,
					ex.getRawStatusCode(), requestMessage, null, null, null, null);
		} catch (Exception e) {
			e.printStackTrace();
			errorMessage = new StringBuilder("Error in getWorkdayLongLeave :").append(e.getMessage()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_LONG_LEAVE, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, null, null);
		} finally {
			manpowerIntegrationDao.deleteOldRowsBasedLongLeaveTriggerDate(calculateDayDifference(Constants.WOKRDAY_LONG_LEAVE_TRIGGER_DATE_DIFFERENCE));
		}
		logger.info("Long Leave Process Completed ");
	}

	private void prepareWorkdayLongLeaveDetailsAndSendMail(List<String> personIds) throws ParseException {
		Integer awardId = null;
		Timestamp currentTimestamp = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		currentTimestamp = removeTimeFactorFromDate(currentTimestamp);
		Map<String, List<AwardPersonVo>> longLeaveList = manpowerIntegrationDao.getActiveAwardsAndLeadUnitNumberByParams(personIds, currentTimestamp).stream()
				.collect(Collectors.groupingBy(AwardPersonVo::getLeadUnitNumber));
		for (Map.Entry<String, List<AwardPersonVo>> mapElement : longLeaveList.entrySet()) {
			String unitNumber = mapElement.getKey();
			logger.info("UnitNumber: {}", unitNumber);
			Map<String, String> placeHolder = new HashMap<>();
			List<AwardPersonVo> awardValue = mapElement.getValue();
			if (awardValue != null && awardValue.get(0) != null) {
				awardId = awardValue.get(0).getAward_Id();
				XSSFWorkbook longLeaveExcel = createLongLeaveExcel(awardValue);
				if (longLeaveExcel != null) {
					File report = commonService.createfileInUploads(longLeaveExcel, "Long_Leave_report_" + unitNumber + ".xlsx");
					EmailServiceVO emailServiceVO = new EmailServiceVO();
					emailServiceVO.setNotificationTypeId(Constants.MANPOWER_WORKDAY_TERMINATION_OR_LONG_LEAVE);
					emailServiceVO.setFileName(report);
					emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
					emailServiceVO.setSubModuleCode("0");
					emailServiceVO.setSubModuleItemKey("0");
					placeHolder.put("{EVENT_NAME}", "Long Leave of Absence");
					emailServiceVO.setPlaceHolder(placeHolder);	
					emailServiceVO.setModuleItemKey(awardId.toString());
					emailService.sendEmail(emailServiceVO);
				}
			}
		}
	}

	private XSSFWorkbook createLongLeaveExcel(List<AwardPersonVo> awardValue) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet("LongLeaveExcelDetails");
		commonService.addDetailsInHeader(workbook,sheet);
		Object[] tableHeadingRow = { "Award Number", "Title", "Full Name", "Person ID", "Lead Unit Name", "First Day Of leave",
				"Initiated", "Last Day of Leave Estimated", "Last Day Of Leave Actual", "Approval Date", "Status",
				"Job Family", "NTU Department", "AU", "BusinessTitle", "Job Family Group","Job Profile", "Leave Type Excluding Family", "Supervisory Organization" };
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		prepareExcelSheetHeader(sheet, tableHeadingRow, "LongLeaveExcelDetails", workbook, tableBodyStyle);
		int rowNumber = 2;
		for (AwardPersonVo awarPerson : awardValue) {
			List<WorkdayLongLeaveDetails> longLeaveDetails = manpowerIntegrationDao.getLongLeaveDetailsByPersonId(awarPerson.getPersonaId());
			for (WorkdayLongLeaveDetails longLeaveData : longLeaveDetails) {
				Row row = sheet.createRow(rowNumber++);
				int cellNumber = 0;
				Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getAwardNumber() != null)
					cell1.setCellValue(awarPerson.getAwardNumber());
				else
					cell1.setCellValue(" ");
				cellNumber++;

				Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getTitle() != null)
					cell2.setCellValue(longLeaveData.getTitle());
				else
					cell2.setCellValue(" ");
				cellNumber++;
				
				Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getFullName() != null)
					cell3.setCellValue(awarPerson.getFullName());
				else
					cell3.setCellValue(" ");
				cellNumber++;

				Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getPersonaId() != null)
					cell4.setCellValue(awarPerson.getPersonaId());
				else
					cell4.setCellValue(" ");
				cellNumber++;

				Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getLeadUnitName() != null)
					cell5.setCellValue(awarPerson.getLeadUnitName());
				else
					cell5.setCellValue(" ");
				cellNumber++;

				Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getFirstDayOfLeave() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(longLeaveData.getFirstDayOfLeave().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell6.setCellValue((String) dateValue);
				} else {
					cell6.setCellValue(" ");
				}
				cellNumber++;

				Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getInitiated() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(longLeaveData.getInitiated().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell7.setCellValue(dateValue);
				}else {
					cell7.setCellValue(" ");
				}
				cellNumber++;

				Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getLastDayOfLeaveEstimated() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(longLeaveData.getLastDayOfLeaveEstimated().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell8.setCellValue((String) dateValue);
				} else {
					cell8.setCellValue(" ");
				}
				cellNumber++;

				Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getLastDayOfLeaveActual() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(longLeaveData.getLastDayOfLeaveActual().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell9.setCellValue((String) dateValue);
				} else {
					cell9.setCellValue(" ");
				}
				cellNumber++;

				Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getApprovalDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(longLeaveData.getApprovalDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell10.setCellValue((String) dateValue);
				} else {
					cell10.setCellValue(" ");
				}
				cellNumber++;

				Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getStatus() != null)
					cell11.setCellValue(longLeaveData.getStatus());
				else
					cell11.setCellValue(" ");
				cellNumber++;
				
				Cell cell12 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getJobFamily() != null)
					cell12.setCellValue(longLeaveData.getJobFamily());
				else
					cell12.setCellValue(" ");
				cellNumber++;
				
				Cell cell13 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getnTUDepartment() != null)
					cell13.setCellValue(longLeaveData.getnTUDepartment());
				else
					cell13.setCellValue(" ");
				cellNumber++;
				
				Cell cell14 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getaU() != null)
					cell14.setCellValue(longLeaveData.getaU());
				else
					cell14.setCellValue(" ");
				cellNumber++;
				
				Cell cell15 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getBusinessTitle() != null)
					cell15.setCellValue(longLeaveData.getBusinessTitle());
				else
					cell15.setCellValue(" ");
				cellNumber++;
				
				Cell cell16 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getJobFamilyGroup() != null)
					cell16.setCellValue(longLeaveData.getJobFamilyGroup());
				else
					cell16.setCellValue(" ");
				cellNumber++;
				
				Cell cell17 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getJobProfile() != null)
					cell17.setCellValue(longLeaveData.getJobProfile());
				else
					cell17.setCellValue(" ");
				cellNumber++;
				
				Cell cell18 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getLeaveTypeExcludingFamily() != null)
					cell18.setCellValue(longLeaveData.getLeaveTypeExcludingFamily());
				else
					cell18.setCellValue(" ");
				cellNumber++;
				
				Cell cell19 = assignCell(cellNumber, tableBodyStyle, row);
				if (longLeaveData.getSupervisoryOrganization() != null)
					cell19.setCellValue(longLeaveData.getSupervisoryOrganization());
				else
					cell19.setCellValue(" ");
				cellNumber++;				
			}
		}
		return workbook;
	}

	private void saveLongLeave(WorkdayLongLeaveDetails longLeaveData) {
		try {
			longLeaveData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			longLeaveData.setUpdateUser(UPDATE_USER);
			longLeaveData.setTriggerDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationDao.saveOrUpdateLongLeaveData(longLeaveData);
		} catch (Exception e) {
			logger.error("Error in saveLongLeave Data{}", e);
		}
	}

	private HttpHeaders setWorkdayHttpHeaders(HttpHeaders headers, Map<String, String> configDetails) {
		headers.set(Constants.WORKDAY_SUBSCRIPTION_KEY_NAME, configDetails.get(Constants.WORKDAY_SUBSCRIPTION_KEY_VALUE).toString());
//		if (configDetails.get("IP_ADDRESS") != null) {
//			String ip = configDetails.get("IP_ADDRESS").toString();
//			headers.set("X-Forwarded-For", ip);
//		}
		return headers;
	}

	public String generateAwardManpowerLinkToApplication(Integer awardId) {
		return Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_MANPOWER_PATH + awardId + Constants.APPLICATION_URL_END_TAG;
	}

	@Override
	public Timestamp prepareFromDateTime(Timestamp toDateTime, Timestamp fromDateTime, String dayDifference) {
		Calendar c = Calendar.getInstance();
		c.setTime(toDateTime);
		c.add(Calendar.DAY_OF_MONTH, Integer.parseInt(dayDifference));
		fromDateTime.setTime(c.getTime().getTime());
		return fromDateTime;
	}

	private String convertTimestampToDate(Timestamp timestamp) {
		long currentTime = timestamp.getTime();
		DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
		return String.valueOf(formatter.format(currentTime));
	}

	@Override
	public void getWorkdayTerminations(AtomicInteger rowCount) throws ParseException {
		Timestamp toDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		Timestamp fromDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		String dayDifference = "";
		Map<String, String> configDetails = getWorkdayConfigDetails();
		dayDifference = manpowerIntegrationDao.getManpowerConfigurationValue("TERMINATIONS_API_DAY_DIFFERENCE");
		Calendar c1 = Calendar.getInstance();
		c1.setTime(toDateTime);
		c1.add(Calendar.DAY_OF_MONTH, 1);
		toDateTime.setTime(c1.getTime().getTime());
		if (dayDifference == null || dayDifference.equals("")) {
			dayDifference = "-6";
		}
		fromDateTime = prepareFromDateTime(toDateTime, fromDateTime, dayDifference);
		String fromDate = convertTimestampToDate(fromDateTime);
		String toDate = convertTimestampToDate(toDateTime);
		String errorMessage = null;
		List<String> personIds = new ArrayList<>();
		String requestMessage = new StringBuilder("Initiated_Date_On_or_After=").append(fromDate)
				.append("Initiated_Date_On_or_Before=").append(toDate).toString();
		String apiUrl = new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("Terminations/?")
				.append("&Initiated_Date_On_or_Before=").append(toDate).append("T00:00:00.000-08:00")
				.append("&Initiated_Date_On_or_After=").append(fromDate).append("T00:00:00.000-08:00").append("&")
				.append(FORMAT_JSON).toString();
		logger.info("Requesting for getWorkdayTerminations {} ", apiUrl);
		try {
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			logger.info("Response Status {}", response.getStatusCode());
			ObjectMapper mapper = new ObjectMapper();
			WorkdayTerminations terminations = mapper.readValue(response.getBody(), WorkdayTerminations.class);
			String successMessage = "";
			if (terminations != null && (terminations.getTerminationReportEntries() != null
					&& !terminations.getTerminationReportEntries().isEmpty())) {
				Integer personCount = 0;
				
				for (TerminationsReportEntry terminationEntry : terminations.getTerminationReportEntries()) {
					WorkdayTerminationDetails terminationDetails = null;
					String employeeId = terminationEntry.getEmployeeID();
					if(employeeId != null && terminationEntry.getOverallEventInitiationDate() != null) {
						terminationDetails = manpowerIntegrationDao.getTerminationDataByEmployeeIdAndEventInitiation(employeeId, terminationEntry.getOverallEventInitiationDate());
					}		
					if (terminationDetails == null && employeeId != null) {
						terminationDetails = new WorkdayTerminationDetails();
						personCount = personCount + 1;
						terminationDetails.setPersonId(employeeId);
					}
					if (terminationEntry.getName() != null) {
						terminationDetails.setName(terminationEntry.getName());
					}
					if (terminationEntry.getTerminationApproveDate() != null) {
						terminationDetails.setTerminationApproveDate(
								dateTimeService.stringToTimestamp(terminationEntry.getTerminationApproveDate()));
					}
					if (terminationEntry.getWorkerProposedTerminationDate() != null) {
						terminationDetails.setWorkerProposedTerminationDate(
								dateTimeService.stringToTimestamp(terminationEntry.getWorkerProposedTerminationDate()));
					}
					if (terminationEntry.getNoticePeriod() != null) {
						terminationDetails.setNoticePeriod(terminationEntry.getNoticePeriod());
					}
					if (terminationEntry.getJobFamily() != null) {
						terminationDetails.setJobFamily(terminationEntry.getJobFamily());
					}
					if (terminationEntry.getTerminationEventStatus() != null) {
						terminationDetails.setTerminationEventStatus(terminationEntry.getTerminationEventStatus());
					}
					if (terminationEntry.getOverallEventStatus() != null) {
						terminationDetails.setOverallEventStatus(terminationEntry.getOverallEventStatus());
					}
					if (terminationEntry.getTitle() != null) {
						terminationDetails.setTitle(terminationEntry.getTitle());
					}
					if (terminationEntry.getnTUDepartment() != null) {
						terminationDetails.setnTUDepartment(terminationEntry.getnTUDepartment());
					}
					if (terminationEntry.getOverallEventInitiationDate() != null) {
						terminationDetails.setOverallEventInitiationDate(dateTimeService.stringToTimestampWithTime(terminationEntry.getOverallEventInitiationDate()));
						terminationDetails.setUniqueEventInitiationDate(terminationEntry.getOverallEventInitiationDate());
					}
					if (terminationEntry.getaU() != null) {
						terminationDetails.setaU(terminationEntry.getaU());
					}
					if (terminationEntry.getBusinessTitle() != null) {
						terminationDetails.setBusinessTitle(terminationEntry.getBusinessTitle());
					}
					if (terminationEntry.getJobFamilyGroup() != null) {
						terminationDetails.setJobFamilyGroup(terminationEntry.getJobFamilyGroup());
					}
					if (terminationEntry.getBusinessProcessName() != null) {
						terminationDetails.setBusinessProcessName(terminationEntry.getBusinessProcessName());
					}
					if (terminationEntry.getJobProfile() != null) {
						terminationDetails.setJobProfile(terminationEntry.getJobProfile());
					}
					if (terminationEntry.getRecommendedSystemTerminationDate() != null) {
						terminationDetails.setRecommendedSystemTerminationDate(dateTimeService
								.stringToTimestamp(terminationEntry.getRecommendedSystemTerminationDate()));
					}
					if (terminationEntry.getSupervisoryOrganization() != null) {
						terminationDetails.setSupervisoryOrganization(terminationEntry.getSupervisoryOrganization());
					}
					if (terminationEntry.getWorkerResignationNotificationDate() != null) {
						terminationDetails.setWorkerResignationNotificationDate(dateTimeService
								.stringToTimestamp(terminationEntry.getWorkerResignationNotificationDate()));
					}
					if (terminationEntry.getWorkerProposedResignationnDate() != null) {
						terminationDetails.setWorkerProposedResignationnDate(dateTimeService
								.stringToTimestamp(terminationEntry.getWorkerProposedResignationnDate()));
					}
					if (terminationEntry.getResignationApproveDate() != null) {
						terminationDetails.setResignationApproveDate(
								dateTimeService.stringToTimestamp(terminationEntry.getResignationApproveDate()));
					}
					if (terminationEntry.getResignationEventStatus() != null) {
						terminationDetails.setResignationEventStatus(terminationEntry.getResignationEventStatus());
					}
					rowCount.getAndIncrement();
					saveOrUpdateWorkdayTerminationDetails(terminationDetails);
					personIds.add(terminationEntry.getEmployeeID());
				}
				successMessage = new StringBuilder("Termination/Resignation ReportEntries found for ").append(personCount).append(" persons.").toString();
			} else {
				successMessage = new StringBuilder("No Termination/Resignation ReportEntries found").toString();
				logger.info(successMessage);
			}
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_TERMINATION_AND_RESIGNATION, MESSAGE_TYPE_SUCCESS, successMessage, 200, requestMessage, null, null, null, null);
			logger.info("personIds : {}", personIds);
			if (personIds != null && !personIds.isEmpty()) {
				prepareWorkdayTerminationDetailsAndSendMail(personIds);
			}				
		} catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getWorkdayTerminations : ").append(ex.getStatusText())
					.append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_TERMINATION_AND_RESIGNATION, MESSAGE_TYPE_ERROR,
					errorMessage, ex.getRawStatusCode(), requestMessage, null, null, null, null);
		} catch (Exception e) {
			e.printStackTrace();
			errorMessage = new StringBuilder("Error in getWorkdayTerminations :").append(e.getMessage()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_TERMINATION_AND_RESIGNATION, MESSAGE_TYPE_ERROR,
					errorMessage, 500, requestMessage, null, null, null, null);
		} finally {			
			manpowerIntegrationDao.deleteOldRowsBasedTerminationTriggerDate(calculateDayDifference(Constants.WORKDAY_TERMINATION_TRIGGER_DATE_DIFFERENCE));		
		}
		logger.info("Workday Termination Process Completed");
	}

	private void prepareWorkdayTerminationDetailsAndSendMail(List<String> personIds) throws ParseException {
		Integer awardId = null;
		Timestamp currentTimestamp = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		currentTimestamp = removeTimeFactorFromDate(currentTimestamp);
		Map<String, List<AwardPersonVo>> terminationList = manpowerIntegrationDao.getActiveAwardsAndLeadUnitNumberByParams(personIds, currentTimestamp).stream()
				.collect(Collectors.groupingBy(AwardPersonVo::getLeadUnitNumber));				
		for (Map.Entry<String, List<AwardPersonVo>> mapElement : terminationList.entrySet()) {					
			String unitNumber = mapElement.getKey();
			logger.info("UnitNumber: {}", unitNumber);
			Map<String, String> placeHolder = new HashMap<>();
			List<AwardPersonVo> awardValue = mapElement.getValue();
			if (awardValue != null && awardValue.get(0) != null) {
				awardId = awardValue.get(0).getAward_Id();
				XSSFWorkbook TerminationExcel = createTerminationExcel(awardValue);
				if (TerminationExcel != null) {
					File report = commonService.createfileInUploads(TerminationExcel,"Termination_report_" + unitNumber + ".xlsx");
					EmailServiceVO emailServiceVO = new EmailServiceVO();
					emailServiceVO.setNotificationTypeId(Constants.MANPOWER_WORKDAY_TERMINATION_OR_LONG_LEAVE);
					emailServiceVO.setFileName(report);
					emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
					emailServiceVO.setSubModuleCode("0");
					emailServiceVO.setSubModuleItemKey("0");							
					placeHolder.put("{EVENT_NAME}", "Termination Details");
					emailServiceVO.setPlaceHolder(placeHolder);							
					emailServiceVO.setModuleItemKey(awardId.toString());							
					emailService.sendEmail(emailServiceVO);
				}
			}
		}
	}

	private Timestamp calculateDayDifference(String triggerDateParam) throws ParseException {
		Timestamp toDateTimeStamp = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		Timestamp deleteFromDate = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		String triggerDateDifference = "";
		triggerDateDifference = manpowerIntegrationDao.getManpowerConfigurationValue(triggerDateParam);
		Calendar c2 = Calendar.getInstance();
		c2.setTime(toDateTimeStamp);
		c2.add(Calendar.DAY_OF_MONTH, 1);
		toDateTimeStamp.setTime(c2.getTime().getTime());
		deleteFromDate = prepareFromDateTime(toDateTimeStamp, deleteFromDate, triggerDateDifference);
		logger.info("deleteFromDate : {}", deleteFromDate);
		return deleteFromDate;
	}

	private XSSFWorkbook createTerminationExcel(List<AwardPersonVo> value) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet("TerminationListDetails");
		commonService.addDetailsInHeader(workbook,sheet);
		Object[] tableHeadingRow = { "Award Number", "Title", "Full Name", "Person ID", "Lead Unit Name",
				"Termination Event Status", "Overall Event Status", "Overall Event Initiation Date",
				"Termination Approve Date", "Notice Period", "Resignation Event Status", "JobFamily", 
				"NTU Department", "AU", "Business Title", "Job Family Group", "Business Process Name", 
				"Job Profile", "Recommended System Termination Date", "Supervisory Organization", "Worker Proposed Termination Date", 
				"Worker Resignation Notification Date", "Worker Proposed Resignation Date", "Resignation Approve Date" };
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		prepareExcelSheetHeader(sheet, tableHeadingRow, "TerminationListDetails", workbook, tableBodyStyle);
		int rowNumber = 2;
		for (AwardPersonVo awarPerson : value) {
			List<WorkdayTerminationDetails> terminationData = manpowerIntegrationDao.getTerminationDetailsByPersonId(awarPerson.getPersonaId());
			for (WorkdayTerminationDetails terminationDetails : terminationData) {
				Row row = sheet.createRow(rowNumber++);
				int cellNumber = 0;
				Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getAwardNumber() != null)
					cell1.setCellValue(awarPerson.getAwardNumber());
				else
					cell1.setCellValue(" ");
				cellNumber++;

				Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getTitle() != null)
					cell2.setCellValue(terminationDetails.getTitle());
				else
					cell2.setCellValue(" ");
				cellNumber++;
				
				Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getFullName() != null)
					cell3.setCellValue(awarPerson.getFullName());
				else
					cell3.setCellValue(" ");
				cellNumber++;

				Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getPersonaId() != null)
					cell4.setCellValue(awarPerson.getPersonaId());
				else
					cell4.setCellValue(" ");
				cellNumber++;

				Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
				if (awarPerson.getLeadUnitName() != null)
					cell5.setCellValue(awarPerson.getLeadUnitName());
				else
					cell5.setCellValue(" ");
				cellNumber++;

				Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getTerminationEventStatus() != null)
					cell6.setCellValue(terminationDetails.getTerminationEventStatus());
				else
					cell6.setCellValue(" ");
				cellNumber++;

				Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getOverallEventStatus() != null)
					cell7.setCellValue(terminationDetails.getOverallEventStatus());
				else
					cell7.setCellValue(" ");
				cellNumber++;

				Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getOverallEventInitiationDate() != null) {				
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getOverallEventInitiationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell8.setCellValue(dateValue);
				} else {					
					cell8.setCellValue(" ");
				}
				cellNumber++;

				Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getTerminationApproveDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getTerminationApproveDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell9.setCellValue((String) dateValue);
				} else {
					cell9.setCellValue(" ");
				}
				cellNumber++;

				Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getNoticePeriod() != null)
					cell10.setCellValue(terminationDetails.getNoticePeriod());
				else
					cell10.setCellValue(" ");
				cellNumber++;

				Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getResignationEventStatus() != null)
					cell11.setCellValue(terminationDetails.getResignationEventStatus());
				else
					cell11.setCellValue(" ");
				cellNumber++;
				
				Cell cell12 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getJobFamily() != null)
					cell12.setCellValue(terminationDetails.getJobFamily());
				else
					cell12.setCellValue(" ");
				cellNumber++;
				
				Cell cell13 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getnTUDepartment() != null)
					cell13.setCellValue(terminationDetails.getnTUDepartment());
				else
					cell13.setCellValue(" ");
				cellNumber++;
				
				Cell cell14 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getaU() != null)
					cell14.setCellValue(terminationDetails.getaU());
				else
					cell14.setCellValue(" ");
				cellNumber++;
				
				Cell cell15 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getBusinessTitle() != null)
					cell15.setCellValue(terminationDetails.getBusinessTitle());
				else
					cell15.setCellValue(" ");
				cellNumber++;
				
				Cell cell16 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getJobFamilyGroup() != null)
					cell16.setCellValue(terminationDetails.getJobFamilyGroup());
				else
					cell16.setCellValue(" ");
				cellNumber++;
				
				Cell cell17 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getBusinessProcessName() != null)
					cell17.setCellValue(terminationDetails.getBusinessProcessName());
				else
					cell17.setCellValue(" ");
				cellNumber++;
				
				Cell cell18 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getJobProfile() != null)
					cell18.setCellValue(terminationDetails.getJobProfile());
				else
					cell18.setCellValue(" ");
				cellNumber++;
				
				Cell cell19 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getRecommendedSystemTerminationDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getRecommendedSystemTerminationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell19.setCellValue(dateValue);
				}
				else
					cell19.setCellValue(" ");
				cellNumber++;
				
				Cell cell20 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getSupervisoryOrganization() != null)
					cell20.setCellValue(terminationDetails.getSupervisoryOrganization());
				else
					cell20.setCellValue(" ");
				cellNumber++;
				
				Cell cell21 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getWorkerProposedTerminationDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getWorkerProposedTerminationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell21.setCellValue(dateValue);
				}
				else
					cell21.setCellValue(" ");
				cellNumber++;
				
				Cell cell22 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getWorkerResignationNotificationDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getWorkerResignationNotificationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell22.setCellValue(dateValue);
				}
				else
					cell22.setCellValue(" ");
				cellNumber++;
				
				Cell cell23 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getWorkerProposedResignationnDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getWorkerProposedResignationnDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell23.setCellValue(dateValue);
				}
				else
					cell23.setCellValue(" ");
				cellNumber++;
				
				Cell cell24 = assignCell(cellNumber, tableBodyStyle, row);
				if (terminationDetails.getResignationApproveDate() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(terminationDetails.getResignationApproveDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell24.setCellValue(dateValue);
				}
				else
					cell24.setCellValue(" ");
				cellNumber++;
			}
		}
		return workbook;
	}

	private XSSFWorkbook prepareExcelSheetHeader(XSSFSheet sheet, Object[] tableHeadingRow, String documentHeading,
			XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle) {
		int headingCellNumber = 0;
		Row headerRow = sheet.createRow(0);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue(documentHeading);
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, tableHeadingRow.length - 1));
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		Row tableHeadRow = sheet.createRow(1);
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		// Table body style and font creation code.
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		// Set table head data to each column.
		for (Object heading : tableHeadingRow) {
			Cell cell = tableHeadRow.createCell(headingCellNumber++);
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
		}
		return workbook;
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	private void saveOrUpdateWorkdayTerminationDetails(WorkdayTerminationDetails terminationDetails) {
		try {
			terminationDetails.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			terminationDetails.setUpdateUser(UPDATE_USER);
			terminationDetails.setTriggerDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
			manpowerIntegrationDao.saveOrUpdateWorkdayTerminations(terminationDetails);
		} catch (Exception e) {
			logger.error("Error in saveWorkdayTerminationDetails{}", e);
		}

	}

	@SuppressWarnings("unchecked")
	public Boolean moveWorkersByOrganizationInWorkday(WorkdayManpowerInterface manpowerInterface, String positionId, String oldSupOrg, String newSupOrg, Map<String, String> configdetails, String personId)
			throws XmlMappingException, IOException {
		Boolean success = false;
		String requestMessage = new StringBuilder("FromSupOrg=").append(oldSupOrg).append(" ToSupOrg=")
				.append(newSupOrg).append(" PositionIds=").toString();
		String awardNumber = manpowerInterface.getAwardNumber();
		requestMessage = new StringBuilder(requestMessage).append(",").append(positionId).append(" Employee_ID=")
				.append(personId).append(",").toString();
		requestMessage = requestMessage.replaceAll(",$", "");
		String errorMessage = "";
		try {
			ClientInterceptor[] interceptors = new ClientInterceptor[] { new CustomClientInterceptor(configdetails) };
			marshaller = new Jaxb2Marshaller();
			marshaller.setPackagesToScan("com.polus.fibicomp.manpowerintegration.dto.staffing");
			template = new WebServiceTemplate(marshaller);
			template.setInterceptors(interceptors);
			MoveWorkersByOrganizationRequestType request = new MoveWorkersByOrganizationRequestType();
			request = prepareMoveWorkersByOrganizationObject(request, positionId, oldSupOrg, newSupOrg, personId);
			com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory factory = new com.polus.fibicomp.manpowerintegration.dto.staffing.ObjectFactory();
			JAXBElement<MoveWorkersByOrganizationRequestType> jaxbRequest = factory
					.createMoveWorkersByOrganizationRequest(request);
			MoveWorkersByOrganizationResponseType response = new MoveWorkersByOrganizationResponseType();
			JAXBElement<MoveWorkersByOrganizationResponseType> jaxbResponse = factory.createMoveWorkersByOrganizationResponse(response);
			logger.info("Requesting for MoveWorkersByOrganization for {}", requestMessage);
			String apiUrl = new StringBuilder(configdetails.get(Constants.WORKDAY_API))
					.append("MoveWorker/Organization/").toString();
			logger.info("Requesting for " + apiUrl);
			jaxbResponse = (JAXBElement<MoveWorkersByOrganizationResponseType>) template.marshalSendAndReceive(apiUrl,
					jaxbRequest);
			if (jaxbResponse != null) {
				response = jaxbResponse.getValue();
				if (response != null) {
					success = true;
					logger.info("MoveWorkersByOrganization success for {}", requestMessage);
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_SUCCESS,
							"MoveWorkersByOrganization success", 200, requestMessage, null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				} else {
					logger.info("Empty Response for MoveWorkersByOrganization for {}", requestMessage);
					saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_SUCCESS,
							"Empty Response for MoveWorkersByOrganization", 500, requestMessage, null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
				}
			} else {
				logger.info("Empty body Response for MoveWorkersByOrganization for {}", requestMessage);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_SUCCESS,
						"Empty body Response for MoveWorkersByOrganization", 500, requestMessage, null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (SoapFaultClientException ex) {
			logger.error("SoapFaultClientError in moveWorkersByOrganizationInWorkday {} ", ex.getMessage());
			SoapFaultDetail soapFaultDetail = ex.getSoapFault().getFaultDetail();
			if (soapFaultDetail != null) {
				SoapFaultDetailElement detailElementChild = soapFaultDetail.getDetailEntries().next();
				Source detailSource = detailElementChild.getSource();
				Object detail = template.getUnmarshaller().unmarshal(detailSource);
				JAXBElement<com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationFaultType> source = (JAXBElement<com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationFaultType>) detail;
				if (source.getValue() != null && (source.getValue().getValidationError() != null
						&& !source.getValue().getValidationError().isEmpty())) {
					for (com.polus.fibicomp.manpowerintegration.dto.staffing.ValidationErrorType error : source
							.getValue().getValidationError()) {
						logger.error(error.getMessage());
						saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_ERROR,
								error.getMessage(), 500, requestMessage, error.getXpath(), error.getDetailMessage(), manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
					}
				}
			} else {
				errorMessage = new StringBuilder()
						.append("SoapFaultClientError in moveWorkersByOrganizationInWorkday : ")
						.append(ex.getFaultCode().toString()).append(ex.getFaultStringOrReason()).toString();
				logger.error(errorMessage);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_ERROR,
						errorMessage, 500, requestMessage, null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (Exception e) {
			errorMessage = new StringBuilder().append("Error in moveWorkersByOrganizationInWorkday : ")
					.append(e.getMessage()).toString();
			logger.error(errorMessage);
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_ERROR, errorMessage,
					500, requestMessage, null, null, manpowerInterface.getAwardId(), manpowerInterface.getWorkdayManpowerInterfaceId());
		}
		return success;
	}

		private MoveWorkersByOrganizationRequestType prepareMoveWorkersByOrganizationObject(MoveWorkersByOrganizationRequestType request, String positionId, String oldSupOrg, String newSupOrg,
				String personId) throws DatatypeConfigurationException, ParseException {
			request.setVersion(WORKDAY_API_VERSION);
			MoveWorkersByOrganizationDataType organizationData = new MoveWorkersByOrganizationDataType();
			XMLGregorianCalendar xmlCalendar = DatatypeFactory.newInstance()
					.newXMLGregorianCalendar(getXMLGregorianCalendarDateFromTimestamp(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)));
			xmlCalendar.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
			organizationData.setEffectiveDate(xmlCalendar);
			StaffObjectType fromStaffObject = new StaffObjectType();
			fromStaffObject.setDescriptor("");
			StaffObjectIDType fromStaffObjectId = new StaffObjectIDType();
			fromStaffObjectId.setValue(oldSupOrg);
			fromStaffObjectId.setType("Organization_Reference_ID");
			fromStaffObject.getID().add(fromStaffObjectId);
			organizationData.setFromOrganizationReference(fromStaffObject);
			StaffObjectType toStaffObject = new StaffObjectType();
			toStaffObject.setDescriptor("");
			StaffObjectIDType toStaffObjectId = new StaffObjectIDType();
			toStaffObjectId.setValue(newSupOrg);
			toStaffObjectId.setType("Organization_Reference_ID");
			toStaffObject.getID().add(toStaffObjectId);
			organizationData.setToOrganizationReference(toStaffObject);
			List<MoveWorkersByOrganizationPositionDataType> moveWorkers = new ArrayList<>();
			MoveWorkersByOrganizationPositionDataType moveWorker = new MoveWorkersByOrganizationPositionDataType();
			StaffingInterfaceObjectType staffInterface = new StaffingInterfaceObjectType();
			staffInterface.setDescriptor("");
			StaffingInterfaceObjectIDType staffInterfaceId = new StaffingInterfaceObjectIDType();
			staffInterfaceId.setValue(positionId);
			staffInterfaceId.setType("Position_ID");
			staffInterface.getID().add(staffInterfaceId);
			moveWorker.setPositionToMoveReference(staffInterface);
			if (personId != null && !personId.isEmpty()) {
				WorkerObjectType workerForPositionReference = new WorkerObjectType();
				workerForPositionReference.setDescriptor("");
				WorkerObjectIDType workerObjectIDType = new WorkerObjectIDType();
				workerObjectIDType.setType("Employee_ID");
				workerObjectIDType.setValue(personId);
				workerForPositionReference.getID().add(workerObjectIDType);
				moveWorker.setWorkerForPositionReference(workerForPositionReference);
			}
			moveWorkers.add(moveWorker);
			organizationData.getMoveWorkersByOrganizationPositionData().addAll(moveWorkers);
			request.setMoveWorkersByOrganizationData(organizationData);
			return request;
		}
																																																																																																																																																																																																																																																					
		@Override
		public String checkAndCreateSupervisoryOrganizationId(Award award, Map<String, String> configDetails, WorkdayManpowerInterface workdayManpowerInterface) {
			logger.info("checkAndCreateSupervisoryOrganizationId : " + award.getAwardNumber());
			String errorMessage = "";
			String message = null;
			String awardNumber = award.getAwardNumber();
			try {
				String pIPersonId = manpowerIntegrationDao.getPIPersonIdByAwardId(award.getAwardId());
				workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				workdayManpowerInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
				AwardSupOrgMapping awardSupOrgMapping = new AwardSupOrgMapping();
				awardSupOrgMapping.setAwardId(workdayManpowerInterface.getAwardId());
				awardSupOrgMapping.setAwardNumber(awardNumber);
				awardSupOrgMapping.setpIPersonId(pIPersonId);
				String superiorSupOrgId = getSuperiorSupOrg(awardNumber, award.getAwardId(), award.getLeadUnitNumber(), awardSupOrgMapping, workdayManpowerInterface);
				logger.info("SuperiorSupOrgId {} ", superiorSupOrgId);
				if (superiorSupOrgId != null && !superiorSupOrgId.isEmpty()) {
					if (!manpowerIntegrationDao.checkIfAwardPISUpOrgMappingExist(awardNumber, pIPersonId, superiorSupOrgId)) {
						String pISupOrg = manpowerIntegrationDao.getPiSupOrgByPiPersonIdAndSuperior(pIPersonId, superiorSupOrgId);	
						if (pISupOrg == null || pISupOrg.isEmpty()) {
							pISupOrg = checkPISupOrgIdInWorkday(pIPersonId, superiorSupOrgId, awardNumber, configDetails, workdayManpowerInterface.getAwardId(), workdayManpowerInterface);
							if (pISupOrg != null && pISupOrg.isEmpty()) {
								String uniqueIIdValue = createSupervisoryOrganization(superiorSupOrgId, pIPersonId, awardNumber, configDetails, workdayManpowerInterface.getAwardId(), workdayManpowerInterface);			   
								if (!uniqueIIdValue.equals("")) {
									Boolean assignSuccess = assignManagerRoleToPI(pIPersonId, uniqueIIdValue, awardNumber, configDetails, workdayManpowerInterface.getAwardId(), workdayManpowerInterface);			
									if (Boolean.TRUE.equals(assignSuccess)) {
										pISupOrg = checkPISupOrgIdInWorkday(pIPersonId, superiorSupOrgId, awardNumber, configDetails, workdayManpowerInterface.getAwardId(), workdayManpowerInterface);
										if (pISupOrg != null && !pISupOrg.isEmpty()) {
											savePISupOrgMapping(pISupOrg, awardSupOrgMapping);
											saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG,
													MESSAGE_TYPE_SUCCESS,
													"PI SupOrg " + pISupOrg + "saved successfully for Award " + awardNumber,
													200,
													"SuperiorSupOrgId=" + superiorSupOrgId + " & PIPersonId=" + pIPersonId,
													null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
											workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
											message = MESSAGE_TYPE_SUCCESS;
										}
									}
								}
							} else {
								if (pISupOrg != null) {
									savePISupOrgMapping(pISupOrg, awardSupOrgMapping);
									saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG,
											MESSAGE_TYPE_SUCCESS,
											"PI SupOrg " + pISupOrg + "saved successfully for Award " + awardNumber, 200,
											"SuperiorSupOrgId=" + superiorSupOrgId + " & PIPersonId=" + pIPersonId, null,
											null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
									workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
									message = MESSAGE_TYPE_SUCCESS;
								}
							}
						} else {	 
							savePISupOrgMapping(pISupOrg, awardSupOrgMapping);
							saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_SUCCESS,
									"PI SupOrg " + pISupOrg + "saved successfully for Award " + awardNumber, 200,
									"SuperiorSupOrgId=" + superiorSupOrgId + " & PIPersonId=" + pIPersonId, null, null,
									workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
							workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
							message = MESSAGE_TYPE_SUCCESS;
						}
						logger.info("PISupOrgId {}", pISupOrg);
					} else {								   
						workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
						message = MESSAGE_TYPE_SUCCESS;
					}						   
				} 
			} catch (Exception e) {
				workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);
				logger.error("Error in checkAndCreateSupervisoryOrganizationId ", e.getMessage());
				errorMessage = new StringBuilder("Error in checkAndCreateSupervisoryOrganizationId ").append(e.getMessage()).toString();
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_FIBI_ERROR, errorMessage, 500, "AwardNumber=" + awardNumber, null, null, award.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
			manpowerIntegrationDao.saveOrUpdateManpowerInterface(workdayManpowerInterface);
			return message;
		}

	private void savePISupOrgMapping(String pISupOrg, AwardSupOrgMapping awardSupOrgMapping) {
		awardSupOrgMapping.setSupOrgId(pISupOrg);
		awardSupOrgMapping.setUpdateUser(UPDATE_USER);
		awardSupOrgMapping.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		manpowerIntegrationDao.saveOrUpdateAwardSupOrgMapping(awardSupOrgMapping);
	}

	private String checkPISupOrgIdInWorkday(String pIPersonId, String superiorSupOrgId, String awardNumber, Map<String, String> configDetails, Integer awardId, WorkdayManpowerInterface workdayManpowerInterface) {
		String pISupOrgId = "";
		String requestMessage = new StringBuilder("Superior_Org=").append(superiorSupOrgId).append("Manager_Id=") .append(pIPersonId).toString();
		String errorMessage = "";
		String apiUrl = configDetails.get(Constants.WORKDAY_API) + "PISupervisoryOrganization/?" + "Superior_Org=" + superiorSupOrgId + "&Manager_ID=" + pIPersonId + "&" + FORMAT_JSON;
		try {
			logger.info("Requesting for PISupOrgId {} ", apiUrl);
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			logger.info("Response Status {}", response.getStatusCode());
			logger.info(response.getBody());
			ObjectMapper objectMapper = new ObjectMapper();
			SupervisoryOrganization supervisoryOrganization = objectMapper.readValue(response.getBody(), SupervisoryOrganization.class);
			if (supervisoryOrganization != null && supervisoryOrganization.getSupOrgReportEntry() != null
					&& !supervisoryOrganization.getSupOrgReportEntry().isEmpty()) {
				pISupOrgId = supervisoryOrganization.getSupOrgReportEntry().get(0).getsUPORGID();
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CHECK_PI_SUP_ORG, MESSAGE_TYPE_SUCCESS, "PI Sup Org Id = " + pISupOrgId, response.getStatusCodeValue(), requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			} else {
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CHECK_PI_SUP_ORG, MESSAGE_TYPE_SUCCESS, "No PI Sup Org Id", response.getStatusCodeValue(), requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (RestClientResponseException e) {
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																 
			pISupOrgId = null;
			logger.info("RestClientError in checkPISupOrgIdInWorkday {}", e.getMessage());
			errorMessage = new StringBuilder("RestClientError in checkPISupOrgIdInWorkday : ").append(e.getStatusText())
					.append(e.getResponseBodyAsString() == null ? "" : e.getResponseBodyAsString()).toString();
			logger.info(errorMessage);
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CHECK_PI_SUP_ORG, MESSAGE_TYPE_ERROR,
					errorMessage, e.getRawStatusCode(), requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		} catch (Exception e) {
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																 
			pISupOrgId = null;
			logger.info("Error in checkPISupOrgIdInWorkday {}", e.getMessage());
			errorMessage = new StringBuilder("Error in checkPISupOrgIdInWorkday :").append(e.getMessage()).toString();
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CHECK_PI_SUP_ORG, MESSAGE_TYPE_ERROR,
					errorMessage, 500, requestMessage, null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
		return pISupOrgId;
	}

	private String getSuperiorSupOrg(String awardNumber, Integer awardId, String awardLeadUnit, AwardSupOrgMapping awardSupOrgMapping, WorkdayManpowerInterface workdayManpowerInterface) {
		logger.info("Finding SuperiorSupOrgId");
		String superiorSupOrgId = "";
		String errorMessage = "";
		String levelTwoSupOrgUnit = "";
		CustomDataElements customDataElement = customDataElementDao.fetchCustomDataElementDetail(Constants.CUSTOM_DATA_LEVEL_2_SUP_ORG);
		if (customDataElement != null) {
			CustomData customData = customDataElementDao.getCustomDataValue(Constants.MODULE_CODE_AWARD, awardId.toString(), customDataElement.getCustomElementId());
			if (customData != null) {
				levelTwoSupOrgUnit = customData.getValue() == null ? "" : customData.getValue();
				if (!levelTwoSupOrgUnit.equals("")) {
					superiorSupOrgId = getSuperiorSupOrgWithUnitNumber(awardSupOrgMapping, levelTwoSupOrgUnit, "Y");
					if (superiorSupOrgId == null || superiorSupOrgId.equals("")) {
						errorMessage = "No SuperiorSupervisoryOrganizationId found for Level 2 Sup Org of Award ";
					} else {
						awardSupOrgMapping.setSuperiorSupOrgId(superiorSupOrgId);
					}
				}
			}
		}
		if (levelTwoSupOrgUnit.equals("")) {
			superiorSupOrgId = getSuperiorSupOrgWithUnitNumber(awardSupOrgMapping, awardLeadUnit, "N");
			if (superiorSupOrgId == null || superiorSupOrgId.equals("")) {
				errorMessage = "No SuperiorSupervisoryOrganizationId found for lead unit of Award ";
			} else {
				awardSupOrgMapping.setSuperiorSupOrgId(superiorSupOrgId);
			}
		}
		if (!errorMessage.equals("")) {
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_ERROR);																																																 
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG, MESSAGE_TYPE_FIBI_ERROR, errorMessage, 500, "AwardNumber=" + awardNumber , null, null, awardId, workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
		return superiorSupOrgId;
	}

	private String getSuperiorSupOrgWithUnitNumber(AwardSupOrgMapping awardSupOrgMapping, String unitNumber, String isLevelTwoSupOrgUnit) {
		awardSupOrgMapping.setIsLevelTwoSupOrgUnit(isLevelTwoSupOrgUnit);
		awardSupOrgMapping.setUnitNumber(unitNumber);
		return manpowerIntegrationDao.getSuperiorSupervisoryOrganizationIdByUnitNumber(unitNumber);
	}

	@Override
	public void saveManpowerLog(String awardNumber, String interfaceTypeCode, String messageType, String message, int statusCode, String request, String errorXPath, String errorDetailMessage, Integer awardId, Integer workdayManpowerIntefaceId) {
		ManpowerLog manpowerLog = new ManpowerLog();
		manpowerLog.setAwardNumber(awardNumber);
		manpowerLog.setInterfaceTypeCode(interfaceTypeCode);
		manpowerLog.setMessageType(messageType);
		manpowerLog.setMessage(message);
		manpowerLog.setStatusCode(statusCode);
		manpowerLog.setRequestParam(request);
		manpowerLog.setIsMailSent("N");
		manpowerLog.setUpdateUser(UPDATE_USER);
		manpowerLog.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		manpowerLog.setErrorXPath(errorXPath);
		manpowerLog.setErrorDetailmessage(errorDetailMessage);
		manpowerLog.setAwardId(awardId);
		manpowerLog.setWorkdayManpowerIntefaceId(workdayManpowerIntefaceId);
		manpowerIntegrationDao.saveOrUpdateManpowerLog(manpowerLog);
	}

	@Override
	public void createPosition(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, String superiorSupOrg, String piPersonId, Integer activeAwardId) {
		List<AwardSupOrgMapping> supOrgMappings = new ArrayList<>();
		AwardSupOrgMapping supOrgMapping = null;
		if (superiorSupOrg != null && !superiorSupOrg.equals("")) {
			supOrgMappings = manpowerIntegrationDao.getAwardUpOrgMappingByAwardNumberAndPIPersonId(workdayManpowerInterface.getAwardNumber(), piPersonId, superiorSupOrg);
			if (supOrgMappings != null && !supOrgMappings.isEmpty()) {
				supOrgMapping = supOrgMappings.get(0);
			}
		}
		String positionId = "";
		String requestMessage = new StringBuilder("AwardNumber=").append(workdayManpowerInterface.getAwardNumber()).toString();
		String errorMessage = "";
		try {
			if (supOrgMapping != null) {
				if (workdayManpowerInterface.getAwardManpowerResource().getCostAllocation().equals(new BigDecimal("100.00"))) {
					AwardManpowerResource resource = workdayManpowerInterface.getAwardManpowerResource();
					positionId = createWorkdayPosition(resource, supOrgMapping.getSupOrgId(), workdayConfigDetails, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
					if (!positionId.equals("")) {
						List<AwardManpowerResource> awardManpowerResources = manpowerIntegrationDao.getAwardManpowerResourcesByResourceUniqueId(resource.getResourceUniqueId());
						if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
							for (AwardManpowerResource awardManpowerResource : awardManpowerResources) {
								awardManpowerResource.setPositionId(positionId);
								awardManpowerResource.setPositionStatusCode(Constants.MANPOWER_POSITION_GENERATED);
								awardManpowerResource.setUpdateUser(UPDATE_USER);
								awardManpowerResource.setUpdateTimestamp(commonDao.getCurrentTimestamp());
								awardManpowerResource.setPositionTriggerDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE));
								manpowerIntegrationDao.saveOrUpdateAwardManpowerResource(awardManpowerResource);
							}
						}
						saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
					} else {
						saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
					}

				} else {
					logger.info("Cost allocation less than 100% for new hire");
					// send mail to PI
					WorkdayHRBusinessPartner hrbp = getHRBPDetails(supOrgMapping.getSupOrgId(),getWorkdayConfigDetails(), workdayManpowerInterface.getAwardNumber(), workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
					if (hrbp != null && hrbp.getReportEntries() != null && !hrbp.getReportEntries().isEmpty()) {
						sendNotificationForLessCostAllocation(workdayManpowerInterface.getAwardManpowerResource(), hrbp.getReportEntries(), activeAwardId);
						saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
					} else {
						saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
					}
				}
			} else {
				saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
				saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_INTERFACE_PENDING, "No supOrg Id", 500, requestMessage , null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
			
		} catch (Exception e) {
			logger.error("Error in createPosition : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in createPosition ").append(e.getMessage()).toString();
			saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_POSITION_CREATION, MESSAGE_TYPE_FIBI_ERROR, errorMessage, 500, requestMessage , null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
	}

	@Override
	public void saveWorkdayManpowerInterfaceWithInterfaceStatus(WorkdayManpowerInterface workdayManpowerInterface, String interfaceStatusCode) {
		workdayManpowerInterface.setInterfaceStatusCode(interfaceStatusCode);
		workdayManpowerInterface.setUpdateUser(UPDATE_USER);
		workdayManpowerInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
		manpowerIntegrationDao.saveOrUpdateWorkdayManpowerInterface(workdayManpowerInterface);
	}

	@Override
	public String getCostingAllocationReconciliation(Entry<String, List<AwardManpowerResource>> awardManpowerResources, Map<String, String> configDetails, AtomicBoolean success, AtomicBoolean isWorkday, AtomicBoolean isRaise, StringBuilder mailContent, AtomicInteger rowCount, Set<String> uniqueId, Set<String> successUniqueId, List<AwardManpowerResource> costAllocationSuccessResources) throws ParseException {
		String requestMessage = new StringBuilder("EmployeeId=").append(awardManpowerResources.getKey()).append("Effective_as_of_Date").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).append(" Costing_Allocation_End_Date_from=").append(getOneMonthBeforeDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
		String errorMessage = "";
		String apiUrl = new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("CostingAllocation/?").append(FORMAT_JSON).append("&Effective_as_of_Date=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE)))
		.append("&Costing_Allocation_End_Date_from=").append(getOneMonthBeforeDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).append("&Employee_ID=").append(awardManpowerResources.getKey()).toString();
		try {
			logger.info("Requesting for getCostingAllocationReconciliation {}", apiUrl);
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			logger.info("Response Status {}", response.getStatusCode());
//			AtomicInteger chargeSuccessCounter = new AtomicInteger(0);
			AtomicInteger riseCounter = new AtomicInteger(0);
			AtomicInteger workdayCounter = new AtomicInteger(0);
			ObjectMapper objectMapper = new ObjectMapper();
			CostAllocationReconciliation reconciliation = objectMapper.readValue(response.getBody(), CostAllocationReconciliation.class);
			StringBuilder successMailContent = new StringBuilder();
			StringBuilder riseMailContent = new StringBuilder();
			StringBuilder workdayMailContent = new StringBuilder();
			String tableHeader = new StringBuilder("<figure class=\"table\"><table border=\"1px solid black\"><tbody>").toString();
			String tableHeaderContent = new StringBuilder("<tr>")
					.append("<td><strong>WBS Number</strong></td><td><strong>Person Id</td></strong>")
					.append("<td><strong>Cost Allocation Start Date</strong></td><td><strong>Cost Allocation End Date</strong></td>")
					.append("<td><strong>Percentage</strong></td></tr>").toString();
			awardManpowerResources.getValue().stream().forEach(awardManpowerResource ->{
				AwardManpower awardManpower = manpowerIntegrationDao.getAwardManpowerById(awardManpowerResource.getAwardManpowerId());				
				if (reconciliation != null && (reconciliation.getReportEntry() != null && !reconciliation.getReportEntry().isEmpty())) {										
					reconciliation.getReportEntry().stream().forEach(repoertEntry -> {
						repoertEntry.getAllocationDetailsGroup().stream().forEach(allocationDetail -> {	
							Timestamp startDate = awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate();
							Timestamp endDate = awardManpowerResource.getChargeEndDate() == null ? awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate();
								if (checkForCostAllocationDate(endDate, allocationDetail.getCOSTALLOCENDDATE()) 
										&& checkForCostAllocationDate(startDate, allocationDetail.getCOSTALLOCSTARTDATE()) 
										&& checkforIOCode(awardManpower.getBudgetReferenceNumber(), allocationDetail.getCOSTALLOCPROJECTID())
										&& checkForCostAllocationPercentage(awardManpowerResource.getCostAllocation(), allocationDetail.getCOSTALLOCPERCENT())) {
									List<String> awardStatuses = new ArrayList<>();
									awardStatuses.add(Constants.AWARD_FINAL_STATUS_ACTIVE);
									awardStatuses.add(Constants.AWARD_FINAL_STATUS_PENDING);
									AwardManpowerBaseSalaryHistory baseSalaryHistory = new AwardManpowerBaseSalaryHistory();
									List<AwardManpowerResource> resources = manpowerIntegrationDao.getAwardManpowerResourcesResourceUniqueIdAndAwardSequenceStatues(awardManpowerResource.getResourceUniqueId(), awardStatuses);
									for (AwardManpowerResource resource : resources) {
										Timestamp allocationStartDate = awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate();
										Timestamp allocationEndDate = awardManpowerResource.getChargeEndDate() == null ?  awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate();
										resource.setCostAllocation(awardManpowerResource.getCostAllocation());
										resource.setChargeStartDate(allocationStartDate);
										resource.setPreviousChargeStartDate(allocationStartDate);
										resource.setChargeEndDate(allocationEndDate);
										resource.setPreviousChargeEndDate(allocationEndDate);
										resource.setChargeDuration(awardManpowerResource.getChargeDuration() == null ? awardManpowerResource.getPlanDuration() : awardManpowerResource.getChargeDuration());
										calculateActualCommittedCost(resource, awardManpower, baseSalaryHistory, costAllocationSuccessResources, awardManpower.getAwardId());	
									}
									updateLatestInterfaceQueueStatus(awardManpowerResource);
									saveBaseSalaryHistory(baseSalaryHistory, awardManpowerResource, startDate, endDate);
									success.compareAndSet(false, true);
									rowCount.getAndIncrement();
//									try {
//										sendMailForSucessReconcilation(awardManpowerResource,allocationDetail,awardManpower,successMailContent,chargeSuccessCounter,repoertEntry, successUniqueId);
//									} catch (Exception e) {
//										e.printStackTrace();
//									}
									logger.info("Match Found with Workday Details(employee id : {}))", awardManpowerResource.getPersonId());
									RiseErrorAllocations errorAllocation = manpowerIntegrationDao.getRiseErrorAllocationByResourceUniqueId(awardManpowerResource.getResourceUniqueId());
									if (errorAllocation != null) {
										manpowerIntegrationDao.deleteRiseErrorAllocation(errorAllocation);
									}
								}
						});
						if (!success.get()) {
							logger.info("No match found with work day details (employee id : {}))", awardManpowerResource.getPersonId());
							sendMailForFailedReconciliationWithTable(repoertEntry, awardManpowerResource, awardManpower,riseMailContent, workdayMailContent, riseCounter, workdayCounter, isWorkday, isRaise, rowCount, uniqueId);
						}
					});
					saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_COST_RECONCILATION, MESSAGE_TYPE_SUCCESS, "Cost Allocation Details for Reconciliation Received", 200, requestMessage, null, null, awardManpower.getAwardId(), null);
				} else {
					saveManpowerLog(awardManpowerResource.getAwardNumber(), Constants.MANPOWER_INTERFACE_COST_RECONCILATION, MESSAGE_TYPE_SUCCESS, "No Cost Allocation Details for Reconciliation Received", 200, requestMessage, null, null, awardManpower.getAwardId(), null);
					sendMailForFailedReconciliationWithTable(new CostReportEntry(), awardManpowerResource, awardManpower, riseMailContent, workdayMailContent, riseCounter, workdayCounter, isWorkday, isRaise, rowCount, uniqueId);
				}
			});		
			if(success.get() && successMailContent.length() != 0) {
				mailContent.append("\n<b> Cost Allocation where RISE and WORKDAY data successfully reconciles: </b><br>");				
				mailContent.append(tableHeader);
				mailContent.append(successMailContent);
				mailContent.append("</tbody></table></figure><br>");
			}if(isRaise.get() && riseMailContent.length() != 0) {
				mailContent.append("\n<b> Cost Allocation where discrepancy found in the data between RISE and WORKDAY: </b><br>");
				mailContent.append("\n<span style=\"padding-left:40px\"><b>RISE Charging Periods:</b>");
				mailContent.append(tableHeader).append(tableHeaderContent);
				mailContent.append(riseMailContent);
				mailContent.append("</tbody></table></figure><br>");
			}if(isWorkday.get() && workdayMailContent.length() != 0) {
				mailContent.append("\n<span style=\"padding-left:40px\"><b>Workday Charging Periods:</b>");
				mailContent.append(tableHeader).append(tableHeaderContent);
				mailContent.append(workdayMailContent);
				mailContent.append("</tbody></table></figure><br>");
			}
			return mailContent.toString();
		}catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getCostingAllocationReconciliation : ").append(ex.getStatusText()).append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_COST_RECONCILATION, MESSAGE_TYPE_ERROR, errorMessage, ex.getRawStatusCode(), requestMessage, null, null, null, null);
		} catch (Exception e) {
			logger.error("Error in getCostingAllocationReconciliation : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getCostingAllocationReconciliation ").append(e.getMessage()).toString();
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_COST_RECONCILATION, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, null, null);
		}
		return null;
	}

	private String getOneMonthBeforeDate(Timestamp currentDateBasedOnTimeZone) {
		Calendar c1 = Calendar.getInstance();
		c1.setTime(currentDateBasedOnTimeZone);
		c1.add(Calendar.MONTH, -1);
		Date updatedDate = c1.getTime();
		DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
		return formatter.format(updatedDate);
	}

	@Override
	public void saveBaseSalaryHistory(AwardManpowerBaseSalaryHistory baseSalaryHistory, AwardManpowerResource awardManpowerResource, Timestamp chargeStartDate, Timestamp chargeEndDate) {
		if (baseSalaryHistory.getCurrentBaseSalary() != null) {
			baseSalaryHistory.setResourceUniqueId(awardManpowerResource.getResourceUniqueId());
			baseSalaryHistory.setAwardNumber(awardManpowerResource.getAwardNumber());
			baseSalaryHistory.setChargeStartDate(chargeStartDate);
			baseSalaryHistory.setChargeEndDate(chargeEndDate);
			baseSalaryHistory.setPersonId(awardManpowerResource.getPersonId());
			baseSalaryHistory.setPositionId(awardManpowerResource.getPositionId());
			baseSalaryHistory.setPreviousBaseSalary(getPreviousBaseSalaryFromHistory(awardManpowerResource.getPersonId()));
			baseSalaryHistory.setUpdateUser(UPDATE_USER);
			baseSalaryHistory.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			manpowerIntegrationDao.saveOrUpdateAwardManpowerBaseSalaryHistory(baseSalaryHistory);
		}
	}

	private String getPreviousBaseSalaryFromHistory(String personId) {
		return manpowerIntegrationDao.getPreviousBaseSalaryFromHistory(personId) == null ? null : manpowerIntegrationDao.getPreviousBaseSalaryFromHistory(personId).getCurrentBaseSalary();
	}

	private void updateLatestInterfaceQueueStatus(AwardManpowerResource awardManpowerResource) {
		List<String> costAllocationInterfaces = new ArrayList<>();
		costAllocationInterfaces.add(Constants.MANPOWER_INTERFACE_COST_ALLOCATION);
		costAllocationInterfaces.add(Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER);
		WorkdayManpowerInterface latestResourceAllocationInterface = manpowerIntegrationDao.getLatestResourceInterfaceByParams(awardManpowerResource.getResourceUniqueId(), costAllocationInterfaces);
		if (latestResourceAllocationInterface != null) {
			latestResourceAllocationInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
			latestResourceAllocationInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
			manpowerDao.saveOrUpdateWorkdayManpowerInterface(latestResourceAllocationInterface);
		}
	}

	private void sendMailForFailedReconciliationWithTable(CostReportEntry reportEntry, AwardManpowerResource awardManpowerResource, AwardManpower awardManpower, StringBuilder riseMailContent,
			StringBuilder workdayMailContent, AtomicInteger riseCounter, AtomicInteger workdayCounter, AtomicBoolean isWorkday, AtomicBoolean isRaise, AtomicInteger rowCount, Set<String> uniqueId) {
		RiseErrorAllocations riseErrorAllocation = manpowerIntegrationDao.getRiseErrorAllocationByResourceUniqueId(awardManpowerResource.getResourceUniqueId());
		if (riseErrorAllocation == null || 
				(!awardManpowerResource.getCostAllocation().equals(riseErrorAllocation.getCostAllocation()) || 
				 !awardManpower.getBudgetReferenceNumber().equals(riseErrorAllocation.getBudgetReferenceNumber()) ||
				 !riseErrorAllocation.getAllocationStartDate().equals(awardManpowerResource.getChargeStartDate() == null ? 
						awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate()) || 
				 !riseErrorAllocation.getAllocationEndDate().equals(awardManpowerResource.getChargeEndDate() == null ? 
						awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate()))) {
			if (riseErrorAllocation == null)
				riseErrorAllocation = new RiseErrorAllocations();
			riseErrorAllocation.setResourceUniqueId(awardManpowerResource.getResourceUniqueId());
			riseErrorAllocation.setPersonId(awardManpowerResource.getPersonId());
			riseErrorAllocation.setPositionId(awardManpowerResource.getPositionId());
			riseErrorAllocation.setBudgetReferenceNumber(awardManpower.getBudgetReferenceNumber());
			riseErrorAllocation.setCostAllocation(awardManpowerResource.getCostAllocation());
			riseErrorAllocation.setAllocationStartDate(awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate());
			riseErrorAllocation.setAllocationEndDate(awardManpowerResource.getChargeEndDate() == null ? awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate());
			riseErrorAllocation.setAwardNumber(awardManpowerResource.getAwardNumber());
			riseErrorAllocation.setUpdateUser(UPDATE_USER);
			riseErrorAllocation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			manpowerIntegrationDao.saveRiseErrorAllocation(riseErrorAllocation);
			if (!uniqueId.contains(awardManpowerResource.getResourceUniqueId())) {
				uniqueId.add(awardManpowerResource.getResourceUniqueId());
				isRaise.set(true);
				riseCounter.getAndIncrement();
				rowCount.getAndIncrement();
				Timestamp allocationStartDate = awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate();
				Timestamp allocationEndDate = awardManpowerResource.getChargeEndDate() == null ?  awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate();
				String tableContent = new StringBuilder("<td>")
						.append(awardManpower.getBudgetReferenceNumber() != null ? awardManpower.getBudgetReferenceNumber() : "").append("</td><td style=\"text-align:center\">")
						.append(awardManpowerResource.getPersonId() != null ? awardManpowerResource.getPersonId() : "").append("</td><td style=\"text-align:center\">")
						.append(allocationStartDate != null ? commonService.convertDateFormatBasedOnTimeZone(allocationStartDate.getTime(), Constants.DEFAULT_DATE_FORMAT) : "").append("</td><td style=\"text-align:center\">")
						.append(allocationEndDate != null ? commonService.convertDateFormatBasedOnTimeZone(allocationEndDate.getTime(), Constants.DEFAULT_DATE_FORMAT) : "").append("</td><td style=\"text-align:center\">")
						.append(awardManpowerResource.getCostAllocation() != null ? awardManpowerResource.getCostAllocation() : "").append("</td>").toString();
				String failedTable = new StringBuilder("<tr>").append(tableContent).append("</tr>").toString();
				riseMailContent.append(failedTable);
				if (reportEntry.getAllocationDetailsGroup() != null) {
					reportEntry.getAllocationDetailsGroup().stream().forEach(allocationDetail -> {
						StringBuilder workdayUniqueId = new StringBuilder();
						String WorkdayUniqueString = null;
						try {
							WorkdayUniqueString = appendMailContentUniqueId(allocationDetail,reportEntry, workdayUniqueId);						
						} catch (ParseException e1) {
							e1.printStackTrace();
						}
						if (!uniqueId.contains(WorkdayUniqueString)) {
							uniqueId.add(WorkdayUniqueString);
							isWorkday.set(true);
							workdayCounter.getAndIncrement();
							rowCount.getAndIncrement();
							DateFormat toDate = new SimpleDateFormat("dd/MM/yyyy"); // wanted format
							DateFormat fromDate = new SimpleDateFormat("yyyy-MM-dd"); // current format
							String workdayTableContent = null;
							try {
								workdayTableContent = new StringBuilder("<td>")
										.append(allocationDetail.getCOSTALLOCPROJECTID() != null ? allocationDetail.getCOSTALLOCPROJECTID() : "").append("</td><td style=\"text-align:center\">")
										.append(reportEntry.getEMPLOYEEID() != null ? reportEntry.getEMPLOYEEID() : "").append("</td><td style=\"text-align:center\">")
										.append(allocationDetail.getCOSTALLOCSTARTDATE() != null ? toDate.format(fromDate.parse(allocationDetail.getCOSTALLOCSTARTDATE())) : "").append("</td><td style=\"text-align:center\">")
										.append(allocationDetail.getCOSTALLOCENDDATE() != null ? toDate.format(fromDate.parse(allocationDetail.getCOSTALLOCENDDATE())) : "").append("</td><td style=\"text-align:center\">")
										.append(allocationDetail.getCOSTALLOCPERCENT() != null ? new BigDecimal(allocationDetail.getCOSTALLOCPERCENT()).multiply(new BigDecimal(100)).setScale(2, RoundingMode.HALF_UP) : "").append("</td>").toString();
							} catch (NumberFormatException | ParseException e) {
								e.printStackTrace();
							}
							String workdayFailedTable = new StringBuilder("<tr>").append(workdayTableContent).append("</tr>").toString();
							workdayMailContent.append(workdayFailedTable);
						}
					});
				}
			}
		}
	}

	private String appendMailContentUniqueId(CostAllocationDetailsGroup allocationDetail, CostReportEntry reportEntry, StringBuilder workdayUniqueId) throws ParseException {								 
		DateFormat toDate = new SimpleDateFormat("dd/MM/yyyy"); // wanted format
		DateFormat fromDate = new SimpleDateFormat("yyyy-MM-dd"); // current format
		workdayUniqueId.append(allocationDetail.getCOSTALLOCPROJECTID() != null ? allocationDetail.getCOSTALLOCPROJECTID() : "").append("_")
		.append(reportEntry.getEMPLOYEEID() != null ? reportEntry.getEMPLOYEEID() : "").append("_")																																																				
		.append(allocationDetail.getCOSTALLOCSTARTDATE() != null ? toDate.format(fromDate.parse(allocationDetail.getCOSTALLOCSTARTDATE())) : "").append("_")
		.append(allocationDetail.getCOSTALLOCENDDATE() != null ? toDate.format(fromDate.parse(allocationDetail.getCOSTALLOCENDDATE())) : "").append("_")
		.append(allocationDetail.getCOSTALLOCPERCENT() != null ? new BigDecimal(allocationDetail.getCOSTALLOCPERCENT()).multiply(new BigDecimal(100)).setScale(2, RoundingMode.HALF_UP) : "").toString();
		return workdayUniqueId.toString();
	}

//	private void sendMailForSucessReconcilation(AwardManpowerResource awardManpowerResource, CostAllocationDetailsGroup allocationDetail, AwardManpower awardManpower, StringBuilder successMailContent,
//		AtomicInteger chargeSuccessCounter, CostReportEntry reportEntry, Set<String> successUniqueId) throws Exception {
//		if (!successUniqueId.contains(awardManpowerResource.getResourceUniqueId())) {
//			successUniqueId.add(awardManpowerResource.getResourceUniqueId());
//		chargeSuccessCounter.getAndIncrement();
//		DateFormat toDate   = new SimpleDateFormat("dd/MM/yyyy"); // wanted format
//		DateFormat fromDate = new SimpleDateFormat("yyyy-MM-dd"); // current format
//		String tableContent = "";
//		Timestamp allocationStartDate = awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate();
//		Timestamp allocationEndDate = awardManpowerResource.getChargeEndDate() == null ?  awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate();
//		String tableHeader = new StringBuilder("<tr><td colspan = \"3\" style=\"text-align:center\"><strong>Charging Period ").append(chargeSuccessCounter).append("</strong></td></tr>").append("<tr><td><strong>Fields</strong></td><td><strong>RISE</strong></td><td><strong>WORKDAY</strong></td></tr>")
//				.append("<tr><td>WBS Number</td><td>").append(awardManpower.getBudgetReferenceNumber()).append("</td><td>").append(allocationDetail.getCOSTALLOCPROJECTID()).append("</td></tr>")
//				.append("<tr><td>Person Id</td><td>").append(awardManpowerResource.getPersonId()).append("</td><td>").append(reportEntry.getEMPLOYEEID()).append("</td></tr>")
//				.append("<tr><td>Cost Allocation Start Date</td><td>").append(commonService.convertDateFormatBasedOnTimeZone(allocationStartDate.getTime(),Constants.DEFAULT_DATE_FORMAT)).append("</td><td>").append(toDate.format(fromDate.parse(allocationDetail.getCOSTALLOCSTARTDATE()))).append("</td></tr>")
//				.append("<tr><td>Cost Allocation End Date</td><td>").append(commonService.convertDateFormatBasedOnTimeZone(allocationEndDate.getTime(),Constants.DEFAULT_DATE_FORMAT)).append("</td><td>").append(toDate.format(fromDate.parse(allocationDetail.getCOSTALLOCENDDATE()))).append("</td></tr>")
//				.append("<tr><td>Percentage</td><td>").append(awardManpowerResource.getCostAllocation()).append("</td><td>").append(new BigDecimal(allocationDetail.getCOSTALLOCPERCENT()).multiply(new BigDecimal(100)).setScale(2, RoundingMode.HALF_UP)).append("</td></tr>")
//				.toString();
//		String table =  new StringBuilder(tableHeader).append(tableContent).toString();
//		successMailContent.append(table);
//		}
//	}

	private boolean checkforIOCode(String budgetReferenceNumber, String costallocprojectId) {
		return budgetReferenceNumber != null && costallocprojectId != null && costallocprojectId.equals(budgetReferenceNumber);
	}

	private boolean checkForCostAllocationDate(Timestamp chargeEndDate, String costallocenddate) {
		try {
			Date endDate = new SimpleDateFormat("yyyy-MM-dd").parse(costallocenddate);
		   return chargeEndDate != null &&  costallocenddate != null && chargeEndDate.equals(new Timestamp(endDate.getTime()));
		} catch (Exception e) {
			return false;
		}
	}

	private boolean checkForCostAllocationPercentage(BigDecimal costAllocation, String costallocpercent) {
		BigDecimal workdayCostAllocation = new BigDecimal(costallocpercent).multiply(new BigDecimal(100)).setScale(2, RoundingMode.HALF_UP);
		return costallocpercent != null && costAllocation != null && workdayCostAllocation.equals(costAllocation);
	}

	@Override
	public void freezePosition(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, Award award, StringBuilder freezeErrorMailContent) {
		try {
			AwardManpowerResource resource = manpowerIntegrationDao.getAwardManpowerResourceById(workdayManpowerInterface.getAwardManpowerResourceId());
			if (resource.getPositionId() != null) {
				if (workdayManpowerInterface.getNewAwardEndDate() == null) {
					workdayManpowerInterface.setNewAwardEndDate(award.getFinalExpirationDate());
				}
				if (Boolean.TRUE.equals(freezeWorkdayPosition(resource.getPositionId(), workdayManpowerInterface.getNewAwardEndDate(), workdayManpowerInterface.getAwardNumber(), workdayConfigDetails, freezeErrorMailContent, workdayManpowerInterface.getOldFreezeDate(), workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId()))) {
					List<AwardManpowerResource> awardManpowerResources = manpowerIntegrationDao.getAwardManpowerResourcesByResourceUniqueId(resource.getResourceUniqueId());
					if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
						for (AwardManpowerResource awardManpowerResource : awardManpowerResources) {
							awardManpowerResource.setFreezeDate(workdayManpowerInterface.getNewAwardEndDate());
							awardManpowerResource.setUpdateUser(UPDATE_USER);
							awardManpowerResource.setUpdateTimestamp(commonDao.getCurrentTimestamp());
							manpowerIntegrationDao.saveOrUpdateAwardManpowerResource(awardManpowerResource);
						}
					}
					saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
				} else {
					saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
				}
			} else {
				String requestMessage = new StringBuilder("AwardNumber=").append(workdayManpowerInterface.getAwardNumber()).toString();
				saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
				saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_INTERFACE_PENDING, "No Position Id", 500, requestMessage , null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (Exception e) {
			logger.error("Exception in freezePosition : {}" + e.getMessage());
			saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION, MESSAGE_TYPE_FIBI_ERROR, "Error in freezePosition " + e.getMessage(), 500, "AwardNumber=" + workdayManpowerInterface.getAwardNumber() , null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
	}

	@Override
	public void getWorkdayJobProfile(AtomicInteger rowCount) {
		Map<String, String> configDetails = getWorkdayConfigDetails();
		String apiUrl = configDetails.get(Constants.WORKDAY_API) + "JobProfile/";
		String errorMessage = "";
		String successMessage = "";
		try {
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			logger.info("Response Status {}", response.getStatusCode());
			ObjectMapper objectMapper = new ObjectMapper();
			WorkdayJobProfile jobProfile = objectMapper.readValue(response.getBody(), WorkdayJobProfile.class);
			if (jobProfile != null && (jobProfile.getJobProfileReportEntries() != null && !jobProfile.getJobProfileReportEntries().isEmpty())) {
				StringBuilder newMailBodyFinal = new StringBuilder();
				StringBuilder updatedMailBodyFinal = new StringBuilder();
				StringBuilder finalEmail = new StringBuilder();
				boolean isUpdated = false;
				boolean isNew = false;
				for (JobProfileReportEntry jobProfileEntry : jobProfile.getJobProfileReportEntries()) {
					isUpdated = false;
					isNew = false;
					StringBuilder updatedEmailBody = new StringBuilder("");
					StringBuilder newMailBody = new StringBuilder("");
					ManpowerJobProfileType manpowerProfile = new ManpowerJobProfileType();
					ManpowerJobProfileType manpowerJobProfileType = new ManpowerJobProfileType();
					if (jobProfileEntry.getJobProfileId() != null) {
						manpowerJobProfileType = manpowerDao.getManpowerJobProfileTypeById(jobProfileEntry.getJobProfileId());
						if (manpowerJobProfileType != null) {
							updatedEmailBody.append("Updated Job Profile Id: ").append(jobProfileEntry.getJobProfileId()).append("<br>");
						} else if (manpowerJobProfileType == null) {
							manpowerProfile.setJobProfileTypeCode(jobProfileEntry.getJobProfileId());
							isNew = true;
							newMailBody.append("New Job Profile Id: ").append(jobProfileEntry.getJobProfileId()).append("<br>");
						}
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getEffectiveDate() != null && jobProfileEntry.getEffectiveDate() != null) {
						Date effectiveDate = new SimpleDateFormat("yyyy-MM-dd").parse(jobProfileEntry.getEffectiveDate());
						Date dbEffectiveDate = new SimpleDateFormat("yyyy-MM-dd").parse(manpowerJobProfileType.getEffectiveDate().toString());
						if (!dbEffectiveDate.equals(effectiveDate)) {
							manpowerJobProfileType.setEffectiveDate(effectiveDate);
							isUpdated = true;
							updatedEmailBody.append("Effective Date:").append(jobProfileEntry.getEffectiveDate()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getEffectiveDate() == null && manpowerJobProfileType.getEffectiveDate() != null) {
						manpowerJobProfileType.setEffectiveDate(null);
						isUpdated = true;
						updatedEmailBody.append("Effective Date:").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getEffectiveDate() != null && manpowerJobProfileType.getEffectiveDate() == null) {
						Date effectiveDate = new SimpleDateFormat("yyyy-MM-dd").parse(jobProfileEntry.getEffectiveDate());
						manpowerJobProfileType.setEffectiveDate(effectiveDate);
						isUpdated = true;
						updatedEmailBody.append("Effective Date:").append(jobProfileEntry.getEffectiveDate()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getEffectiveDate() != null) {
						Date effectiveDate = new SimpleDateFormat("yyyy-MM-dd").parse(jobProfileEntry.getEffectiveDate());
						manpowerProfile.setEffectiveDate(effectiveDate);
						isNew = true;
						newMailBody.append("Effective Date:").append(jobProfileEntry.getEffectiveDate()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getPublicJob() != null && jobProfileEntry.getPublicJob() != null) {
						if (!manpowerJobProfileType.getPublicJob().equals(jobProfileEntry.getPublicJob())) {
							manpowerJobProfileType.setPublicJob(jobProfileEntry.getPublicJob());
							isUpdated = true;
							updatedEmailBody.append("Public Job:").append(jobProfileEntry.getPublicJob()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getPublicJob() == null && manpowerJobProfileType.getPublicJob() != null) {
						manpowerJobProfileType.setPublicJob(null);
						isUpdated = true;
						updatedEmailBody.append("Public Job:").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getPublicJob() != null && manpowerJobProfileType.getPublicJob() == null) {
						manpowerJobProfileType.setPublicJob(jobProfileEntry.getPublicJob());
						isUpdated = true;
						updatedEmailBody.append("Public Job:").append(jobProfileEntry.getPublicJob()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getPublicJob() != null) {
						manpowerProfile.setPublicJob(jobProfileEntry.getPublicJob());
						isNew = true;
						newMailBody.append("Public Job:").append(jobProfileEntry.getPublicJob()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getIsActive() != null && jobProfileEntry.getInactive() != null) {
						if (!manpowerJobProfileType.getIsActive().equals("Y") && jobProfileEntry.getInactive().equals("0")) {
							manpowerJobProfileType.setIsWorkdayActive("Y");
							manpowerJobProfileType.setIsActive("Y");
							isUpdated = true;
							updatedEmailBody.append("InActive:").append("0").append("<br>");
						} else if (!manpowerJobProfileType.getIsActive().equals("N") && jobProfileEntry.getInactive().equals("1")) {
							manpowerJobProfileType.setIsWorkdayActive("N");
							manpowerJobProfileType.setIsActive("N");
							isUpdated = true;
							updatedEmailBody.append("InActive:").append("1").append("<br>");
						}
					} else if (manpowerJobProfileType == null && jobProfileEntry.getInactive() != null) {
						if (jobProfileEntry.getInactive().equals("0")) {
							manpowerProfile.setIsWorkdayActive("Y");
							manpowerProfile.setIsActive("Y");
							isNew = true;
							newMailBody.append("InActive:").append("0").append("<br>");
						} else if (jobProfileEntry.getInactive().equals("1")) {
							manpowerProfile.setIsWorkdayActive("N");
							manpowerProfile.setIsActive("N");
							isNew = true;
							newMailBody.append("InActive:").append("1").append("<br>");
						}
					}
					String jobFamily = "";
					if (jobProfileEntry.getJobFamilyGroups() != null && jobProfileEntry.getJobFamilyGroups().get(0).getJobFamily() != null) {
						jobFamily = jobProfileEntry.getJobFamilyGroups().get(0).getJobFamily();
						if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamily() != null) {
							if (!manpowerJobProfileType.getJobFamily().equals(jobFamily)) {
								manpowerJobProfileType.setJobFamily(jobFamily);
								isUpdated = true;
								updatedEmailBody.append("Job Family:").append(jobFamily).append("<br>");
							}
						} else if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamily() == null) {
							manpowerJobProfileType.setJobFamily(jobFamily);
							isUpdated = true;
							updatedEmailBody.append("Job Family:").append(jobFamily).append("<br>");
						} else if (manpowerJobProfileType == null) {
							manpowerProfile.setJobFamily(jobFamily);
							isNew = true;
							newMailBody.append("Job Family:").append(jobFamily).append("<br>");
						}
					} else if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamily() != null) {
						manpowerJobProfileType.setJobFamily(null);
						isUpdated = true;
						updatedEmailBody.append("Job Family:").append("").append("<br>");
					}				
					String jobFamilyId = "";
					if (jobProfileEntry.getJobFamilyGroups() != null && jobProfileEntry.getJobFamilyGroups().get(0).getJobFamilyId() != null) {
						jobFamilyId = jobProfileEntry.getJobFamilyGroups().get(0).getJobFamilyId();
						if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamilyId() != null) {
							if (!manpowerJobProfileType.getJobFamilyId().equals(jobFamilyId)) {
								manpowerJobProfileType.setJobFamilyId(jobFamilyId);
								isUpdated = true;
								updatedEmailBody.append("Job Family Id:").append(jobFamilyId).append("<br>");
							}
						} else if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamilyId() == null) {
							manpowerJobProfileType.setJobFamilyId(jobFamilyId);
							isUpdated = true;
							updatedEmailBody.append("Job Family Id:").append(jobFamilyId).append("<br>");
						} else if (manpowerJobProfileType == null) {
							manpowerProfile.setJobFamilyId(jobFamilyId);
							isNew = true;
							newMailBody.append("Job Family Id:").append(jobFamilyId).append("<br>");
						}
					} else if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamilyId() != null) {
						manpowerJobProfileType.setJobFamilyId(null);
						isUpdated = true;
						updatedEmailBody.append("Job Family Id:").append("").append("<br>");
					}
					String jobFamilyGroup = "";
					if (jobProfileEntry.getJobFamilyGroups() != null && jobProfileEntry.getJobFamilyGroups().get(0).getJobFamilyGroup() != null) {
						jobFamilyGroup = jobProfileEntry.getJobFamilyGroups().get(0).getJobFamilyGroup();
						if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamilyGroup() != null) {
							if (!manpowerJobProfileType.getJobFamilyGroup().equals(jobFamilyGroup)) {
								manpowerJobProfileType.setJobFamilyGroup(jobFamilyGroup);
								isUpdated = true;
								updatedEmailBody.append("Job Family Group:").append(jobFamilyGroup).append("<br>");
							}
						} else if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamilyGroup() == null) {
							manpowerJobProfileType.setJobFamilyGroup(jobFamilyGroup);
							isUpdated = true;
							updatedEmailBody.append("Job Family Group:").append(jobFamilyGroup).append("<br>");
						} else if (manpowerJobProfileType == null) {
							manpowerProfile.setJobFamilyGroup(jobFamilyGroup);
							isNew = true;
							newMailBody.append("Job Family Group:").append(jobFamilyGroup).append("<br>");
						}
					} else if (manpowerJobProfileType != null && manpowerJobProfileType.getJobFamilyGroup() != null) {
						manpowerJobProfileType.setJobFamilyGroup(null);
						isUpdated = true;
						updatedEmailBody.append("Job Family Group:").append("").append("<br>");
					}										
					if (manpowerJobProfileType != null && manpowerJobProfileType.getWorkShiftRequired() != null && jobProfileEntry.getWorkShiftRequired() != null) {
						if (!manpowerJobProfileType.getWorkShiftRequired().equals(jobProfileEntry.getWorkShiftRequired())) {
							manpowerJobProfileType.setWorkShiftRequired(jobProfileEntry.getWorkShiftRequired());
							isUpdated = true;
							updatedEmailBody.append("Work Shift Required:") .append(jobProfileEntry.getWorkShiftRequired()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getWorkShiftRequired() == null && manpowerJobProfileType.getWorkShiftRequired() != null) {
						manpowerJobProfileType.setWorkShiftRequired(null);
						isUpdated = true;
						updatedEmailBody.append("Work Shift Required:").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getWorkShiftRequired() != null && manpowerJobProfileType.getWorkShiftRequired() == null) {
						manpowerJobProfileType.setWorkShiftRequired(jobProfileEntry.getWorkShiftRequired());
						isUpdated = true;
						updatedEmailBody.append("Work Shift Required:").append(jobProfileEntry.getWorkShiftRequired()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getWorkShiftRequired() != null) {
						manpowerProfile.setWorkShiftRequired(jobProfileEntry.getWorkShiftRequired());
						isNew = true;
						newMailBody.append("Work Shift Required:").append(jobProfileEntry.getWorkShiftRequired()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getDefaultJobTitle() != null && jobProfileEntry.getDefaultJobTitle() != null) {
						if (!manpowerJobProfileType.getDefaultJobTitle().equals(jobProfileEntry.getDefaultJobTitle())) {
							manpowerJobProfileType.setDefaultJobTitle(jobProfileEntry.getDefaultJobTitle());
							isUpdated = true;
							updatedEmailBody.append("Default Job Titile:").append(jobProfileEntry.getDefaultJobTitle()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getDefaultJobTitle() == null && manpowerJobProfileType.getDefaultJobTitle() != null) {
						manpowerJobProfileType.setDefaultJobTitle(null);
						isUpdated = true;
						updatedEmailBody.append("Default Job Titile:").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getDefaultJobTitle() != null && manpowerJobProfileType.getDefaultJobTitle() == null) {
						manpowerJobProfileType.setDefaultJobTitle(jobProfileEntry.getDefaultJobTitle());
						isUpdated = true;
						updatedEmailBody.append("Default Job Titile:").append(jobProfileEntry.getDefaultJobTitle()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getDefaultJobTitle() != null) {
						manpowerProfile.setDefaultJobTitle(jobProfileEntry.getDefaultJobTitle());
						isNew = true;
						newMailBody.append("Default Job Titile:").append(jobProfileEntry.getDefaultJobTitle()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getJobProfileCode() != null && jobProfileEntry.getJobProfileCode() != null) {
						if (!manpowerJobProfileType.getJobProfileCode().equals(jobProfileEntry.getJobProfileCode())) {
							manpowerJobProfileType.setJobProfileCode(jobProfileEntry.getJobProfileCode());
							isUpdated = true;
							updatedEmailBody.append("Job Profile Code: ").append(jobProfileEntry.getJobProfileCode()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobProfileCode() == null && manpowerJobProfileType.getJobProfileCode() != null) {
						manpowerJobProfileType.setJobProfileCode(null);
						isUpdated = true;
						updatedEmailBody.append("Job Profile Code: ").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobProfileCode() != null && manpowerJobProfileType.getJobProfileCode() == null) {
						manpowerJobProfileType.setJobProfileCode(jobProfileEntry.getJobProfileCode());
						isUpdated = true;
						updatedEmailBody.append("Job Profile Code: ").append(jobProfileEntry.getJobProfileCode()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getJobProfileCode() != null) {
						manpowerProfile.setJobProfileCode(jobProfileEntry.getJobProfileCode());
						isNew = true;
						newMailBody.append("Job Profile Code: ").append(jobProfileEntry.getJobProfileCode()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getDescription() != null && jobProfileEntry.getJobProfileName() != null) {
						if (!manpowerJobProfileType.getDescription().equals(jobProfileEntry.getJobProfileName())) {
							manpowerJobProfileType.setDescription(jobProfileEntry.getJobProfileName());
							isUpdated = true;
							updatedEmailBody.append("Description: ").append(jobProfileEntry.getJobProfileName()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobProfileName() == null && manpowerJobProfileType.getDescription() != null) {
						manpowerJobProfileType.setDescription(null);
						isUpdated = true;
						updatedEmailBody.append("Description: ").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobProfileName() != null && manpowerJobProfileType.getDescription() == null) {
						manpowerJobProfileType.setDescription(jobProfileEntry.getJobProfileName());
						isUpdated = true;
						updatedEmailBody.append("Description: ").append(jobProfileEntry.getJobProfileName()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getJobProfileName() != null) {
						manpowerProfile.setDescription(jobProfileEntry.getJobProfileName());
						isNew = true;
						newMailBody.append("Description: ").append(jobProfileEntry.getJobProfileName()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getJobCategory() != null && jobProfileEntry.getJobCategory() != null) {
						if (!manpowerJobProfileType.getJobCategory().equals(jobProfileEntry.getJobCategory())) {
							manpowerJobProfileType.setJobCategory(jobProfileEntry.getJobCategory());
							isUpdated = true;
							updatedEmailBody.append("Job Category: ").append(jobProfileEntry.getJobCategory()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobCategory() == null && manpowerJobProfileType.getJobCategory() != null) {
						manpowerJobProfileType.setJobCategory(null);
						isUpdated = true;
						updatedEmailBody.append("Job Category: ").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobCategory() != null && manpowerJobProfileType.getJobCategory() == null) {
						manpowerJobProfileType.setJobCategory(jobProfileEntry.getJobCategory());
						isUpdated = true;
						updatedEmailBody.append("Job Category: ").append(jobProfileEntry.getJobCategory()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getJobCategory() != null) {
						manpowerProfile.setJobCategory(jobProfileEntry.getJobCategory());
						isNew = true;
						newMailBody.append("Job Category: ").append(jobProfileEntry.getJobCategory()).append("<br>");
					}
					if (manpowerJobProfileType != null && manpowerJobProfileType.getJobCategoryId() != null && jobProfileEntry.getJobCategoryId() != null) {
						if (!manpowerJobProfileType.getJobCategoryId().equals(jobProfileEntry.getJobCategoryId())) {
							manpowerJobProfileType.setJobCategoryId(jobProfileEntry.getJobCategoryId());
							isUpdated = true;
							updatedEmailBody.append("Job Category Id: ").append(jobProfileEntry.getJobCategoryId()).append("<br>");
						}
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobCategoryId() == null
							&& manpowerJobProfileType.getJobCategoryId() != null) {
						manpowerJobProfileType.setJobCategoryId(null);
						isUpdated = true;
						updatedEmailBody.append("Job Category Id: ").append("").append("<br>");
					} else if (manpowerJobProfileType != null && jobProfileEntry.getJobCategoryId() != null && manpowerJobProfileType.getJobCategoryId() == null) {
						manpowerJobProfileType.setJobCategoryId(jobProfileEntry.getJobCategoryId());
						isUpdated = true;
						updatedEmailBody.append("Job Category Id: ").append(jobProfileEntry.getJobCategoryId()).append("<br>");
					} else if (manpowerJobProfileType == null && jobProfileEntry.getJobCategoryId() != null) {
						manpowerProfile.setJobCategoryId(jobProfileEntry.getJobCategoryId());
						isNew = true;
						newMailBody.append("Job Category Id: ").append(jobProfileEntry.getJobCategoryId()).append("<br>");
					}
					if (isUpdated) {
						rowCount.getAndIncrement();
						manpowerJobProfileType.setUpdateUser(UPDATE_USER);
						manpowerJobProfileType.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						manpowerIntegrationDao.saveOrUpdateJobProfileType(manpowerJobProfileType);
						updatedEmailBody.append("<br>");
						updatedMailBodyFinal.append(updatedEmailBody);
						logger.info("Updated Data saved successfully ");
					} else if (isNew) {
						rowCount.getAndIncrement();
						manpowerProfile.setUpdateUser(UPDATE_USER);
						manpowerProfile.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						manpowerIntegrationDao.saveOrUpdateJobProfileType(manpowerProfile);
						newMailBody.append("<br>");
						newMailBodyFinal.append(newMailBody);
						logger.info("New Data saved successfully ");
					}
				}
				if (updatedMailBodyFinal.length() != 0) {
					finalEmail.append("<p><strong>Update Job Profiles</strong></P>");
					finalEmail.append(updatedMailBodyFinal);
				}
				if (newMailBodyFinal.length() != 0) {
					finalEmail.append("<p><strong>New Job Profiles</strong></P>");
					finalEmail.append(newMailBodyFinal);
				}
				if (updatedMailBodyFinal.length() != 0 || newMailBodyFinal.length() != 0) {
					sendMailForJobProfile(finalEmail);
				}
				successMessage = "Job Profiles received";
			} else {
				successMessage = "No Job Profiles received";
				logger.info("No Job Profiles received");
			}
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_JOB_PROFILE, MESSAGE_TYPE_SUCCESS, successMessage, 200, null, null, null, null, null);
		} catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getWorkdayJobProfile : ").append(ex.getStatusText()).append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_JOB_PROFILE, MESSAGE_TYPE_ERROR, errorMessage, ex.getRawStatusCode(), null, null, null, null, null);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in getWorkdayJobProfile : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getWorkdayJobProfile : ").append(e.getMessage()).toString();
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_JOB_PROFILE, MESSAGE_TYPE_ERROR, errorMessage, 500, null, null, null, null, null);
		}
	}

	private void sendMailForJobProfile(StringBuilder emailBody) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_JOB_PROFILE_MAIL_NOTIFICATOIN_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE.toString());
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{JOB_PROFILE}", emailBody.toString());
		emailServiceVO.setPlaceHolder(placeHolder);
		emailService.sendEmail(emailServiceVO);
	}

	@Override
	public Boolean getWorkdayHRBusinessPartner(AwardManpowerResource resource, String supOrgId, Map<String, String> workdayConfigDetails, Integer awardId, Integer workdayManpowerInterfaceId, Integer activeAwardId) {
		Boolean success = false;
		String requestMessage = new StringBuilder("SupOrgID=").append(supOrgId).toString();
		String errorMessage = "";
		String apiUrl = new StringBuilder(workdayConfigDetails.get(Constants.WORKDAY_API)).append("HR/BusinessPartner/?").append("SupOrgID=").append(supOrgId).append("&").append(FORMAT_JSON).toString();
		try {
		logger.info("Requesting for getWorkdayHRBusinessPartner {}", apiUrl);
		ResponseEntity<String> response = callExternalCalls(apiUrl, workdayConfigDetails);
		logger.info("Response Status {}", response.getStatusCode());
		ObjectMapper objectMapper = new ObjectMapper();
		WorkdayHRBusinessPartner hRBP = objectMapper.readValue(response.getBody(), WorkdayHRBusinessPartner.class);
		if (hRBP != null && (hRBP.getReportEntries() != null && !hRBP.getReportEntries().isEmpty())) { 
			if (resource.getUpgradeTypeCode() != null && (resource.getUpgradeTypeCode().equals(Constants.MANPOWER_UPGRADE_TYPE_CODE)
					|| resource.getUpgradeTypeCode().equals(Constants.MANPOWER_UPGRADE_NEW_PROJECT_TYPE_CODE))) {
				prepareHRBPDataBasedIsUpgradeAndSendMail(resource, hRBP.getReportEntries(),activeAwardId);
			} else if(resource.getPositionStatusCode().equals(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION)) {
				sendNotificationForHiringOnExistingPosition(resource, hRBP.getReportEntries(), activeAwardId);
			} else {
				prepareHRBPDataAndSendMail(resource, hRBP.getReportEntries(), activeAwardId);
			} 
			success = true;
			logger.info("HRBP detail obtained for PISupOrgId = {}", supOrgId);
			saveManpowerLog(resource.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_SUCCESS, "HRBP detail received", 200, requestMessage, null, null, awardId, workdayManpowerInterfaceId);
		} else {
			logger.info("No HRBP detail obtained for PISupOrgId = " + supOrgId);
			saveManpowerLog(resource.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_SUCCESS, "No HRBP detail received", 200, requestMessage, null, null, awardId, workdayManpowerInterfaceId);
		}
		} catch (RestClientResponseException ex) {
			logger.info("RestClientError in getWorkdayHRBusinessPartner for  PISupOrgId = " + supOrgId +  ": " + ex.getStatusText() + (ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()));
			errorMessage = "RestClientError in getWorkdayHRBusinessPartner : "  + ex.getStatusText() + (ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString());
			saveManpowerLog(resource.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_ERROR, errorMessage, ex.getRawStatusCode(), requestMessage, null, null, awardId, workdayManpowerInterfaceId);
		} catch (Exception e) {
			logger.info("Error in getWorkdayHRBusinessPartner {}", e.getMessage());
			errorMessage = new StringBuilder("Error in getWorkdayHRBusinessPartner :").append(e.getMessage()).toString();
			saveManpowerLog(resource.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, awardId, workdayManpowerInterfaceId);
		}
		return success;
	}

	private void sendNotificationForLessCostAllocation(AwardManpowerResource resource, List<HRBPReportEntry> hRBPReportEntries,Integer activeAwardId) {
		logger.info("Received request for cost allocation less than 100% mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_LESS_COST_ALLOCATION_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(activeAwardId.toString());
		emailServiceVO.setSubModuleItemKey(resource.getManpowerResourceId().toString());
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(hRBPReportEntries));
		emailService.sendEmail(emailServiceVO);
	}

	private void sendNotificationForHiringOnExistingPosition(AwardManpowerResource resource, List<HRBPReportEntry> hRBPReportEntries, Integer activeAwardId) {
		logger.info("Received request for hiring on existing position mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(activeAwardId.toString());
		emailServiceVO.setSubModuleItemKey(resource.getManpowerResourceId().toString());
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(hRBPReportEntries));
		emailService.sendEmail(emailServiceVO);
	}

	public void prepareHRBPDataAndSendMail(AwardManpowerResource resource, List<HRBPReportEntry> hRBPReportEntries, Integer activeAwardId) {
		logger.info("Received request for HRBP mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_HRBP_DETAIL_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(activeAwardId.toString());
		emailServiceVO.setSubModuleItemKey(resource.getManpowerResourceId().toString());
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(hRBPReportEntries));
		emailService.sendEmail(emailServiceVO);
	}

	public void prepareHRBPDataBasedIsUpgradeAndSendMail(AwardManpowerResource resource, List<HRBPReportEntry> hRBPReportEntries, Integer activeAwardId) {
		logger.info("Received request for HRBP Upgrade mail for sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_HRBP_UPGRADE_DETAIL_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(activeAwardId.toString());
		emailServiceVO.setSubModuleItemKey(resource.getManpowerResourceId().toString());
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(hRBPReportEntries));
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getDynamicPlaceholders(List<HRBPReportEntry> hRBPReportEntries) {
		Map<String, String> placeHolder = new HashMap<>();
		String hRBPTable = prepareHRBPTableContent(hRBPReportEntries);
		placeHolder.put("{HRBP_DETAIL}", hRBPTable);
		return placeHolder;
	}

	private String prepareHRBPTableContent(List<HRBPReportEntry> hRBPReportEntries) {
		String tableContent = "";
		String tableHeader = new StringBuilder("<figure class=\"table\"><table border=\"1px solid black\"><tbody><tr><td><strong>S No</strong></td><td><strong>HRBP Name</strong></td>").append("<td><strong>HRBP Email</strong></td><td><strong>Role Name</strong></td></tr>").toString();
		String tableClose = "</tbody></table></figure>";
		Integer rowCount = 1;
		for (HRBPReportEntry entry : hRBPReportEntries) {
			if ((entry.gethRBPClusterLeads() != null && !entry.gethRBPClusterLeads().isEmpty()) || (entry.gethRBusinessPartnerResearchers() != null && !entry.gethRBusinessPartnerResearchers().isEmpty())) {
				if (entry.gethRBPClusterLeads() != null && !entry.gethRBPClusterLeads().isEmpty()) {
					for (HRBPClusterLead lead : entry.gethRBPClusterLeads()) {
						tableContent = new StringBuilder(tableContent).append("<tr><td>").append(rowCount.toString()).append("</td><td>").append(lead.gethRBPClName()).append("</td><td>").append(lead.gethRBPClEmail()).append("</td><td>").append("HRBP Cluster Lead (Research)").append("</td></tr>").toString();
						++rowCount;
					}
				}
				if (entry.gethRBusinessPartnerResearchers() != null && !entry.gethRBusinessPartnerResearchers().isEmpty()) {
					for (HRBusinessPartnerResearch research : entry.gethRBusinessPartnerResearchers()) {
						tableContent = new StringBuilder(tableContent).append("<tr><td>").append(rowCount.toString()).append("</td><td>").append(research.gethRBPName()).append("</td><td>").append(research.gethRBPEmail()).append("</td><td>").append("HR Business Partner (Research)").append("</td></tr>").toString();
						++rowCount;
					}
				}
			} else {
				return "";
			}
		}	
		return new StringBuilder(tableHeader).append(tableContent).append(tableClose).toString();
	}

	@Override
	public boolean checkIfAwardHasManpowerStaffResource(Integer awardId) {
		return manpowerIntegrationDao.checkIfAwardHasManpowerStaffResource(awardId);
	}

	@Override
	public void sendHrbpDetailsToPI(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, String superiorSupOrg, String piPersonId, Integer activeAwardId) {
		List<AwardSupOrgMapping> supOrgMappings = new ArrayList<>();
		AwardSupOrgMapping supOrgMapping = null;
		if (superiorSupOrg != null && !superiorSupOrg.equals("")) {
			supOrgMappings = manpowerIntegrationDao.getAwardUpOrgMappingByAwardNumberAndPIPersonId(workdayManpowerInterface.getAwardNumber(), piPersonId, superiorSupOrg);
			if (supOrgMappings != null && !supOrgMappings.isEmpty()) {
				supOrgMapping = supOrgMappings.get(0);
			}
		}		
		String requestMessage = new StringBuilder("AwardNumber=").append(workdayManpowerInterface.getAwardNumber()).toString();
		String errorMessage = "";
		try {
			if (supOrgMapping != null) {
				AwardManpowerResource resource = manpowerIntegrationDao.getAwardManpowerResourceById(workdayManpowerInterface.getAwardManpowerResourceId());
				if (resource.getPositionId() != null) {
					if (Boolean.TRUE.equals(getWorkdayHRBusinessPartner(resource, supOrgMapping.getSupOrgId(), workdayConfigDetails, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId(), activeAwardId))) {
						saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
					} else {
						saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
					}
				} else {
					saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
					saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_INTERFACE_PENDING, "No Position Id", 500, requestMessage, null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
				}
			} else {
				saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
				saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_INTERFACE_PENDING, "No supOrg Id", 500, requestMessage, null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
			}
		} catch (Exception e) {
			logger.error("Error in sendHrbpDetailsToPI : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in sendHrbpDetailsToPI : ").append(e.getMessage()).toString();
			saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_FIBI_ERROR, errorMessage, 500, requestMessage, null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
	}

	@Override
	public void assignCostAllocation(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, List<AwardManpowerResource> costAllocationSuccessResources,Integer activeAwardId) {
		String requestMessage = new StringBuilder("AwardNumber=").append(workdayManpowerInterface.getAwardNumber()).toString();
		String errorMessage = "";
		try {
			if (workdayManpowerInterface.getAwardManpowerResource().getCostAllocation().equals(new BigDecimal("100.00"))) {
				if (Boolean.TRUE.equals(assignCostAllocationInWorkday(workdayManpowerInterface, workdayConfigDetails))) {
			     	saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
			     	AwardManpowerResource awardManpowerResource = workdayManpowerInterface.getAwardManpowerResource();
			     	Timestamp allocationStartDate = awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate();
					Timestamp allocationEndDate = awardManpowerResource.getChargeEndDate() == null ? awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate();
					List<String> awardStatuses = new ArrayList<>();
					awardStatuses.add(Constants.AWARD_FINAL_STATUS_ACTIVE);
					awardStatuses.add(Constants.AWARD_FINAL_STATUS_PENDING);
					AwardManpowerBaseSalaryHistory baseSalaryHistory = new AwardManpowerBaseSalaryHistory();
					List<AwardManpowerResource> resources = manpowerIntegrationDao.getAwardManpowerResourcesResourceUniqueIdAndAwardSequenceStatues(awardManpowerResource.getResourceUniqueId(), awardStatuses);
					for (AwardManpowerResource resource : resources) {
						resource.setCostAllocation(awardManpowerResource.getCostAllocation());
						resource.setChargeStartDate(allocationStartDate);
						resource.setPreviousChargeStartDate(allocationStartDate);
						resource.setChargeEndDate(allocationEndDate);
						resource.setPreviousChargeEndDate(allocationEndDate);
						resource.setChargeDuration(awardManpowerResource.getChargeDuration() == null ? awardManpowerResource.getPlanDuration() : awardManpowerResource.getChargeDuration());
						calculateActualCommittedCost(resource, workdayManpowerInterface.getAwardManpower(), baseSalaryHistory, costAllocationSuccessResources, activeAwardId);	
					}
			     	saveBaseSalaryHistory(baseSalaryHistory, awardManpowerResource, allocationStartDate, allocationEndDate);
				} else {
					saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
				}
			}
		} catch (Exception e) {
			logger.error("Error in assignCostAllocation : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in assignCostAllocation : ").append(e.getMessage()).toString();
			saveManpowerLog(workdayManpowerInterface.getAwardNumber(), Constants.MANPOWER_INTERFACE_COST_ALLOCATION, MESSAGE_TYPE_FIBI_ERROR, errorMessage, 500, requestMessage, null, null, workdayManpowerInterface.getAwardId(), workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
	}

	private void calculateActualCommittedCost(AwardManpowerResource resource, AwardManpower awardManpower, AwardManpowerBaseSalaryHistory baseSalaryHistory, List<AwardManpowerResource> costAllocationSuccessResources, Integer activeAwardId) {
		String multiplier = manpowerService.findMultiplier(activeAwardId);
		BigDecimal committedCost =  manpowerService.calculatePlannedCost(resource, awardManpower.getAwardId().toString(), Boolean.TRUE, multiplier);
		if (committedCost != null) {
			Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(resource.getPersonId());
			resource.setBaseSalaryUsed(manpower.getBaseSalary());
			resource.setCommittedCost(committedCost);
			resource.setPositionStatusCode(Constants.MANPOWER_ACTIVE);
			resource.setMultiplierValueUsed(new BigDecimal(multiplier));
			baseSalaryHistory.setCurrentBaseSalary(manpower.getBaseSalary());
			if (resource.getAwardManpowerId().equals(awardManpower.getAwardManpowerId())) {
				BigDecimal budgetAmount = manpowerIntegrationDao.getBudgetAmountByAwardIdAndBudgetReferenceNumber(awardManpower.getAwardId(), awardManpower.getBudgetReferenceNumber());
				if (budgetAmount != null) {
					ManpowerVO manpowerVo = new ManpowerVO();
					manpowerVo.setBudgetAmount(budgetAmount);
					manpowerVo = manpowerService.setInitialCommittedAmount(manpowerVo, awardManpower, committedCost, null, resource.getManpowerResourceId());
					if (Boolean.TRUE.equals(manpowerVo.getIsSalaryValidationExist())) {
						costAllocationSuccessResources.add(resource);
					}
				}
			}
		}
		manpowerIntegrationDao.saveOrUpdateAwardManpowerResource(resource);
	}

	@Override
	public void moveWorkers(WorkdayManpowerInterface workdayManpowerInterface, Map<String, String> workdayConfigDetails, Integer activeAwardId, StringBuilder moveWorkerSuccessMailContent, StringBuilder moveWorkerErrorMailContent) {
		logger.info("Requesting for Move workers");
		String awardNumber = workdayManpowerInterface.getAwardNumber();
		String oldPiPersonId = workdayManpowerInterface.getOldPIPersonId();
		String newPiPersonId = workdayManpowerInterface.getNewPIPersonId();
		String oldSuperiorSupOrgId = workdayManpowerInterface.getOldSuperiorSupOrg();
		String newSuperiorSupOrgId = workdayManpowerInterface.getNewSuperiorSupOrg();
		AwardManpowerResource resource = workdayManpowerInterface.getAwardManpowerResource();
		logger.info("awardNumber : {}", awardNumber);
		logger.info("oldPiPersonId : {}", oldPiPersonId);
		logger.info("newPiPersonId : {}", newPiPersonId);
		logger.info("oldSuperiorSupOrgId : {}", oldSuperiorSupOrgId);
		logger.info("newSuperiorSupOrgId : {}", newSuperiorSupOrgId);
		StringBuilder requestMessage = new StringBuilder("AwardNumber=").append(awardNumber);
		StringBuilder resourceDetail = new StringBuilder();
		if (resource != null) {
			resourceDetail.append("Position Id=").append(resource.getPositionId() == null ? "" : resource.getPositionId());
			if (resource.getPersonId() != null) {
				resourceDetail.append("Person Id=").append(resource.getPersonId() == null ? "" : resource.getPersonId());
			}
			resourceDetail.append("<br/>");
			requestMessage.append(resourceDetail);
		}
		String errorMessage = "";
		String message = "";
		Boolean responsePending = false;
		try {
			if (oldSuperiorSupOrgId != null && !oldSuperiorSupOrgId.equals("")) {
				if (newSuperiorSupOrgId != null && !newSuperiorSupOrgId.equals("")) {
					List<AwardSupOrgMapping> oldSupOrgMappings = manpowerIntegrationDao.getAwardUpOrgMappingByAwardNumberAndPIPersonId(awardNumber, oldPiPersonId, oldSuperiorSupOrgId);
					if (oldSupOrgMappings != null && !oldSupOrgMappings.isEmpty()) {
						AwardSupOrgMapping oldSupOrgMapping = oldSupOrgMappings.get(0);
						List<AwardSupOrgMapping> newSupOrgMappings = manpowerIntegrationDao.getAwardUpOrgMappingByAwardNumberAndPIPersonId(awardNumber, newPiPersonId, newSuperiorSupOrgId);
						if (newSupOrgMappings != null && !newSupOrgMappings.isEmpty()) {
							AwardSupOrgMapping newSupOrgMapping = newSupOrgMappings.get(0);
							logger.info("oldSupOrgMapping.getSupOrgId : {}", oldSupOrgMapping.getSupOrgId());
							logger.info("newSupOrgMapping.getSupOrgId : {}", newSupOrgMapping.getSupOrgId());
							if (resource != null) {
								if (Boolean.TRUE.equals(moveWorkersByOrganizationInWorkday(workdayManpowerInterface,
										resource.getPositionId(), oldSupOrgMapping.getSupOrgId(),
										newSupOrgMapping.getSupOrgId(), workdayConfigDetails,
										resource.getPersonId()))) {
									saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_SUCCESS);
									moveWorkerSuccessMailContent.append(resourceDetail);
								} else {
									saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
									moveWorkerErrorMailContent.append(resourceDetail);
								}
							}
						} else {
							responsePending = true;
							message = new StringBuilder("No To Sup Org Id for AwardNumber = ").append(awardNumber).append(" and PIPersonId ").append(newPiPersonId).toString();
							logger.info(message);
						}
					} else {
						message = new StringBuilder("No From Sup Org Id for AwardNumber = ").append(awardNumber).append(" and PIPersonId ").append(oldPiPersonId).toString();
						logger.info(message);
					}
				} else {
					message = new StringBuilder("No New Superior Sup  Org Id for AwardNumber = ").append(awardNumber).toString();
					logger.info(message);
				}
			} else {
				message = new StringBuilder("No Old Superior Sup Org Id for AwardNumber = ").append(awardNumber).toString();
				logger.info(message);
			}
			if (!message.equals("")) {
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, Boolean.TRUE.equals(responsePending) ? MESSAGE_TYPE_INTERFACE_PENDING : MESSAGE_TYPE_FIBI_ERROR,
						message, 500, requestMessage.toString(), null, null, workdayManpowerInterface.getAwardId(),
						workdayManpowerInterface.getWorkdayManpowerInterfaceId());
				saveWorkdayManpowerInterfaceWithInterfaceStatus(workdayManpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
				if (!moveWorkerErrorMailContent.toString().contains(message)) {
					moveWorkerErrorMailContent.append(message);
					moveWorkerErrorMailContent.append("<br/>");
				}
				moveWorkerErrorMailContent.append(resourceDetail);
			}
		} catch (Exception e) {
			logger.error("Error in moveWorkers : {}", e.getMessage());
			errorMessage = new StringBuilder("Error in moveWorkers :").append(e.getMessage()).toString();
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_MOVE_WORKERS, MESSAGE_TYPE_FIBI_ERROR,
					errorMessage, 500, requestMessage.toString(), null, null, workdayManpowerInterface.getAwardId(),
					workdayManpowerInterface.getWorkdayManpowerInterfaceId());
		}
	}

	@Override
	public void sendMoveWorkerNotificationMail(StringBuilder exclusionMailContent, StringBuilder successMailContent, StringBuilder errorMailContent, Integer awardId) {
		logger.info("Received request for sendMoveWorkersNotification mail sending");
		StringBuilder emailContent = new StringBuilder();
		Map<String, String> placeHolder = new HashMap<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MOVE_WORKERS_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(awardId.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (exclusionMailContent.length() != 0) {
			emailContent.append("Exclusion in Move Worker:").append("<br/>");
			emailContent.append(exclusionMailContent);
			emailContent.append("<br/>");
		}
		if (successMailContent.length() != 0) {
			emailContent.append("Success :").append("<br/>");
			emailContent.append(successMailContent);
			emailContent.append("<br/>");
		}
		if (errorMailContent.length() != 0) {
			emailContent.append("Error :").append("<br/>");
			emailContent.append(errorMailContent);
			emailContent.append("<br/>");
		}
		emailServiceVO.setPlaceHolder(getPlaceHolderData(placeHolder, emailContent));
		emailService.sendEmail(emailServiceVO);
	}

	private void prepareMoveWorkerErrorNotification(List<AwardManpowerResource> notMigratedManpowers, String message, Integer awardId, StringBuilder failureMailContent) {
		StringBuilder emailContent = new StringBuilder();
		if (notMigratedManpowers != null && !notMigratedManpowers.isEmpty()) {
			for (AwardManpowerResource excludedManpowerResource : notMigratedManpowers) {
				emailContent.append("Position Id : ").append(excludedManpowerResource.getPositionId()).append(",");
				if (excludedManpowerResource.getPersonId() != null) {
					emailContent.append("Person Id : ").append(excludedManpowerResource.getPersonId()).append(",");
				}
				emailContent.append("<br/>");
			}
		}
		if (message != null && !message.equals("")) {
			emailContent.append("<br/>");
			emailContent.append(message);
		}
		failureMailContent.append(emailContent);
	}

	@Override
	public void prepareNotificationForChangeInPIOrSuperiorSupOrgId(String oldPiPersonId, String newPiPersonId, String oldSuperiorSupOrgId, String newSuperiorSupOrgId, Integer awardId) {
		StringBuilder emailContent = new StringBuilder();  
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if (!oldPiPersonId.equals("") && !newPiPersonId.equals("") && !oldPiPersonId.equals(newPiPersonId) &&
				!oldSuperiorSupOrgId.equals("") && !newSuperiorSupOrgId.equals("") && oldSuperiorSupOrgId.equals(newSuperiorSupOrgId)) {
			String oldPersonName = personDao.getPersonFullNameByPersonId(oldPiPersonId);
			String newPersonName = personDao.getPersonFullNameByPersonId(newPiPersonId);
			emailContent.append("  PI   ").append(oldPersonName).append("  is changed  to   ").append(newPersonName);
			commonService.setNotificationRecipients(oldPiPersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendNotificationForChangeInPIOrSuperiorSupOrgId(awardId, emailContent, dynamicEmailRecipients);
		} else if( !oldPiPersonId.equals("") && !newPiPersonId.equals("") && !oldSuperiorSupOrgId.equals("") && !newSuperiorSupOrgId.equals("") 
				&& !oldSuperiorSupOrgId.equals(newSuperiorSupOrgId) && oldPiPersonId.equals(newPiPersonId)) {
			emailContent.append("  Superior Sup Org  ").append(oldSuperiorSupOrgId).append("   is changed  to  ").append(newSuperiorSupOrgId);
			sendNotificationForChangeInPIOrSuperiorSupOrgId(awardId, emailContent, dynamicEmailRecipients);
		} else if ( !oldPiPersonId.equals("") && !newPiPersonId.equals("") && !oldPiPersonId.equals(newPiPersonId) 
				&& !oldSuperiorSupOrgId.equals("") && !newSuperiorSupOrgId.equals("") && !oldSuperiorSupOrgId.equals(newSuperiorSupOrgId)) {
			String oldPersonName = personDao.getPersonFullNameByPersonId(oldPiPersonId);
			String newPersonName = personDao.getPersonFullNameByPersonId(newPiPersonId);
			emailContent.append(" PI   ").append(oldPersonName).append("   is changed  to  ").append(newPersonName)
			.append(" and Superior Sup Org  ").append(oldSuperiorSupOrgId).append("   is changed  to  ").append(newSuperiorSupOrgId);
			commonService.setNotificationRecipients(oldPiPersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			sendNotificationForChangeInPIOrSuperiorSupOrgId(awardId, emailContent, dynamicEmailRecipients);
		}
	}

	private void sendNotificationForChangeInPIOrSuperiorSupOrgId(Integer awardId, StringBuilder emailContent, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		Map<String, String> placeHolder = new HashMap<>();
		logger.info("sendNotificationForChangeInPIOrSuperiorSupOrgId");
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_CHANGE_IN_PI_SUPERIOR_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(awardId.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		Award award = new Award();
		award.setAwardId(awardId);
		award.setAwardNumber(awardDao.getAwardNumberBasedOnAwardId(awardId));
		getDynamicEmailRecipients(emailServiceVO, award, dynamicEmailRecipients);
		emailServiceVO.setPlaceHolder(getPlaceHolderForChangePI(placeHolder, emailContent));
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getPlaceHolderForChangePI(Map<String, String> placeHolder, StringBuilder mailContent) {
		placeHolder.put("{CONTENT_OF_CHANGED_PI_SUPERIOR}", mailContent.toString());
		return placeHolder;
	}

	private void prepareMoveWorkersSuccessNotification(Boolean workersToMove, List<AwardManpowerResource> sucsessManpowers, List<AwardManpowerResource> awardManpowerResources,
			String message, Integer awardId, StringBuilder successMailContent) {
		Boolean success = false;
		Boolean error = false;
		if (sucsessManpowers != null && !sucsessManpowers.isEmpty()) {
			StringBuilder emailContent = new StringBuilder();
			if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
				for (AwardManpowerResource excludedManpowerResource : awardManpowerResources) {
					if (excludedManpowerResource.getPositionId() != null) {
						success = true;
						emailContent.append("Position Id : ").append(excludedManpowerResource.getPositionId()).append(",");
					}
					if (excludedManpowerResource.getPersonId() != null) {
						emailContent.append("Person Id : ").append(excludedManpowerResource.getPersonId()).append(",");
					}
					emailContent.append("<br/>");
				}
			}
			if (message != null && !message.equals("")) {
				emailContent.append("<br/>");
				emailContent.append(message);
			}
			if (success) {
				successMailContent.append("Successfully Moved Workers: ").append("<br/>").append(emailContent);
			}
		}
		if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
			StringBuilder emailContent = new StringBuilder();
			awardManpowerResources.removeAll(sucsessManpowers);
			if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
				for (AwardManpowerResource excludedManpowerResource : awardManpowerResources) {
					if (excludedManpowerResource.getPositionId() != null) {
						error = true;
						emailContent.append("Position Id : ").append(excludedManpowerResource.getPositionId()).append(",");
					}
					if (excludedManpowerResource.getPersonId() != null) {
						emailContent.append("Person Id : ").append(excludedManpowerResource.getPersonId()).append(",");
					}
					emailContent.append("<br/>");
				}
			}
			if (message != null && !message.equals("")) {
				emailContent.append("<br/>");
				emailContent.append(message);
			}
			if (error) {
				successMailContent.append("Error in Move worker position: ").append("<br/>").append(emailContent);
			}
		}
	}

	private Map<String, String> getPlaceHolderData(Map<String, String> placeHolder, StringBuilder mailContent) {
		placeHolder.put("{POSITION_NUMBER}", mailContent.toString().replaceAll(",$", ""));
		return placeHolder;
	}

	@Override
	public void getManpowerDetails(AtomicInteger rowCount) throws ParseException {
		String requestMessage = new StringBuilder("Effective_as_of_Date=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
		Map<String, String> configDetails = getWorkdayConfigDetails();
		String successMessage = "";
		String errorMessage = null;
		try {
			String apiUrl = new  StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("ManpowerDetails/?").append(FORMAT_JSON).append("&Effective_as_of_Date=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
			ObjectMapper objectMapper = new ObjectMapper();
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			ManpowerDetails manpowerDetails = objectMapper.readValue(response.getBody(), ManpowerDetails.class);
			if (manpowerDetails != null && (manpowerDetails.getReportEntry() != null && !manpowerDetails.getReportEntry().isEmpty())) {
				manpowerDetails.getReportEntry().stream().forEach(manpowerDetail -> {
					if (manpowerDetail.getEMPLOYEEID() != null) {
						rowCount.getAndIncrement();
						Person person = savePersonDetails(manpowerDetail);
						Manpower manpower = saveOrUpdateManpower(manpowerDetail, person);
						saveOrUpdateManpowerResource(manpower, person);
					}
				});
				successMessage = new StringBuilder("ManpowerDetails Received").toString();
				logger.info(successMessage);
			} else {
				successMessage = new StringBuilder("No ManpowerDetails Received").toString();
				logger.info(successMessage);
			}
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_MANPOWER_DETAILS, MESSAGE_TYPE_SUCCESS, successMessage, 200, requestMessage, null, null, null, null);
		} catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getManpowerDetails : ").append(ex.getStatusText()).append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_MANPOWER_DETAILS, MESSAGE_TYPE_ERROR, errorMessage, ex.getRawStatusCode(), requestMessage, null, null, null, null);
		} catch (Exception e) {
			errorMessage = new StringBuilder("Error in getManpowerDetails : ").append(e.getMessage()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_MANPOWER_DETAILS, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, null, null);
		}
	}

	private ResponseEntity<String> callExternalCalls(String apiUrl, Map<String, String> configDetails) {
		RestTemplate restTemplate = new RestTemplate();
		logger.info("Requesting for api url  : {}", apiUrl );
		HttpHeaders headers = new HttpHeaders();
		headers = setWorkdayHttpHeaders(headers, configDetails);
		HttpEntity<String> entity = new HttpEntity<>("parameters", headers);
		ResponseEntity<String> response = restTemplate.exchange(apiUrl, HttpMethod.GET, entity, String.class);
		logger.info("Response Status {}", response.getStatusCode());
		return response;
	}

	private Person savePersonDetails(ManpowerReportEntry manpowerDetail) {
		Person person = personDao.getPersonDetailById(manpowerDetail.getEMPLOYEEID());
		if (person == null) {
			person = new Person();
			person.setPersonId(manpowerDetail.getEMPLOYEEID());
			person.setStatus("I");
			person.setUpdateUser("workday admin");
			person.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		}
		if (manpowerDetail.getGENDER() != null) {
			person.setGender(manpowerDetail.getGENDER());
		}
		personDao.saveOrUpdatePerson(person);
		return person;
	}

	private Manpower saveOrUpdateManpower(ManpowerReportEntry manpowerDetail, Person person) {
		Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(person.getPersonId());
		try {
			if (manpower == null) {
				manpower = new Manpower();
				manpower.setManpowerPersonId(person.getPersonId());
			}
			if (manpowerDetail.getPOSITIONID() != null) {
				manpower.setPositionId(manpowerDetail.getPOSITIONID());
			}
			if (manpowerDetail.getHIREDATE() != null) {
				Date hireDate = new SimpleDateFormat("yyyy-MM-dd").parse(manpowerDetail.getHIREDATE());
				manpower.setHireDate(new Timestamp(hireDate.getTime()));
			}
			if (manpowerDetail.getCONTRACTENDDATE() != null) {
				Date contractEndDate = new SimpleDateFormat("yyyy-MM-dd").parse(manpowerDetail.getCONTRACTENDDATE());
				manpower.setContractEndDate(new Timestamp(contractEndDate.getTime()));
			}
			if (manpowerDetail.getJOBCODE() != null) {
				manpower.setJobCode(manpowerDetail.getJOBCODE());
			}
			manpower.setFullName(person.getFullName());
			manpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			manpower.setUpdateUser(UPDATE_USER);
			manpowerIntegrationDao.saveOrUpdate(manpower);
		} catch (Exception e) {
			logger.error("Error occured in saveOrUpdateManpower : {}", e.getMessage());
		}
		return manpower;
	}

	@SuppressWarnings("deprecation")
	private void saveOrUpdateManpowerResource(Manpower manpower, Person person) {
		int cutOffDay = Integer.parseInt(manpowerService.getManpowerConfigurationValueAsString(Constants.MANPOWER_CUT_OFF_DATE));
		List<String> statuses = new ArrayList<String>();
		statuses.add(Constants.MANPOWER_POSITION_GENERATED);
		statuses.add(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION);
		List<AwardManpowerResource> awardManpowerResources = manpowerIntegrationDao
				.getManpowerResourcesByParams(manpower.getPositionId(), statuses);
		awardManpowerResources.stream().forEach(awardManpowerResource -> {
			try {
				if ((manpower.getHireDate() != null && awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION) && (awardManpowerResource.getPlanStartDate().equals(manpower.getHireDate()) || awardManpowerResource.getPlanStartDate().before(manpower.getHireDate()))) || (awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_POSITION_GENERATED))) {
					if (awardManpowerResource.getUpgradeTypeCode() != null && (awardManpowerResource.getUpgradeTypeCode().equals(Constants.MANPOWER_UPGRADE_TYPE_CODE)
							|| awardManpowerResource.getUpgradeTypeCode().equals(Constants.MANPOWER_UPGRADE_NEW_PROJECT_TYPE_CODE))) {
						awardManpowerResource.setChargeStartDate(awardManpowerResource.getPlanStartDate());
						awardManpowerResource.setChargeEndDate(awardManpowerResource.getPlanEndDate());
						awardManpowerResource.setChargeDuration(awardManpowerResource.getPlanDuration());
					} else {
						String awardNumber = awardManpowerResource.getAwardNumber();
						logger.info("awardNumber : {}", awardNumber);
						Award award = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
						awardManpowerResource.setPersonId(manpower.getManpowerPersonId());
						Timestamp hiredDate = manpower.getHireDate();
						logger.info("hiredDate : {}", hiredDate);
						Timestamp beginDate = award.getBeginDate();
						logger.info("beginDate : {}", beginDate);
						Timestamp systemDateTime = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
						logger.info("systemDateTime : {}", systemDateTime);
						int systemDay = systemDateTime.getDate();
						if (hiredDate != null && hiredDate.before(beginDate)) {
							logger.info("systemDay : {}", systemDay);
							if (systemDay > cutOffDay) {
								awardManpowerResource.setChargeStartDate(getNextMonthFirstDay(systemDateTime));
							} else {
								awardManpowerResource.setChargeStartDate(removeTimeFactorFromDate(systemDateTime));
							}
						} else {
							int hireDay = hiredDate.getDate();
							logger.info("hireDay : {}", hireDay);
							if ((systemDay > cutOffDay && (hiredDate.before(systemDateTime) || hiredDate.equals(systemDateTime) || (hiredDate.after(systemDateTime) && hiredDate.getMonth() == systemDateTime.getMonth() && hiredDate.getYear() == systemDateTime.getYear()))) || (hireDay > cutOffDay && hiredDate.before(systemDateTime))) {
								awardManpowerResource.setChargeStartDate(getNextMonthFirstDay(hiredDate));
							} else {
								awardManpowerResource.setChargeStartDate(hiredDate);
							}
						}
						Timestamp contractFinalDate = manpower.getContractEndDate();
						Timestamp finalExpirationDate = award.getFinalExpirationDate();
						logger.info("contractFinalDate : {}", contractFinalDate);
						logger.info("finalExpirationDate : {}", finalExpirationDate);
						if (contractFinalDate == null || contractFinalDate.after(finalExpirationDate)) {
							awardManpowerResource.setChargeEndDate(finalExpirationDate);
						} else {
							awardManpowerResource.setChargeEndDate(contractFinalDate);
						}
						if (awardManpowerResource.getChargeStartDate() != null && awardManpowerResource.getChargeEndDate() != null) {
							LocalDate startDate = awardManpowerResource.getChargeStartDate().toInstant().atZone(ZoneId.of("UTC")).toLocalDate();
							LocalDate endDate = awardManpowerResource.getChargeEndDate().toInstant().atZone(ZoneId.of("UTC")).toLocalDate().plusDays(1);
							Period diff = Period.between(startDate, endDate);
							String chargeDuration = (diff.getYears() + "year(s)," + diff.getMonths() + " month(s) & "
									+ diff.getDays() + " day(s)");
							logger.info("chargeDuration : {}", chargeDuration);
							awardManpowerResource.setChargeDuration(chargeDuration);
						}
					}
					if (manpower.getJobCode() != null) {
						awardManpowerResource.setJobProfileTypeCode(manpower.getJobCode());
					}
					awardManpowerResource.setFullName(person.getFullName());
					manpowerIntegrationDao.saveOrUpdateAwardManpowerResource(awardManpowerResource);
				}
			} catch (Exception e) {
				String error = new StringBuilder("Error in saveOrUpdateManpowerResource for person ").append(manpower.getManpowerPersonId()).append(" and position ").append(manpower.getPositionId()).append(e.getMessage()).toString();
				logger.error(error);
			}
		});
	}

	public Timestamp getNextMonthFirstDay(Timestamp systemDateTime) {
		Calendar calendar = Calendar.getInstance();
		calendar.add(Calendar.MONTH, 1);
		calendar.set(Calendar.DATE, calendar.getActualMinimum(Calendar.DAY_OF_MONTH));
		systemDateTime.setTime(calendar.getTime().getTime());
		return removeTimeFactorFromDate(systemDateTime);
	}

	@SuppressWarnings("deprecation")
	private Timestamp removeTimeFactorFromDate(Timestamp systemDateTime) {
		systemDateTime.setHours(00);
		systemDateTime.setMinutes(00);
		systemDateTime.setSeconds(00);
		systemDateTime.setNanos(00);
		return systemDateTime;
	}

	@Override
	public void getNationalityDetails(AtomicInteger rowCount) throws ParseException {
		Map<String, String> configDetails = getWorkdayConfigDetails();
		String errorMessage = null;
		String requestMessage = new StringBuilder("Effective_as_of_Date=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
		String successMessage = "";
		try {
			String apiUrl = new  StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("CitizenNationality/?").append(FORMAT_JSON).append("&Effective_as_of_Date=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			ObjectMapper objectMapper = new ObjectMapper();
			CitizenshipNationality citizenshipNationalities = objectMapper.readValue(response.getBody(), CitizenshipNationality.class);
			if (citizenshipNationalities != null && (citizenshipNationalities.getReportEntry() != null && !citizenshipNationalities.getReportEntry().isEmpty())) {
				citizenshipNationalities.getReportEntry().stream().forEach(citizenshipNationality -> {	
					if (citizenshipNationality.getEMPLOYEEID() != null) {
						rowCount.getAndIncrement();
						saveOrUpdateManpower(citizenshipNationality);
					}
				});
				successMessage = new StringBuilder("CitizenshipNationalityDetails Received").toString();
				logger.info(successMessage);
			} else {
				successMessage = new StringBuilder("No CitizenshipNationalityDetails Received").toString();
				logger.info(successMessage);
			}
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_CITIZENSHIP_NATIONALITY, MESSAGE_TYPE_SUCCESS, successMessage, 200, requestMessage, null, null, null, null);
		} catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getNationalityDetails : ").append(ex.getRawStatusCode()).append(ex.getStatusText()).append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_CITIZENSHIP_NATIONALITY, MESSAGE_TYPE_ERROR, errorMessage, ex.getRawStatusCode(), requestMessage, null, null, null, null);
		} catch (Exception e) {
			errorMessage = new StringBuilder("Error in getNationalityDetails : ").append(e.getMessage()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_CITIZENSHIP_NATIONALITY, MESSAGE_TYPE_ERROR, errorMessage, 500, requestMessage, null, null, null, null);
		}
	}

	private void saveOrUpdateManpower(CitizenReportEntry citizenshipNationality) {
		Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(citizenshipNationality.getEMPLOYEEID());
		if (manpower != null) {
			try {
				if (citizenshipNationality.getCITIZENSHIP() != null) {
					manpower.setCitizenship(excelityService.encryptAES(citizenshipNationality.getCITIZENSHIP()));
				}
				if (citizenshipNationality.getNATIONALITY() != null) {
					manpower.setNationality(excelityService.encryptAES(citizenshipNationality.getNATIONALITY()));
				}
				manpower.setUpdateUser(UPDATE_USER);
				manpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				manpowerIntegrationDao.saveOrUpdate(manpower);
			} catch (Exception e) {
				logger.info("Error occured in saveOrUpdateManpower : {}", e.getMessage());
			}
		}
	}

	@Override
	public void prepareAndSendFreezeReducedMail(String positionIds, Award award) {
		logger.info("Received request for FreezeDate Error mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_POSITION_FREEZE_ERROR_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode("0");
		emailServiceVO.setModuleItemKey(award.getAwardId().toString());
		emailServiceVO.setSubModuleItemKey("0");
		getDynamicEmailRecipients(emailServiceVO, award, dynamicEmailRecipients);
		emailServiceVO.setPlaceHolder(getFreezeMailDynamicPlaceholders(award.getFinalExpirationDate(), positionIds));
		emailService.sendEmail(emailServiceVO);
	}

	private void getDynamicEmailRecipients(EmailServiceVO emailServiceVO, Award award, Set<NotificationRecipient> dynamicEmailRecipients) {
		AwardSupOrgMapping awardSupOrgMapping = manpowerIntegrationDao.getLatestAwardSupOrgByPI(award.getAwardNumber(), manpowerIntegrationDao.getPIPersonIdByAwardId(award.getAwardId()));
		if (awardSupOrgMapping!= null && awardSupOrgMapping.getSupOrgId() != null && !awardSupOrgMapping.getSupOrgId().equals("")) {
			WorkdayHRBusinessPartner hrbp = getHRBPDetails(awardSupOrgMapping.getSupOrgId(),getWorkdayConfigDetails(), award.getAwardNumber(), award.getAwardId(), null);
			dynamicEmailRecipients = getMailIdsFromHrbp(hrbp, dynamicEmailRecipients);
			if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
				emailServiceVO.setRecipients(dynamicEmailRecipients);
			}
		}
	}

	private Set<NotificationRecipient> getMailIdsFromHrbp(WorkdayHRBusinessPartner hrbp, Set<NotificationRecipient> dynamicEmailRecipients) {
		if (hrbp != null && (hrbp.getReportEntries() != null && !hrbp.getReportEntries().isEmpty())) { 
			for (HRBPReportEntry entry : hrbp.getReportEntries()) {
				if ((entry.gethRBPClusterLeads() != null && !entry.gethRBPClusterLeads().isEmpty()) || (entry.gethRBusinessPartnerResearchers() != null && !entry.gethRBusinessPartnerResearchers().isEmpty())) {
					if (entry.gethRBPClusterLeads() != null && !entry.gethRBPClusterLeads().isEmpty()) {
						for (HRBPClusterLead lead : entry.gethRBPClusterLeads()) {
							if (lead.gethRBPClEmail() != null && !lead.gethRBPClEmail().equals("")) {
								commonService.setNotificationRecipientsforNonEmployees(lead.gethRBPClEmail(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
							}
						}
					}
					if (entry.gethRBusinessPartnerResearchers() != null && !entry.gethRBusinessPartnerResearchers().isEmpty()) {
						for (HRBusinessPartnerResearch research : entry.gethRBusinessPartnerResearchers()) {
							if (research.gethRBPEmail() != null && !research.gethRBPEmail().equals("")) {
								commonService.setNotificationRecipientsforNonEmployees(research.gethRBPEmail(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
							}
						}
					}
				}
			}
		}
		return dynamicEmailRecipients;
	}

	private WorkdayHRBusinessPartner getHRBPDetails(String supOrgId, Map<String, String> workdayConfigDetails,
			String awardNumber, Integer awardId, Integer workdayManpowerInterfaceId) {
		String apiUrl = new StringBuilder(workdayConfigDetails.get(Constants.WORKDAY_API))
				.append("HR/BusinessPartner/?").append("SupOrgID=").append(supOrgId).append("&").append(FORMAT_JSON)
				.toString();
		logger.info("Requesting for getWorkdayHRBusinessPartner {}", apiUrl);
		String requestMessage = new StringBuilder("SupOrgID=").append(supOrgId).toString();
		WorkdayHRBusinessPartner hRBP = null;
		try {
			ResponseEntity<String> response = callExternalCalls(apiUrl, workdayConfigDetails);
			logger.info("Response Status {}", response.getStatusCode());
			ObjectMapper objectMapper = new ObjectMapper();
			hRBP = objectMapper.readValue(response.getBody(), WorkdayHRBusinessPartner.class);
			if (hRBP != null && (hRBP.getReportEntries() != null && !hRBP.getReportEntries().isEmpty())) {
				logger.info("HRBP detail obtained for PISupOrgId = {}", supOrgId);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_SUCCESS,
						"HRBP detail received", 200, requestMessage, null, null, awardId, workdayManpowerInterfaceId);
				return hRBP;
			} else {
				logger.info("No HRBP detail obtained for PISupOrgId = " + supOrgId);
				saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_SUCCESS,
						"No HRBP detail received", 200, requestMessage, null, null, awardId, workdayManpowerInterfaceId);
				return hRBP;
			}
		} catch (Exception e) {
			e.printStackTrace();
			String errorMessage = new StringBuilder("Error in getWorkdayHRBusinessPartner :").append(e.getMessage())
					.toString();
			saveManpowerLog(awardNumber, Constants.MANPOWER_INTERFACE_HRBP, MESSAGE_TYPE_ERROR, errorMessage, 500,
					requestMessage, null, null, awardId, workdayManpowerInterfaceId);
			return hRBP;
		}
	}

	private Map<String, String> getFreezeMailDynamicPlaceholders(Timestamp newAwardEndDate, String positionIds) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{NEW_AWARD_END_DATE}",  newAwardEndDate != null ? commonService.convertDateFormatBasedOnTimeZone(newAwardEndDate.getTime(),Constants.DEFAULT_DATE_FORMAT) : "");
		placeHolder.put("{POSITION_ID}", positionIds);
		return placeHolder;
	}

	@Override
	public void prepareAndSendAllocationLessThanHundredMail(List<WorkdayManpowerInterface> allocationLessThanHundredInterfaces, StringBuilder allocationContent, Integer awardId) {
		logger.info("Received request for CostAllocationLessThanHunred mail sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_COST_ALLOCATION_LESS_THAN_HUNDRED_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setSubModuleCode("0");
		emailServiceVO.setModuleItemKey(awardId.toString());
		emailServiceVO.setSubModuleItemKey("0");
		emailServiceVO.setPlaceHolder(getCostAllocationDynamicPlaceholders(allocationContent.toString()));
		emailService.sendEmail(emailServiceVO);
		for (WorkdayManpowerInterface manpowerInterface : allocationLessThanHundredInterfaces) {
			saveWorkdayManpowerInterfaceWithInterfaceStatus(manpowerInterface, Constants.MANPOWER_INTERFACE_ERROR);
		}
	}

	private Map<String, String> getCostAllocationDynamicPlaceholders(String allocationContent) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{COST_ALLOCATION_CONTENT}",  allocationContent);
		return placeHolder;
	}

	public String getDecryptedSecretKeyAES() throws Exception {
		return excelityService.decryptAESKey(excelityService.getSftpConfigurationValueAsString(Constants.AES_SECRETE_KEY));
	}

	public String decryptAESData(String data, String secretKey) throws Exception {
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
			e.printStackTrace();
			logger.error("Error while AES Data decrypting:", e.getMessage());
		}
		return null;
	}

	@Override
	public void getManPowerDetails() {
		try {
			String secretKey = getDecryptedSecretKeyAES();
			logger.info(" Manpower details copying to temp table start at : {}", commonDao.getCurrentTimestamp());
			manpowerIntegrationDao.getAllManPowerDetails().stream().forEach(manpower -> {
				ManpowerTemp manpowerTemp = new ManpowerTemp();
				manpowerTemp.setManpowerPersonId(manpower.getManpowerPersonId());
				try {
					manpowerTemp.setCitizenship(manpower.getCitizenship() != null ? decryptAESData(manpower.getCitizenship(), secretKey): null);
					manpowerTemp.setNationality(manpower.getNationality() != null ? decryptAESData(manpower.getNationality(), secretKey): null);
				} catch (Exception e) {
					logger.info("Error Occurred while decrypting data: {} ", manpower.getManpowerPersonId());
				}
				manpowerIntegrationDao.saveManpowerTemp(manpowerTemp);
			});
			logger.info(" Manpower details copying to temp table ended at : {}", commonDao.getCurrentTimestamp());
		} catch (Exception e) {
			logger.error("Error Occurred in getManPowerDetails: {}", e.getMessage());
		}
	}

	@Override
	public Map<String, String> getWorkdayConfigDetails() {
		Map<String, String> configDetails = new HashMap<>();
		List <WorkdayConfigurationData> configurations = manpowerIntegrationDao.getWorkdayConfigurationData();
		configDetails = configurations.stream().collect(Collectors.toMap(configuration -> configuration.getConfigurationKey(), configuration -> configuration.getConfigurationValue()));
		logger.info("My Ip Adress from application properties {} ", appServerIp.toString());
//		configDetails.put("IP_ADDRESS", appServerIp.toString());
		InetAddress myIP;
		try {
			myIP = InetAddress.getLocalHost();
			if (myIP.getHostAddress() != null) {
				logger.info("My Ip Adress from Inet Address{} ", myIP.getHostAddress());
			} else {
				logger.info("My Ip Adress from Inet Address is null");
			}
		} catch (UnknownHostException e) {
			logger.error("Error due to UnknownHostException {}", e.getMessage());
		}
		return configDetails;
	}

	@Override
	public void getJobProfileChanges() {
		Map<String, String> configDetails = getWorkdayConfigDetails();
		String errorMessage = null;
		String requestMessage = null;
		String successMessage = null;
		try {
			requestMessage = new StringBuilder("Effective=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
			String apiUrl = new StringBuilder(configDetails.get(Constants.WORKDAY_API)).append("JobProfileChanges/?").append(FORMAT_JSON)
			.append("&effective=").append(convertTimestampToDate(commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE))).toString();
			logger.info("Requesting for getJobProfileChanges {} ", apiUrl);
			ResponseEntity<String> response = callExternalCalls(apiUrl, configDetails);
			logger.info("Response Status {}", response.getStatusCode());
			ObjectMapper objectMapper = new ObjectMapper();
			JobProfileChange jobProfileChange = objectMapper.readValue(response.getBody(), JobProfileChange.class);
			if (jobProfileChange != null && (jobProfileChange.getJobProfileChangeReportEntry() != null
					&& !jobProfileChange.getJobProfileChangeReportEntry().isEmpty())) {
				for (JobProfileChangeReportEntry jobChangeRE : jobProfileChange.getJobProfileChangeReportEntry()) {
					if (jobChangeRE.geteMPLOYEEID() != null) {
						logger.info("Employee Id {} ", jobChangeRE.geteMPLOYEEID());
					}
					WorkdayJobProfileChange workdayJobProfile = new WorkdayJobProfileChange();
					if (jobChangeRE.geteMPLOYEEID() != null) {
						workdayJobProfile.setPersonId(jobChangeRE.geteMPLOYEEID());
					}
					if (jobChangeRE.getjOBCODECURRENT() != null) {
						logger.info("Current Job Code {} ", jobChangeRE.getjOBCODECURRENT());
						workdayJobProfile.setJobCodeCurrent(jobChangeRE.getjOBCODECURRENT());
					}
					if (jobChangeRE.geteFFECTIVEDATE() != null) {
						logger.info("Effective Date {} ", jobChangeRE.geteFFECTIVEDATE());
						workdayJobProfile.setEffectiveDate(jobChangeRE.geteFFECTIVEDATE());
					}

					if (jobChangeRE.getjOBCODEPROPOSED() != null) {
						logger.info("Proposed Job Code {} ", jobChangeRE.getjOBCODEPROPOSED());
						workdayJobProfile.setJobCodeProposed(jobChangeRE.getjOBCODECURRENT());
					}
					saveJobProfileChange(workdayJobProfile);
				}
				successMessage = new StringBuilder("Job Profile Changes Received ").toString();
				logger.info(successMessage);
			} else {
				successMessage = new StringBuilder("No Job Profile Changes Received ").toString();
				logger.info(successMessage);
			}
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_DESIGNATION_CHANGE, MESSAGE_TYPE_SUCCESS, successMessage,
					200, requestMessage, null, null, null, null);
		} catch (RestClientResponseException ex) {
			errorMessage = new StringBuilder("RestClientError in getJobProfileChanges : ").append(ex.getRawStatusCode())
					.append(ex.getStatusText())
					.append(ex.getResponseBodyAsString() == null ? "" : ex.getResponseBodyAsString()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_DESIGNATION_CHANGE, MESSAGE_TYPE_ERROR, errorMessage,
					ex.getRawStatusCode(), requestMessage, null, null, null, null);
		} catch (Exception e) {
			e.printStackTrace();
			errorMessage = new StringBuilder("Error in getJobProfileChanges : ").append(e.getMessage()).toString();
			logger.error(errorMessage);
			saveManpowerLog(null, Constants.MANPOWER_INTERFACE_DESIGNATION_CHANGE, MESSAGE_TYPE_ERROR, errorMessage,
					500, requestMessage, null, null, null, null);
		}
	}

	private void saveJobProfileChange(WorkdayJobProfileChange workdayJobProfile) {
		try {
			workdayJobProfile.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			workdayJobProfile.setUpdateUser(UPDATE_USER);
			manpowerIntegrationDao.saveOrUpdateJobProfileChanges(workdayJobProfile);
		} catch (Exception e) {
			logger.error("Error in saveJobProfileChange{}", e);
		}
	}

	@Override
	public String getManpowerLookupSyncDetails() {
		List<ManpowerLookUpSyncDto> manpowerLookUpSyncDtos = new ArrayList<>();
		manpowerLookUpSyncDtos.add(setManpowerLookUpDetails(Constants.MANPOWER_INTERFACE_TERMINATION_AND_RESIGNATION));
		manpowerLookUpSyncDtos.add(setManpowerLookUpDetails(Constants.MANPOWER_INTERFACE_LONG_LEAVE));
		manpowerLookUpSyncDtos.add(setManpowerLookUpDetails(Constants.MANPOWER_INTERFACE_DESIGNATION_CHANGE));
		manpowerLookUpSyncDtos.add(setManpowerLookUpDetails(Constants.MANPOWER_INTERFACE_JOB_PROFILE));
		manpowerLookUpSyncDtos.add(setManpowerLookUpDetails(Constants.MANPOWER_INTERFACE_MANPOWER_DETAILS));
		manpowerLookUpSyncDtos.add(setManpowerLookUpDetails(Constants.MANPOWER_INTERFACE_CITIZENSHIP_NATIONALITY));
		return commonDao.convertObjectToJSON(manpowerLookUpSyncDtos);
	}

	private ManpowerLookUpSyncDto setManpowerLookUpDetails(String manpowerInterfaceTypeCode) {
		ManpowerLookUpSyncDto manpowerLookUpSyncDto = new ManpowerLookUpSyncDto();
		manpowerLookUpSyncDto.setManpowerInterfaceType(manpowerDao.fetchManpowerInterfaceById(manpowerInterfaceTypeCode));
		manpowerLookUpSyncDto.setInterfaceTypeCode(manpowerInterfaceTypeCode);
		Timestamp lastSyncedSuccessTime = manpowerIntegrationDao.getLastSyncedOnTime(manpowerInterfaceTypeCode, Constants.MANPOWER_MESSAGE_TYPE);
		manpowerLookUpSyncDto.setLastSyncedOn(lastSyncedSuccessTime);
		String lastSyncDate = lastSyncedSuccessTime.toString().substring(0, 11);
		manpowerLookUpSyncDto.setNoOfRecords(manpowerIntegrationDao.getNoOfRecords(manpowerInterfaceTypeCode, Timestamp.valueOf(lastSyncDate.concat(Constants.START_TIME)), Timestamp.valueOf(lastSyncDate.concat(Constants.END_TIME))));
		return manpowerLookUpSyncDto;
	}

	@Override
	public String getAwardTriggerDetails(ManpowerIntegrationVO vo) {
		vo = manpowerIntegrationDao.fetchManpowerTriggerDetails(vo);
		vo.setManpowerUserActions(manpowerIntegrationDao.fetchManpowerUserActions());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getPositionTriggerDetails(ManpowerIntegrationVO vo) {
		vo = manpowerIntegrationDao.fetchManpowerTriggerDetails(vo);
		vo.setManpowerUserActions(manpowerIntegrationDao.fetchManpowerUserActions());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getPositionErrorDetails(ManpowerIntegrationVO vo) {
		List<String> manpowerInterfaceStatuses = new ArrayList<>();
		List<String> interfaceTypeCodes = new ArrayList<>();
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_POSITION_CREATION);
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_HRBP);
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_FREEZE_POSITION);
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_MOVE_WORKERS);
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_COST_RECONCILATION);
		manpowerInterfaceStatuses.add(Constants.MANPOWER_INTERFACE_ERROR);
		manpowerInterfaceStatuses.add(Constants.MANPOWER_INTERFACE_RETRIGGERED);
		List<WorkdayInterfaceLogDto> workdayInterfaceLogDtos = manpowerIntegrationDao.fetchPositionErrorDetailsByParams(vo.getResourceUniqueId(), interfaceTypeCodes, manpowerInterfaceStatuses);
		vo.setWorkdayInterfaceLogDtos(workdayInterfaceLogDtos);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void sendNotificationForLessCostAllocation() {
		List<AwardManpowerResource> awardManpowerResources = manpowerIntegrationDao.getLessCostAllocationResources();
		if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
			Map<String, List<AwardManpowerResource>> awardManpowerResourceGroups = awardManpowerResources.stream()
					.collect(Collectors.groupingBy(resource -> resource.getDepartment()));
			for (Map.Entry<String, List<AwardManpowerResource>> entry : awardManpowerResourceGroups.entrySet()) {
				prepareWorkbookForResource(entry.getKey(), entry.getValue());
			}
		}
	}

	private void prepareWorkbookForResource(String department, List<AwardManpowerResource> manpowerResources) {
			try {
				if (manpowerResources != null && !manpowerResources.isEmpty()) {
					StringBuilder emailBody = new StringBuilder("");
					XSSFWorkbook workbook = new XSSFWorkbook();
					XSSFSheet sheet = workbook.createSheet("Report for manpower cost allocation less than 100 percent" + department);
					createManpowerReportByParams(manpowerResources, department, sheet, workbook);
					String fileName = new StringBuilder("Manpower cost allocation report ").append(department).append("_").append(createdDateWithTime()).append(".xlsx").toString();
					File report = commonService.createfileInUploads(workbook, fileName);
					sendNotificationForManpowerReport(manpowerResources, report, emailBody, department);
				}
			} catch (Exception e) {
				logger.info("error in prepareWorkbookForResource {}", e.getMessage());
			}
	}

	private void sendNotificationForManpowerReport(List<AwardManpowerResource> manpowerResources, File report, StringBuilder emailBody, String department) {
		try {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setFileName(report);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);	
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_COST_ALLOC_LESS_THAN_HUNDRED_SCHEDULER_NOTIFICATION_CODE);
		if (manpowerResources != null && !manpowerResources.isEmpty()) {
			emailServiceVO.setModuleItemKey(manpowerResources.get(0).getAwardId().toString());
		}
		emailServiceVO.setSubModuleItemKey("0");
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		if (!dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getRecipients() != null && !emailServiceVO.getRecipients().isEmpty() && emailServiceVO.getErrorMessage() == null && manpowerResources != null && !manpowerResources.isEmpty()) {
			manpowerResources.forEach(workday -> {
				WorkdayManpowerInterface workdayManpowerInterface = manpowerIntegrationDao.getWorkdayManpowerInterfaceById(workday.getWorkdayManpowerInterfaceId());
				if (workdayManpowerInterface != null) {
					saveWorkdayManpowerInterfaceWithMailFlag(workdayManpowerInterface, Constants.YES);
				}
			});
		}
		} catch(Exception e) {
			logger.info("error in sendNotificationForManpowerReport {}",e.getMessage());
		}
	}

	@Override
	public void saveWorkdayManpowerInterfaceWithMailFlag(WorkdayManpowerInterface workdayManpowerInterface, String mailFlag) {
		workdayManpowerInterface.setIsMailActive(mailFlag);
		workdayManpowerInterface.setUpdateUser(UPDATE_USER);
		workdayManpowerInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
		manpowerIntegrationDao.saveOrUpdateWorkdayManpowerInterface(workdayManpowerInterface);
	}

	private String createdDateWithTime() {
		String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(),
				Constants.FAST_INTEGRATION_LOG_DATE_FORMAT);
		SimpleDateFormat time = new SimpleDateFormat("HHmmss");
		time.setTimeZone(TimeZone.getTimeZone(timezone));
		String createdTime = time.format(new Date());
		return date + createdTime;
	}

	private String convertDateFormatBasedOnTimeZone(Long dateValue, String dateFormat) {
		Date date = new Date(dateValue);
		return new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
	}

	private void createManpowerReportByParams(List<AwardManpowerResource> manpowerResources, String department, XSSFSheet sheet, XSSFWorkbook workbook) {
		CommonVO commonVo = new CommonVO();
		commonVo.setDocumentHeading(new StringBuilder("Manpower cost allocation less than 100 percent Report for ").append(department).toString());
		List<Object[]> reportData = new ArrayList<>();
		manpowerResources.forEach(resource -> {
			Object[] object = new Object[16];
			object[0] = resource.getPersonId();
			object[1] = resource.getPositionId();
			object[2] = resource.getFullName();
			object[3] = resource.getChargeStartDate();
			object[4] = resource.getChargeEndDate();
			object[5] = resource.getCostAllocation();
			object[6] = resource.getBudgetReferenceNumber();
			object[7] = resource.getAwardNumber();
			object[8] = resource.getPlanStartDate();
			object[9] = resource.getPlanEndDate();
			object[10] = resource.getDescription();
			object[11] = resource.getIsRemainingCAFromWBS().equals(Boolean.TRUE) ? "Y" : "N";
			object[12] = resource.getUpdateTimestamp();
			object[13] = resource.getSubmitUser();
			object[14] = resource.getPiName();
			object[15] = resource.getLeadUnit();
			reportData.add(object);
		});
		Object[] tableHeadingRow = {"Person ID","Position ID","Full Name","Charge Start Date","Charge End Date","Cost Allocation","Budget Reference Number","Award Number","Plan Start Date","Plan End Date","Comment From Request","Fully Changed to WBS","Completion Date","UMO/Submit User","Principal Investigator","Lead Unit"};
		dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, commonVo);
	}

	@Override
	public String getCostAllocationTriggerDetails(ManpowerIntegrationVO vo) {
		List<String> interfaceTypeCodes = new ArrayList<>();
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER);
		interfaceTypeCodes.add(Constants.MANPOWER_INTERFACE_COST_ALLOCATION);
		vo = manpowerIntegrationDao.fetchManpowerTriggerDetails(vo);
		vo.setManpowerUserActions(manpowerIntegrationDao.fetchManpowerUserActions());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getCurrentCostAllocationDetails(ManpowerIntegrationVO vo) {
		vo.setWorkdayInterfaceLogDtos(manpowerIntegrationDao.getOtherCurrentCostAllocationDetails(vo.getPersonId(), vo.getResourceUniqueId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String updateManpowerInterfaceManually(ManpowerIntegrationVO vo) {
		WorkdayManpowerInterface workdayManpowerInterface = manpowerIntegrationDao.getWorkdayManpowerInterfaceById(vo.getWorkdayManpowerInterfaceId());
		workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_SUCCESS);
		workdayManpowerInterface.setComments(vo.getComments());
		workdayManpowerInterface.setManpowerUserActionCode(vo.getManpowerUserActionCode());
		workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
        workdayManpowerInterface.setUpdateUser(AuthenticatedUser.getLoginUserName());
        workdayManpowerInterface.setManpowerUserAction(manpowerIntegrationDao.fetchManpowerUserActionNameById(vo.getManpowerUserActionCode()));
        workdayManpowerInterface.setParentInterfaceStatus(manpowerIntegrationDao.getInterfaceErrorCountByResourceUniqueId(workdayManpowerInterface.getResourceUniqueId()) > 0 ? "Error" : "Success" );
		return commonDao.convertObjectToJSON(manpowerIntegrationDao.saveOrUpdateManpowerInterface(workdayManpowerInterface));
	}

}
