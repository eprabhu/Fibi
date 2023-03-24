package com.polus.fibicomp.negotiation.service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.service.AgreementService;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.medusa.dto.MedusaDTO;
import com.polus.fibicomp.negotiation.dao.NegotiationAgreementDao;
import com.polus.fibicomp.negotiation.dao.NegotiationDao;
import com.polus.fibicomp.negotiation.dto.AttachmentActivityDetail;
import com.polus.fibicomp.negotiation.dto.AttachmentData;
import com.polus.fibicomp.negotiation.dto.NegotiationDataBus;
import com.polus.fibicomp.negotiation.dto.NegotiationReportDto;
import com.polus.fibicomp.negotiation.pojo.NegotiationCommentAttachment;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementValue;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsComment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnel;
import com.polus.fibicomp.negotiation.vo.LastLocationDetails;
import com.polus.fibicomp.negotiation.vo.NegotiationActivityVO;
import com.polus.fibicomp.negotiation.vo.NegotiationAssociationVO;
import com.polus.fibicomp.negotiation.vo.NegotiationMode;
import com.polus.fibicomp.negotiation.vo.NegotiationVO;
import com.polus.fibicomp.notification.email.dao.EmailMaintenanceDao;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.OrganizationSearchResult;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Configuration
@Service(value = "negotiationService")
public class NegotiationServiceImpl implements NegotiationService {

	protected static Logger logger = LogManager.getLogger(NegotiationServiceImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private NegotiationDao negotiationDao;

	@Autowired
	private NegotiationAgreementDao negotiationAgreementDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EmailMaintenanceDao emailMaintenanceDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private BusinessRuleDao businessRuleDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private AgreementService agreementService;

	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private InboxService inboxService;

	@Override
	public String getLocation() {
		return commonDao.convertObjectToJSON(negotiationAgreementDao.getNegotiationsLocationTypes());
	}

	@Override
	public String getLocationById(NegotiationVO negotiationVO) {
		return commonDao
				.convertObjectToJSON(negotiationAgreementDao.getLocationByNegotiationId(negotiationVO.getNegotiationId()));
	}

	@Override
	public String addNegotiationActivity(NegotiationVO negotiationsActivityTypeVo) {
		Timestamp updateTimeStamp = commonDao.getCurrentTimestamp();
		NegotiationsActivity negotiationsActivity = negotiationsActivityTypeVo.getNegotiationsActivity();
		negotiationsActivity.setUpdateTimestamp(updateTimeStamp);
		if (negotiationsActivity.getCreateDate() == null) {
			negotiationsActivity.setCreateDate(updateTimeStamp);
		}
		return negotiationDao.addNegotiationActivity(negotiationsActivity);
	}

	@Override
	public String getActivityTypeList() {
		return commonDao.convertObjectToJSON(negotiationAgreementDao.getNegotiationsActivityTypes());
	}

	@Override
	public String getNegotiationAssociationTypeList() {
		NegotiationVO negotiationVO = new NegotiationVO();
		negotiationVO.setNegotiationsAssociationTypeList(negotiationAgreementDao.getNegotiationAssociationTypes());
		return commonDao.convertObjectToJSON(negotiationVO);
	}

	@Override
	public String saveNegotiationInfo(NegotiationVO negotiationVO) {
		Negotiations negotiation = new Negotiations();
		int nextSeqId = 0;
		Timestamp negotiationTimestamp = commonDao.getCurrentTimestamp();
		Negotiations negotiations = negotiationVO.getNegotiations();
		if (negotiations != null) {
			if (negotiations.getAcType() != null && negotiations.getAcType().equalsIgnoreCase(Constants.acTypeInsert)) {
				negotiations.setCreateTimeStamp(negotiationTimestamp);
				if (oracledb.equalsIgnoreCase("Y")) {
					String nextSequenceId = getNextSeq("SEQ_NEGOTIATIONS_ID");
					nextSeqId = Integer.parseInt(nextSequenceId);
					negotiation.setNegotiationId(nextSeqId);
				} else {
					String nextSequenceId = getNextSeqMySql(
							"SELECT ifnull(MAX(NEGOTIATION_ID),1000)+1 FROM NEGOTIATION");
					nextSeqId = Integer.parseInt(nextSequenceId);
					negotiation.setNegotiationId(nextSeqId);
				}
			} else {
				negotiation.setNegotiationId(negotiations.getNegotiationId());
			}
			negotiation.setUpdateTimeStamp(negotiationTimestamp);
			negotiation.setUpdateUser(negotiations.getUpdateUser());
			negotiation.setNegotiationStatusCode(negotiations.getNegotiationStatusCode());
			negotiation.setWorkflowStatusCode(negotiations.getWorkflowStatusCode());
			negotiation.setNegotiationsWorkflowStatus(negotiations.getNegotiationsWorkflowStatus());
			negotiation.setAgreementTypeCode(negotiations.getAgreementTypeCode());
			negotiation.setNegotiationsAgreementType(negotiations.getNegotiationsAgreementType());
			negotiation.setAssociatedProjectId(negotiations.getAssociatedProjectId());
			negotiation.setNegotiatorPersonId(negotiations.getNegotiatorPersonId());
			negotiation.setNegotiatorFullName(negotiations.getNegotiatorFullName());
			negotiation.setStartDate(negotiations.getStartDate());
			negotiation.setEndDate(negotiations.getEndDate());
			negotiation.setTotalBudgetAmount(negotiations.getTotalBudgetAmount());
			negotiation.setFinalContractDocument(negotiations.getFinalContractDocument());
			negotiation.setCreateUser(negotiations.getUpdateUser());
			negotiation.setCreateTimeStamp(negotiations.getCreateTimeStamp());
			negotiation.setSummaryComment(negotiations.getSummaryComment());
			negotiation.setLegalComment(negotiations.getLegalComment());
			negotiation.setNegotiatorComment(negotiations.getNegotiatorComment());
			negotiation.setNegotiationsActivities(negotiations.getNegotiationsActivities());
			negotiation.setNegotiationsAgreementValues(negotiations.getNegotiationsAgreementValues());
			negotiation.setNegotiationsAssociationDetails(negotiations.getNegotiationsAssociationDetails());
			negotiation.setNegotiationsAttachments(negotiations.getNegotiationsAttachments());
			negotiation.setNegotiationsPersonnels(negotiations.getNegotiationsPersonnels());
			negotiation.setNegotiationsAssociations(negotiation.getNegotiationsAssociations());
			negotiation.setAssociationsTypeCode(negotiation.getAssociationsTypeCode());
			negotiation.setProjectDetails(negotiations.getProjectDetails());
			negotiation.setTitle(negotiations.getTitle());
			negotiation = negotiationAgreementDao.saveNegotiationInfo(negotiation);
			getCreateUserFullName(negotiation);
			List<NegotiationsAssociation> negotiationsAssociations = negotiations.getNegotiationsAssociations();
			if (negotiationsAssociations != null && !negotiationsAssociations.isEmpty()) {
				String associationTypeCode = negotiationsAssociations.get(0).getAssociationTypeCode();
				negotiation.setAssociationsTypeCode(associationTypeCode);
			}
			negotiation.setNegotiationsStatus(negotiations.getNegotiationsStatus());
			negotiationVO.setNegotiations(negotiation);
			Integer negotiationId = negotiation.getNegotiationId();
			if (negotiationId != null) {
				negotiationVO.setLastLocationDetails(negotiationDao.getLastLocationDetails(negotiationId));
			}
			negotiationVO.setNegotiationMode(getNegotiationModeData(negotiation));
			String message;
			if (negotiations.getAcType() != null && negotiations.getAcType().equalsIgnoreCase("U")) {
				message = "Negotiation updated successfully";
			} else {
				message = "Negotiation saved successfully";
			}
			negotiationVO.setIsSubmit(Constants.NEGOTIATION_SHOW_SUBMIT_BUTTON);// this is hardcoded need to check the
																				// respective rights
			negotiationVO.setMessage(message);
			return commonDao.convertObjectToJSON(negotiationVO);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	private NegotiationMode getNegotiationModeData(Negotiations negotiation) {
		NegotiationMode negotiationMode = new NegotiationMode();
		if (negotiation.getNegotiationStatusCode().equalsIgnoreCase(Constants.NEGOTIATION_MODE_INPROGRESS)
				|| negotiation.getNegotiationStatusCode()
						.equalsIgnoreCase(Constants.NEGOTIATION_MODE_WAITING_FOR_RESPONSE)
				|| negotiation.getNegotiationStatusCode().equalsIgnoreCase(Constants.NEGOTIATION_MODE_SUSPENDED)) {
			negotiationMode.setMode(Constants.NEGOTIATION_EDIT_MODE);
		} else {
			negotiationMode.setMode(Constants.NEGOTIATION_VIEW_MODE);
		}
		negotiationMode.setStatus(negotiation.getNegotiationStatusCode());
		return negotiationMode;
	}

	private String getNextSeqMySql(String sql) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		Statement statement;
		int nextSeq = 0;
		try {
			statement = connection.createStatement();
			ResultSet rs = statement.executeQuery(sql);
			if (rs.next()) {
				nextSeq = rs.getInt(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return Integer.toString(nextSeq);
	}

	private String getNextSeq(String sequence) {
		logger.info("----------- this method generate id not auto incriment Service ------------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		Statement statement;
		int nextSeq = 0;
		try {
			statement = connection.createStatement();
			ResultSet rs = statement.executeQuery("select +" + sequence + ".nextval from dual");
			if (rs.next()) {
				nextSeq = rs.getInt(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return Integer.toString(nextSeq);
	}

	@Override
	public String loadNegotiation(NegotiationVO negotiationVO) {
		String personId = negotiationVO.getPersonId();
		Integer negotiationId = negotiationVO.getNegotiationId();
		if (negotiationId != null) {
			negotiationVO = negotiationDao.loadNegotiation(negotiationVO);
			List<NegotiationsAssociation> negotiationsAssociations = negotiationVO.getNegotiations().getNegotiationsAssociations();
			if (negotiationsAssociations != null && !negotiationsAssociations.isEmpty()) {
				String associationTypeCode = negotiationsAssociations.get(0).getAssociationTypeCode();
				negotiationVO.getNegotiations().setAssociationsTypeCode(associationTypeCode);
			}
			List<NegotiationsActivity> negotiationsActivitiesList = negotiationVO.getNegotiations()
					.getNegotiationsActivities();
			if (negotiationsActivitiesList != null && !negotiationsActivitiesList.isEmpty()) {
				for (NegotiationsActivity activity : negotiationsActivitiesList) {
					if (activity.getEndDate() != null && activity.getStartDate() != null) {
						Long noOfDays = commonDao.getNumberOfDays(activity.getStartDate(), activity.getEndDate());
						activity.setNoOfDays(noOfDays);
					}
					List<AttachmentData> attachmentDataList = new ArrayList<>();
					List<NegotiationsAttachment> attachmentList = negotiationAgreementDao.getAttachmentData(activity.getNegotiationsActivityId());
					if (attachmentList != null && !attachmentList.isEmpty()) {
						for (NegotiationsAttachment attachment : attachmentList) {
							AttachmentData data = new AttachmentData();
							data.setFileName(attachment.getFileName());
							data.setNegotiationAttachmentId(attachment.getNegotiationsAttachmentId());
							attachmentDataList.add(data);
						}
					}
					activity.setAttachmentDataList(attachmentDataList);
				}
			}
			negotiationVO.setLastLocationDetails(negotiationDao.getLastLocationDetails(negotiationId));
			negotiationVO.getNegotiations().setProjectDetails(negotiationDao.getDetailsFromProjectId(negotiationVO));
			negotiationVO.setProjectDetails(negotiationDao.getDetailsFromProjectId(negotiationVO));
			negotiationVO.setNegotiationsStatusList(negotiationAgreementDao.getAllNegotiationsStatus());
			negotiationVO.setNegotiationsAgreementTypeList(negotiationAgreementDao.getNegotiationsAgreementTypes());
			negotiationVO.setNegotiationsActivityTypeList(negotiationAgreementDao.getNegotiationsActivityTypes());
			negotiationVO.setNegotiationsAssociationTypeList(negotiationAgreementDao.getNegotiationAssociationTypes());
			negotiationVO.setNegotiationsAttachmentTypeList(negotiationAgreementDao.getNegotiationsAttachmentTypes());
			negotiationVO.setNegotiationsLocationTypeList(negotiationAgreementDao.getNegotiationsLocationTypes());
			negotiationVO.setNegotiationsPersonnelTypeList(negotiationAgreementDao.getNegotiationsPersonnelTypes());
			negotiationVO.setModuleCode(5);
			negotiationVO.setModuleItemKey(negotiationVO.getNegotiations().getNegotiationId().toString());
			negotiationVO.setPersonId(personId);
			negotiationVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(negotiationVO.getModuleItemKey(), personId, negotiationVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE));
			negotiationVO.setCanApproveRouting(businessRuleDao.canApproveRouting(negotiationVO.getModuleItemKey(), personId, negotiationVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE));
			if ((negotiationVO.getNegotiations().getNegotiationStatusCode().equals("1")) || (negotiationVO.getNegotiations().getNegotiationStatusCode().equals("2"))) {
				negotiationVO.setIsSubmit("1");
			}
			if (negotiationVO.getNegotiations().getNegotiationStatusCode().equals(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString()) || negotiationVO.getNegotiations().getNegotiationStatusCode().equals(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString())) {
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
				workflowService.prepareWorkflowDetails(workflow);
				negotiationVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					negotiationVO.setWorkflowList(workFlows);
				}
			}
			return commonDao.convertObjectToJSON(negotiationVO);
		} else {
			Negotiations negotiations = new Negotiations();
			negotiationVO.setNegotiations(negotiations);
			negotiationVO.setProjectDetails(negotiationDao.getDetailsFromProjectId(negotiationVO));
			negotiationVO.getNegotiations().setProjectDetails(negotiationDao.getDetailsFromProjectId(negotiationVO));
			negotiationVO.setNegotiationsStatusList(negotiationAgreementDao.getAllNegotiationsStatus());
			negotiationVO.setNegotiationsAgreementTypeList(negotiationAgreementDao.getNegotiationsAgreementTypes());
			negotiationVO.setNegotiationsActivityTypeList(negotiationAgreementDao.getNegotiationsActivityTypes());
			negotiationVO.setNegotiationsAssociationTypeList(negotiationAgreementDao.getNegotiationAssociationTypes());
			negotiationVO.setNegotiationsAttachmentTypeList(negotiationAgreementDao.getNegotiationsAttachmentTypes());
			negotiationVO.setNegotiationsLocationTypeList(negotiationAgreementDao.getNegotiationsLocationTypes());
			negotiationVO.setNegotiationsPersonnelTypeList(negotiationAgreementDao.getNegotiationsPersonnelTypes());
			LastLocationDetails lastLocationDetail = new LastLocationDetails();
			lastLocationDetail.setLastLocation("Not Available");
			negotiationVO.setLastLocationDetails(lastLocationDetail);
			NegotiationMode negotiationsMode = new NegotiationMode();
			negotiationsMode.setMode("CREATE");
			negotiationVO.setNegotiationMode(negotiationsMode);
			return commonDao.convertObjectToJSON(negotiationVO);
		}
	}

	@Override
	public String getNegotiationLocationHistory(NegotiationVO negotiationVO) {
		if (negotiationVO.getNegotiationId() != null) {
			int negotiationId = negotiationVO.getNegotiationId();
			NegotiationVO negotiationVo = negotiationDao.getNegotiationsLocationHistory(negotiationId);
			return commonDao.convertObjectToJSON(negotiationVo);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String setNegotiationLocation(NegotiationVO negotiationVO) {
		NegotiationsLocation negotiationsLocation = negotiationVO.getNegotiationsLocation();
		negotiationsLocation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		if (negotiationsLocation.getNegotiationLocationId() == null) {
			negotiationsLocation.setCreateTimestamp(commonDao.getCurrentTimestamp());
		}
		if (negotiationsLocation.getAssigneePersonId() != null) {
			negotiationsLocation.setPerson(personDao.getPersonDetailById(negotiationsLocation.getAssigneePersonId()));
		}
		negotiationsLocation = negotiationAgreementDao.saveOrUpdateNegotiationLocation(negotiationsLocation);
		Integer agreementRequestId = negotiationsLocation.getAgreementRequestId();
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementRequestId);
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS));
		agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS);
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		String assigneePersonId = negotiationsLocation.getAssigneePersonId();
		if (negotiationsLocation.getLocationStatusCode().equals(Constants.LOCATION_ASSIGNED)) {
			agreementService.addActionLogEntry(agreementRequestId, Constants.ACTION_LOG_LOCATION_ASSIGN, negotiationsLocation.getUpdateUser(), Constants.LOCATION_ASSIGN_START_MESSAGE + negotiationsLocation.getNegotiationsLocationType().getDescription() + Constants.LOCATION_ASSIGN_END_MESSAGE);
			if (negotiationsLocation.getAssigneePersonId() != null) {
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				commonService.setNotificationRecipients(negotiationsLocation.getAssigneePersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailrecipients);
				sendNotificationForAgreementNegotiation(negotiationVO, Constants.LOCATION_ASSIGN_NOTIFICATION_CODE, dynamicEmailrecipients);
			}
			String userMessage = "#" + agreementRequestId + " - " + agreementHeader.getTitle() + " - " + agreementHeader.getAgreementType().getDescription();
			inboxService.addAgreementMessageToInbox(agreementRequestId.toString(), assigneePersonId, negotiationsLocation.getUpdateUser(), Constants.MESSAGE_TYPE_LOCATION_ASSIGNMENT, "P", 0, Constants.AGREEMENT_SUBMODULE_CODE, userMessage);
		}
		if (negotiationsLocation.getLocationStatusCode().equals("3")) {
			agreementService.addActionLogEntry(agreementRequestId, Constants.ACTION_LOG_LOCATION_COMPLETE, negotiationsLocation.getUpdateUser(), negotiationsLocation.getNegotiationsLocationType().getDescription() + " review has been completed");
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			sendNotificationForAgreementNegotiation(negotiationVO, Constants.LOCATION_COMPLETE_NOTIFICATION_CODE, dynamicEmailrecipients);
			if (assigneePersonId != null) {
				inboxDao.markReadMessage(Constants.AGREEMENT_MODULE_CODE, agreementRequestId.toString(), negotiationsLocation.getAssigneePersonId(), Constants.MESSAGE_TYPE_LOCATION_ASSIGNMENT, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
			}
		}
		if (negotiationsLocation.getCreateUser() != null) {
			negotiationsLocation.setCreateUserFullName(personDao.getUserFullNameByUserName(negotiationsLocation.getCreateUser()));
		}
		if (negotiationsLocation.getUpdateUser() != null) {
			negotiationsLocation.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsLocation.getUpdateUser()));
		}
		negotiationVO.setNegotiationsLocations(agreementService.prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(negotiationsLocation.getNegotiationId())));
		return commonDao.convertObjectToJSON(negotiationVO);
	}

	@Override
	public String getLastLocationDetails(NegotiationVO negotiationVO) {
		NegotiationVO negotiationVo = new NegotiationVO();
		if (negotiationVO.getNegotiationId() != null) {
			int negotiationId = negotiationVO.getNegotiationId();
			negotiationVo.setLastLocationDetails(negotiationDao.getLastLocationDetails(negotiationId));
			return commonDao.convertObjectToJSON(negotiationVo);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String maintainNegotiationAgreement(NegotiationVO negotiationVO) {
		Timestamp timestamp = commonDao.getCurrentTimestamp();
		NegotiationsAgreementValue negotiationsAgreementValue = negotiationVO.getNegotiationsAgreementValue();
		if (negotiationsAgreementValue != null) {
			if (negotiationsAgreementValue.getAcType().equalsIgnoreCase(Constants.acTypeInsert) || negotiationsAgreementValue.getAcType().equalsIgnoreCase(Constants.acTypeUpdate)) {
				negotiationsAgreementValue.setUpdateTimestamp(timestamp);
				negotiationsAgreementValue.setAcType(Constants.acTypeUpdate);
				negotiationsAgreementValue = negotiationDao.addAgreementValuePeriod(negotiationsAgreementValue);
				return commonDao.convertObjectToJSON(negotiationsAgreementValue);
			} else {
				return negotiationDao.deleteAgreementValuePeriod(negotiationsAgreementValue);
			}
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String maintainNegotiationPerson(NegotiationVO negotiationVO) {
		NegotiationsPersonnel negotiationsPersonnel = negotiationVO.getNegotiationsPersonnel();
		if (negotiationsPersonnel != null) {
			if (negotiationsPersonnel.getAcType() != null) {
				if (negotiationsPersonnel.getAcType().equalsIgnoreCase(Constants.acTypeInsert) || negotiationsPersonnel.getAcType().equalsIgnoreCase(Constants.acTypeUpdate)) {
					negotiationsPersonnel.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					negotiationsPersonnel = negotiationDao.addPerson(negotiationsPersonnel);
					negotiationsPersonnel.setAcType(Constants.acTypeUpdate);
					return commonDao.convertObjectToJSON(negotiationsPersonnel);
				} else if (negotiationsPersonnel.getAcType().equalsIgnoreCase(Constants.acTypeDelete)) {
					return negotiationDao.deletePersonnel(negotiationsPersonnel);
				}
			}
			return "";
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String maintainNegotiationActivity(MultipartFile[] files, String formDataJson) {
		NegotiationActivityVO negotiationActivityVO = null;
		List<NegotiationsAttachment> negotiationsAttachments = new ArrayList<>();
		NegotiationsActivity newNegotiationsActivity = new NegotiationsActivity();
		Timestamp negotiationTimestamp = commonDao.getCurrentTimestamp();
		try {
			ObjectMapper mapper = new ObjectMapper();
			negotiationActivityVO = mapper.readValue(formDataJson, NegotiationActivityVO.class);
			NegotiationsActivity negotiationsActivity = negotiationActivityVO.getNegotiationActivity();
			if (negotiationsActivity != null) {
				negotiationsActivity.setUpdateUser(AuthenticatedUser.getLoginUserName());
				negotiationsActivity.setUpdateTimestamp(negotiationTimestamp);
				if (negotiationsActivity.getCreateDate() == null) {
					negotiationsActivity.setCreateDate(negotiationTimestamp);
				}
			}
			newNegotiationsActivity = negotiationAgreementDao.saveOrUpdateActivity(negotiationsActivity);
			@SuppressWarnings("unused")
			Boolean isReplaced = false;
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					NegotiationsAttachment attachment = new NegotiationsAttachment();
					attachment.setNegotiationId(newNegotiationsActivity.getNegotiationId());
					attachment.setNegotiationsActivityId(newNegotiationsActivity.getNegotiationsActivityId());
					if (negotiationsActivity != null && negotiationsActivity.getUpdateUser() != null) {
						attachment.setUpdateUser(negotiationsActivity.getUpdateUser());
					}
					attachment.setUpdateTimestamp(negotiationTimestamp);
					isReplaced = true;
					File file = new File(files[i].getOriginalFilename());
					FileData fileData = new FileData();
					fileData.setAttachment(files[i].getBytes());
					fileData = commonDao.saveFileData(fileData);
					attachment.setFileId(fileData.getFileDataId());
					attachment.setDocumentId(0);
					attachment.setFileName(file.getName());
					attachment.setContentType(files[i].getContentType());
					attachment = negotiationAgreementDao.saveAttachment(attachment);
					negotiationsAttachments.add(attachment);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		List<AttachmentData> attachmentDataList = new ArrayList<>();
		for (NegotiationsAttachment attachmentData : negotiationsAttachments) {
			AttachmentData data = new AttachmentData();
			data.setFileName(attachmentData.getFileName());
			data.setNegotiationAttachmentId(attachmentData.getNegotiationsAttachmentId());
			attachmentDataList.add(data);
		}
		if (newNegotiationsActivity.getAttachmentDataList() != null && !newNegotiationsActivity.getAttachmentDataList().isEmpty()) {
			List<AttachmentData> attachmentListInbound = newNegotiationsActivity.getAttachmentDataList();
			attachmentListInbound.addAll(attachmentDataList);
			newNegotiationsActivity.setAttachmentDataList(attachmentListInbound);
		} else {
			newNegotiationsActivity.setAttachmentDataList(attachmentDataList);
		}
		if (negotiationActivityVO != null && negotiationActivityVO.getNegotiationActivity() != null && 
				negotiationActivityVO.getNegotiationActivity().getStartDate() != null && negotiationActivityVO.getNegotiationActivity().getEndDate() != null) {
			negotiationActivityVO.getNegotiationActivity().setNoOfDays(commonDao.getNumberOfDays(negotiationActivityVO.getNegotiationActivity().getStartDate(),negotiationActivityVO.getNegotiationActivity().getEndDate()));
		}
		newNegotiationsActivity.setNegotiationsLocation(newNegotiationsActivity.getNegotiationLocationId() != null ? negotiationAgreementDao.getNegotiationLocationById(newNegotiationsActivity.getNegotiationLocationId()) : null);
		newNegotiationsActivity.setUpdateUserFullName(personDao.getUserFullNameByUserName(newNegotiationsActivity.getUpdateUser()));
		negotiationActivityVO.setNegotiationActivity(newNegotiationsActivity);
		return commonDao.convertObjectToJSON(negotiationActivityVO);
	}

	@Override
	public String getNegotiationsActivityById(NegotiationVO negotiationVO) {
		negotiationVO.setNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(negotiationVO.getNegotiationId()));
		negotiationVO.setNegotiationActivitysById(negotiationAgreementDao.getNegotiationsActivityByNegotiationId(negotiationVO.getNegotiationId()));
		List<NegotiationVO> negotiationVOList = new ArrayList<>();
		negotiationVOList.add(negotiationVO);
		return commonDao.convertObjectToJSON(negotiationVOList);
	}

	@Override
	public String addNegotiationsAssociationDetails(NegotiationVO negotiationVO) {
		if (negotiationVO.getNegotiationsAssociationDetailsList() != null) {
			List<NegotiationsAssociationDetails> negotiationsAssociationDetailsList = negotiationVO.getNegotiationsAssociationDetailsList();
			if (negotiationsAssociationDetailsList != null && !negotiationsAssociationDetailsList.isEmpty()) {
				for (NegotiationsAssociationDetails association : negotiationsAssociationDetailsList) {
					association.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					negotiationDao.addNegotiationsAssociationDetails(association);
				}
			}
			return commonDao.convertObjectToJSON("sucess");
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	private Integer getNextSequenceActivityId() {
		Integer nextActivitySequenceId = 0;
		if (oracledb.equalsIgnoreCase("Y")) {
			String nextSequenceId = getNextSeq("SEQ_NEGOTIATION_ACTIVITY_ID");
			logger.info("getNextSeq : {}", Integer.parseInt(nextSequenceId));
			nextActivitySequenceId = Integer.parseInt(nextSequenceId);
		} else {
			String nextSequenceId = getNextSeqMySql("SELECT max(NEGOTIATION_ACTIVITY_ID)+1 FROM NEGOTIATION_ACTIVITY");
			logger.info("getNextSeq : {}", Integer.parseInt(nextSequenceId));
			nextActivitySequenceId = Integer.parseInt(nextSequenceId);
		}
		return nextActivitySequenceId;
	}

	public NegotiationsAttachment addNegotiationsAttachment(NegotiationsAttachment attachment, MultipartFile files,
			String fileName, Integer documentId, Integer negotiationId) {
		NegotiationsAttachment negotiationsAttachment = new NegotiationsAttachment();
		try {
			negotiationsAttachment.setAttachmentTypeCode(attachment.getAttachmentTypeCode());
			negotiationsAttachment.setNegotiationsAttachmentType(attachment.getNegotiationsAttachmentType());
			negotiationsAttachment.setContentType(attachment.getContentType());
			negotiationsAttachment.setContentType(files.getContentType());
			negotiationsAttachment.setFileName(files.getOriginalFilename());
			negotiationsAttachment.setNegotiationId(attachment.getNegotiationId());
			negotiationsAttachment.setNegotiations(attachment.getNegotiations());
			negotiationsAttachment.setNegotiationsActivity(attachment.getNegotiationsActivity());
			negotiationsAttachment.setNegotiationsActivityId(attachment.getNegotiationsActivityId());
			negotiationsAttachment.setNegotiationsAttachmentId(attachment.getNegotiationsAttachmentId());
			negotiationsAttachment.setRestricted(attachment.getRestricted());
			negotiationsAttachment.setUpdateTimestamp(attachment.getUpdateTimestamp());
			negotiationsAttachment.setUpdateUser(attachment.getUpdateUser());
			negotiationsAttachment.setNegotiationId(negotiationId);
			FileData fileData = new FileData();
			fileData.setAttachment(files.getBytes());
			fileData = commonDao.saveFileData(fileData);
			negotiationsAttachment.setFileData(fileData);
			negotiationsAttachment.setDocumentId(attachment.getDocumentId());
			negotiationsAttachment.setFileName(attachment.getFileName());
			negotiationsAttachment.setFileId(fileData.getFileDataId());
		} catch (Exception e) {
			logger.error("Exception while adding Negotiations Attachment {}", e.getMessage());
		}
		return negotiationsAttachment;
	}

	@Override
	public String deleteAssociatedDetail(NegotiationVO negotiationVO) {
		NegotiationsAssociationDetails negotiationsAssociationDetail = negotiationVO.getNegotiationsAssociationDetail();
		if (negotiationsAssociationDetail != null) {
			return negotiationAgreementDao.deleteAssociatedDetail(negotiationsAssociationDetail);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String deleteActivity(NegotiationVO negotiationVO) {
		deleteAttachment(negotiationVO.getNegotiationsActivity().getNegotiationsActivityId());
		negotiationVO.getNegotiationsActivity().getAttachmentDataList().clear();
		NegotiationsActivity negotiationsActivity = negotiationVO.getNegotiationsActivity();
		negotiationVO = null;
		if (negotiationsActivity != null) {
			return negotiationAgreementDao.deleteActivity(negotiationsActivity);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public ResponseEntity<byte[]> downloadNegotiationAttachment(Integer attachmentId) {
		NegotiationsAttachment attachment = negotiationAgreementDao.fetchAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileId());
			byte[] data = fileData.getAttachment();
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType(attachment.getContentType()));
			String filename = attachment.getFileName();
			headers.setContentDispositionFormData(filename, filename);
			headers.setContentLength(data.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Exception while download Negotiations Attachment {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public ResponseEntity<byte[]> generateNegotiationReport(HttpServletResponse response, NegotiationDataBus negotiationDataBus) {
		ResponseEntity<byte[]> attachmentData = null;
		logger.info("-------- generateNegotiationReport serviceimpl ---------");
		try {
			byte[] data = negotiationDao.getTemplateData(negotiationDataBus);
			NegotiationReportDto negotiationReportDto = negotiationDao.fetchNegotiationsData(negotiationDataBus);
			byte[] mergedOutput = negotiationDao.mergePlaceHolders(negotiationDataBus.getOutputDataFormat(), data, negotiationReportDto);
			String generatedFileName = "Result" + System.nanoTime() + ".pdf";
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType("application/pdf"));
			headers.setContentDispositionFormData(generatedFileName, generatedFileName);
			headers.setContentLength(mergedOutput.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<>(mergedOutput, headers, HttpStatus.OK);
			response.setCharacterEncoding(StandardCharsets.UTF_8.name());
			response.setContentType("application/pdf");
			response.setContentLength(mergedOutput.length);
			response.setHeader("Content-Disposition", "attachment; filename=\"" + generatedFileName + "\"");
			FileCopyUtils.copy(mergedOutput, response.getOutputStream());
		} catch (Exception e) {
			logger.error("Exception in generateReport {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String addNegotiationsActivityDashboard(MultipartFile[] files, String formDataJSON) {
		NegotiationVO negotiationVO = null;
		Timestamp negotiationTimestamp = commonDao.getCurrentTimestamp();
		try {
			ObjectMapper mapper = new ObjectMapper();
			negotiationVO = mapper.readValue(formDataJSON, NegotiationVO.class);
			Integer nextActivitySequenceId = 0;
			nextActivitySequenceId = getNextSequenceActivityId();
			NegotiationsActivity negotiationsActivity = negotiationVO.getNegotiationsActivity();
			if (negotiationsActivity != null) {
				negotiationsActivity.setNegotiationsActivityId(nextActivitySequenceId);
				negotiationsActivity.setUpdateTimestamp(negotiationTimestamp);
				if (negotiationsActivity.getCreateDate() == null) {
					negotiationsActivity.setCreateDate(negotiationTimestamp);
				}
			}
			negotiationAgreementDao.saveOrUpdateActivity(negotiationsActivity);
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					NegotiationsAttachment attachment = negotiationVO.getNegotiationsAttachment();
					if (attachment != null) {
						attachment.setUpdateTimestamp(negotiationTimestamp);
						attachment.setNegotiationsActivityId(nextActivitySequenceId);
						if (negotiationsActivity != null && negotiationsActivity.getUpdateUser() != null) {
							attachment.setUpdateUser(negotiationsActivity.getUpdateUser());
						}
						attachment.setUpdateTimestamp(negotiationTimestamp);
						File file = new File(files[i].getOriginalFilename());
						FileData fileData = new FileData();
						fileData.setAttachment(files[i].getBytes());
						fileData = commonDao.saveFileData(fileData);
						attachment.setFileId(fileData.getFileDataId());
						attachment.setDocumentId(0);
						attachment.setFileName(file.getName());
						attachment.setContentType(files[i].getContentType());
						attachment.setDocumentId(0);
					}
					negotiationAgreementDao.saveAttachment(attachment);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "sucess";
	}

	@Override
	public String maintainNegotiationAssociation(NegotiationAssociationVO vo) {
		logger.info("Requesting for Negotiation Association service impl request body");
		String response = "";
		String associationTypeCodefromVO = vo.getAssociationTypeCode();
		if (Constants.acTypeDelete.equalsIgnoreCase(vo.getAcType())) {
			if (Constants.ASSOCIATION_NONETYPE.equals(vo.getAssociationTypeCode())) {
				logger.info("Requesting for Negotiation Association delete none case");
				if (vo.getNegotiationsAssociationDetails() != null) {
					NegotiationsAssociationDetails negoAssocDetail = vo.getNegotiationsAssociationDetails();
					negoAssocDetail.setUnit(null);
					negoAssocDetail.setSponsor(null);
					negoAssocDetail.setPrimeSponsor(null);
					negoAssocDetail.setContactAdminPerson(null);
					negoAssocDetail.setOrganization(null);
					response = negotiationAgreementDao.deleteNegotiationAssociationDetails(negoAssocDetail);
					List<NegotiationsAssociationDetails> negotiationAssociationNoneList = negotiationAgreementDao.fetchNegotiationAssociationDetailsBasedOnNegotiationAssociationId(negoAssocDetail.getNegotiationsAssociationId());
					if (negotiationAssociationNoneList != null && negotiationAssociationNoneList.isEmpty()) {
						negotiationAgreementDao.deleteNegotiationAssociation(negoAssocDetail.getNegotiationsAssociationId());
					}
					return commonDao.convertObjectToJSON(response);
				}
			} else {
				logger.info("Requesting for Negotiation Association delete ip/award case");
				if (vo.getNegotiationsAssociations() != null) {
					negotiationAgreementDao.deleteNegotiationAssociation(vo.getNegotiationsAssociations().getNegotiationsAssociationId());
					return commonDao.convertObjectToJSON("sucess");
				}
			}
		} else if ((Constants.acTypeInsert.equalsIgnoreCase(vo.getAcType())) || (Constants.acTypeUpdate.equalsIgnoreCase(vo.getAcType()))) {
			if (Constants.ASSOCIATION_NONETYPE.equals(vo.getAssociationTypeCode())) {
				logger.info("Requesting for Negotiation Association insert none case");
				NegotiationsAssociationDetails negotiationsAssociationDetails = vo.getNegotiationsAssociationDetails();
				NegotiationsAssociation negoAssociation = vo.getNegotiationsAssociations();
				if (negotiationsAssociationDetails != null) {
					negoAssociation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					negotiationsAssociationDetails.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					negoAssociation.setAssociationTypeCode(associationTypeCodefromVO);
					List<NegotiationsAssociation> associationList = negotiationDao.fetchAssociationData(negoAssociation.getNegotiationId());
					if (associationList != null && !associationList.isEmpty()) {
						vo.getNegotiationsAssociations().setNegotiationsAssociationId(associationList.get(0).getNegotiationsAssociationId());
						NegotiationsAssociation association = negotiationAgreementDao.saveNegotiationAssociation(vo.getNegotiationsAssociations());
						negotiationsAssociationDetails.setNegotiationsAssociationId(association.getNegotiationsAssociationId());
					} else if (associationTypeCodefromVO.equals("3")) {
						negoAssociation = negotiationAgreementDao.saveNegotiationAssociation(negoAssociation);
						negotiationsAssociationDetails.setNegotiationsAssociationId(negoAssociation.getNegotiationsAssociationId());
					}
					if (negotiationsAssociationDetails.getLeadUnit() != null) {
						negotiationsAssociationDetails.getUnit().setUnitNumber(negotiationsAssociationDetails.getLeadUnit());
					}
					if (negotiationsAssociationDetails.getSponsorCode() != null) {
						negotiationsAssociationDetails.getSponsor().setSponsorCode(negotiationsAssociationDetails.getSponsorCode());
					}
					if (negotiationsAssociationDetails.getPrimeSponsorCode() != null) {
						negotiationsAssociationDetails.getPrimeSponsor().setSponsorCode(negotiationsAssociationDetails.getPrimeSponsorCode());
					}
					if (negotiationsAssociationDetails.getContactAdminPersonId() != null) {
						negotiationsAssociationDetails.getContactAdminPerson().setPersonId(negotiationsAssociationDetails.getContactAdminPersonId());
					}
					if (negotiationsAssociationDetails.getSubAwardOrg() != null) {
						negotiationsAssociationDetails.getOrganization().setOrganizationId(negotiationsAssociationDetails.getSubAwardOrg());
					}
					negotiationAgreementDao.saveNegotiationAssociationDetails(negotiationsAssociationDetails);
					negotiationsAssociationDetails.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsAssociationDetails.getUpdateUser()));
				}
			} else {
				NegotiationsAssociation negotiationAssociation = vo.getNegotiationsAssociations();
				if (negotiationAssociation != null) {
					negotiationAssociation.setAssociationTypeCode(associationTypeCodefromVO);
					negotiationAssociation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					List<NegotiationsAssociation> associationList = negotiationDao.fetchAssociationData(negotiationAssociation.getNegotiationId());
					negotiationAssociation.setAssociationTypeCode(associationTypeCodefromVO);
					if (associationList != null && associationList.isEmpty()) {
						negotiationAgreementDao.saveNegotiationAssociation(negotiationAssociation);
					} else {
						negotiationAssociation.setNegotiationsAssociationId(Integer.parseInt(associationList.get(0).getNegotiationsAssociationId().toString()));
						negotiationAgreementDao.saveNegotiationAssociation(negotiationAssociation);
					}
				}
				vo.setProjectDetails(negotiationDao.getDetailsFromProjectId(negotiationAssociation));
			}
			vo.setAcType(Constants.acTypeUpdate);
		} else {
			return commonDao.convertObjectToJSON("error");
		}
		if (!(Constants.ASSOCIATION_NONETYPE.equals(associationTypeCodefromVO))) {
			negotiationDao.deleteNegotiationAssociationDetailsByAssociationId(vo.getNegotiationsAssociations().getNegotiationsAssociationId());
			if (vo.getNegotiationsAssociationDetails() != null) {
				negotiationDao.deleteNegotiationAssociationDetailsByAssociationId(vo.getNegotiationsAssociations().getNegotiationsAssociationId());
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getMedusaMoreDetails(MedusaDTO vo) {
		HashMap<String, Object> detailsMap = negotiationDao.getProjectDetailsSP(vo.getModuleName(), vo.getProjectNumber());
		if (detailsMap != null) {
			return commonDao.convertObjectToJSON(detailsMap);
		} else {
			return "";
		}
	}

	@Override
	public String deleteNegotiationAssociatedDetail(NegotiationAssociationVO vo) {
		NegotiationsAssociationDetails negotiationsAssociationDetail = vo.getNegotiationsAssociationDetails();
		if (negotiationsAssociationDetail != null) {
			return negotiationAgreementDao.deleteAssociatedDetail(negotiationsAssociationDetail);
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String deleteNegotiationAssociation(NegotiationAssociationVO vo) {
		NegotiationsAssociation negotiationsAssociation = vo.getNegotiationsAssociations();
		if (negotiationsAssociation != null) {
			return negotiationAgreementDao.deleteAssociations(negotiationsAssociation.getNegotiationsAssociationId());
		} else {
			return commonDao.convertObjectToJSON("failure");
		}
	}

	@Override
	public String deleteActivityAttachments(NegotiationActivityVO vo) {
		if (vo.getNegotiationsAttachmentId() != null) {
			negotiationDao.deleteAttachment(vo.getNegotiationsAttachmentId());
		}
		return commonDao.convertObjectToJSON("sucess");
	}

	@Override
	public String deleteAttachment(Integer negotiationsActivityId) {
		return negotiationAgreementDao.deleteActivityAttachments(negotiationsActivityId);
	}

	@Override
	public String submitNegotiation(NegotiationVO negotiationVO) {
		EvaluateValidationRuleVO evaluateValidationRuleVO = setRuleEvaluationVO(negotiationVO);
		List<WorkFlowResultDto> validationRule = businessRuleDao.evaluateValidationRule(evaluateValidationRuleVO);
		if (validationRule != null && !validationRule.isEmpty()) {
			negotiationVO.setValidationError(validationRule);
			return commonDao.convertObjectToJSON(negotiationVO);
		}
		Negotiations negotiation = negotiationVO.getNegotiations();
		buildWorkflow(negotiationVO, evaluateValidationRuleVO);
		fetchPreviousWorkFlowsList(negotiationVO);
		if (negotiation.getNegotiationStatusCode().equals(Constants.NEGOTIATION_STATUS_CODE_RETURNED.toString())) {
			inboxDao.markReadMessage(Constants.NEGOTIATION_MODULE_CODE, negotiation.getNegotiationId().toString(), negotiationVO.getPersonId(), Constants.MESSAGE_TYPE_NEGOTIATION_REJECT, Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
		}
		if (negotiationVO.getWorkflow().getWorkflowId() != null) {
			negotiation.setNegotiationStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString());
			negotiation.setNegotiationsStatus(negotiationAgreementDao.fetchStatusByStatusCode(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString()));
			if (negotiation.getEndDate() == null) {
				negotiation.setEndDate(commonDao.getCurrentTimestamp());
			}
			negotiation = negotiationAgreementDao.saveNegotiationInfo(negotiation);
		}
		loadNegotiation(negotiationVO);
		negotiationVO.setNegotiations(negotiation);
		negotiationVO.setModuleCode(Constants.NEGOTIATION_MODULE_CODE);
		String personId = negotiationVO.getLoginPersonId();
		negotiationVO.setModuleItemKey(negotiationVO.getNegotiations().getNegotiationId().toString());
		negotiationVO.setPersonId(personId);
		negotiationVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(negotiationVO.getModuleItemKey(),
				personId, negotiationVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY,
				Constants.NEGOTIATION_SUBMODULE_CODE));
		negotiationVO.setCanApproveRouting(businessRuleDao.canApproveRouting(negotiationVO.getModuleItemKey(), personId,
				negotiationVO.getModuleCode(), Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE));
		negotiationVO.setIsSubmit("0");
		NegotiationMode negotiationMode = new NegotiationMode();
		negotiationMode.setMode("VIEW");
		negotiationVO.setNegotiationMode(negotiationMode);
		negotiationVO.setMessage("submited successfully");
		negotiationVO.setLastLocationDetails(negotiationDao.getLastLocationDetails(negotiationVO.getNegotiations().getNegotiationId()));
		if (businessRuleDao.evaluateNotificationRule(evaluateValidationRuleVO) != null) {
			//Integer notificationId = Integer.parseInt(businessRuleDao.evaluateNotificationRule(evaluateValidationRuleVO));
		//	sendNegotiationNotification(negotiationVO, notificationId);
		}
		return commonDao.convertObjectToJSON(negotiationVO);
	}

	private NegotiationVO sendNegotiationNotification(NegotiationVO vo, Integer notificationTypeId) {
		NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(notificationTypeId);
		if (notificationType.getPromptUser().equals("Y")) {
			vo.setNotificationTypeId(notificationType.getNotificationTypeId());
		}
		return vo;
	}

	private EvaluateValidationRuleVO setRuleEvaluationVO(NegotiationVO negotiationVO) {
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(5);
		evaluateValidationRuleVO.setSubModuleCode(0);
		evaluateValidationRuleVO.setModuleItemKey(negotiationVO.getNegotiations().getNegotiationId().toString());
		//evaluateValidationRuleVO.setLoginPersonId(negotiationVO.getNegotiations().getNegotiatorPersonId());
		evaluateValidationRuleVO.setUpdateUser(negotiationVO.getNegotiations().getUpdateUser());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		return evaluateValidationRuleVO;
	}

	private NegotiationVO fetchPreviousWorkFlowsList(NegotiationVO negotiationVO) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(
				negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE,
				Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			negotiationVO.setWorkflowList(workFlows);
		}
		return negotiationVO;
	}

	private NegotiationVO buildWorkflow(NegotiationVO negotiationVO,
			EvaluateValidationRuleVO evaluateValidationRuleVO) {
		businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(
				negotiationVO.getNegotiations().getNegotiationId().toString(), Constants.NEGOTIATION_MODULE_CODE,
				Constants.SUBMODULE_ITEM_KEY, Constants.NEGOTIATION_SUBMODULE_CODE);
		negotiationVO.setWorkflow(workflow);
		return negotiationVO;
	}

	@Override
	public List<OrganizationSearchResult> findSubawardOrganisations(String searchString) {
		return negotiationDao.findSubawardOrganisations(searchString);
	}

	private void getCreateUserFullName(Negotiations negotiation) {
		if (negotiation.getCreateUser() != null) {
			negotiation.setCreateUserFullName(personDao.getUserFullNameByUserName(negotiation.getCreateUser()));
		}
	}

	@Override
	public String loadNegotiationAttachments(NegotiationVO vo) {
		List<NegotiationsAttachment> negotiationsAttachments = negotiationAgreementDao.loadNegotiationAttachments(vo.getNegotiationId());
		if (negotiationsAttachments != null && !negotiationsAttachments.isEmpty()) {
			for (NegotiationsAttachment negotiationsAttachment : negotiationsAttachments) {
				negotiationsAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsAttachment.getUpdateUser()));
			}
		}
		vo.setNegotiationAttachments(negotiationsAttachments);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void exportSelectedNegotiationAttachments(NegotiationVO vo, HttpServletResponse response) {
		List<Integer> attachmentIds = vo.getAttachmentIds();
		Negotiations negotiation = negotiationAgreementDao.fetchNegotiationById(vo.getNegotiationId());
		if (negotiation != null && attachmentIds != null && !attachmentIds.isEmpty()) {
			String fileName = "Negotitation_#"+vo.getNegotiationId() + "_attachments";
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<NegotiationsAttachment> attachments = negotiationAgreementDao.fetchNegotiationAttachmentBasedOnAttachmentIds(attachmentIds);
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ZipOutputStream zos = new ZipOutputStream(baos);
				if (attachments != null && !attachments.isEmpty()) {
					for (NegotiationsAttachment attachment : attachments) {
						zos.putNextEntry(new ZipEntry(attachment.getFileName()));
						FileData fileData = commonDao.getFileDataById(attachment.getFileId());
						byte[] data = fileData.getAttachment();
						zos.write(data);
					}
				}
				zos.closeEntry();
				zos.flush();
				baos.flush();
				zos.close();
				baos.close();
				ServletOutputStream op = response.getOutputStream();
				op.write(baos.toByteArray());
				op.flush();
				op.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

	}

	@SuppressWarnings("unused")
	@Override
	public String addNegotiationsAttachment(MultipartFile[] files, String formDataJSON) {
		NegotiationVO negotiationVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			negotiationVO = mapper.readValue(formDataJSON, NegotiationVO.class);
			List<NegotiationsAttachment> attachments = fetchNegotiationsAttachmentBasedOnNegotiationId(negotiationVO.getNegotiationId());
			Integer documentId = 0;
			if (attachments != null && !attachments.isEmpty()) {
				Collections.sort(attachments, (attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
								: attachment1.getDocumentId().equals(attachment2.getDocumentId()) ? 0 : 1);
				documentId = attachments.get(0).getDocumentId();
			}
			List<NegotiationsAttachment> newAttachments = negotiationVO.getNewAttachments();
			Integer versionNumber = 0;
			for (int i = 0; i < files.length; i++) {
				for (NegotiationsAttachment newAttachment : newAttachments) {
					String replaceFileName = newAttachment.getFileName();
					boolean isRenameRequired = false;
					int count = 1;
					isRenameRequired = checkForDuplication(newAttachment.getFileName(), attachments);
					while (isRenameRequired) {
						replaceFileName = newAttachment.getFileName();
						replaceFileName = generateFileName(replaceFileName, count);
						count = count + 1;
						isRenameRequired = checkForDuplication(replaceFileName, attachments);
					}
					if (newAttachment.getNegotiationsAttachmentId() != null) {
						for (NegotiationsAttachment attachment : attachments) {
							if (attachment.getNegotiationsAttachmentId() != null && attachment.getNegotiationsAttachmentId().equals(newAttachment.getNegotiationsAttachmentId())) {
								NegotiationsAttachment negotiationsAttachment = new NegotiationsAttachment();
								File file = new File(files[i].getOriginalFilename());
								String fileName = file.getName();
								documentId = attachment.getDocumentId();
								negotiationsAttachment = addNegotiationsAttachment(newAttachment, files[i], fileName, documentId, negotiationVO.getNegotiationId());
								negotiationsAttachment.setNegotiationId(negotiationVO.getNegotiationId());
								negotiationAgreementDao.saveOrUpdateNegotiationsAttachment(negotiationsAttachment);
							}
						}
					} else {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						if (newAttachment.getFileName().equals(fileName)) {
							documentId = documentId + 1;
							NegotiationsAttachment negotiationsAttachment = new NegotiationsAttachment();
							negotiationsAttachment = addNegotiationsAttachment(newAttachment, files[i], fileName,
									documentId, negotiationVO.getNegotiationId());
							negotiationAgreementDao.saveOrUpdateNegotiationsAttachment(negotiationsAttachment);
						}
					}
				}
			}
			negotiationVO.setNegotiationAttachments((fetchNegotiationsAttachmentBasedOnNegotiationId(negotiationVO.getNegotiationId())));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(negotiationVO);
	}

	private List<NegotiationsAttachment> fetchNegotiationsAttachmentBasedOnNegotiationId(Integer negotiationId) {
		List<NegotiationsAttachment> negotiationsAttachments = negotiationAgreementDao.fetchNegotiationsAttachmentBasedOnNegotiationId(negotiationId);
		if (negotiationsAttachments != null && !negotiationsAttachments.isEmpty()) {
			for (NegotiationsAttachment negotiationsAttachment : negotiationsAttachments) {
				negotiationsAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsAttachment.getUpdateUser()));
			}
		}
		return negotiationsAttachments;
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
	}

	private boolean checkForDuplication(String fileName, List<NegotiationsAttachment> attachments) {
		for (NegotiationsAttachment attachment : attachments) {
			if (fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String deleteNegotiationAttachment(NegotiationVO vo) {
		try {
			Negotiations negotiations = negotiationAgreementDao.fetchNegotiationById(vo.getNegotiationId());
			List<NegotiationsAttachment> negotiationsAttachments = negotiations.getNegotiationsAttachments();
			List<NegotiationsAttachment> updatedlist = new ArrayList<>(negotiationsAttachments);
			Collections.copy(updatedlist, negotiationsAttachments);
			if (negotiationsAttachments != null && !negotiationsAttachments.isEmpty()) {
				for (NegotiationsAttachment negotiationsAttachment : negotiationsAttachments) {
					if (negotiationsAttachment.getNegotiationsAttachmentId().equals(vo.getNegotiationAttachmentId())) {
						commonDao.deleteFileData(commonDao.getFileDataById(negotiationsAttachment.getFileId()));
						negotiationAgreementDao.deleteNegotiationAttachment(negotiationsAttachment);
						updatedlist.remove(negotiationsAttachment);
					}
				}
			}
			negotiations.getNegotiationsAttachments().clear();
			negotiations.getNegotiationsAttachments().addAll(updatedlist);
			negotiationAgreementDao.saveNegotiationInfo(negotiations);
			List<NegotiationsAttachment> negotiationsAttachmentList = negotiationAgreementDao.fetchNegotiationsAttachmentBasedOnNegotiationId(vo.getNegotiationId());
			for (NegotiationsAttachment negotiationsAttachment : negotiationsAttachmentList) {
				negotiationsAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsAttachment.getUpdateUser()));
			}
			vo.setNegotiationAttachments(negotiationsAttachmentList);
		} catch (Exception e) {
			vo.setMessage("Problem occurred in deleting proposal attachment");
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public NegotiationVO sendNegotiationNotification(NegotiationVO vo, Integer notificationTypeId,
			Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.NEGOTIATION_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.getNegotiations().getNegotiationId().toString());
		emailServiceVO.setPlaceHolder(getNegotiationPlaceholders(vo));
		emailServiceVO.setSubModuleCode((Constants.NEGOTIATION_SUBMODULE_CODE).toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && !emailServiceVO.getPrompted().equals(Boolean.TRUE)) {
			vo.setNotificationTypeId(notificationTypeId);
			vo.setBody(emailServiceVO.getBody());
			vo.setSubject(emailServiceVO.getSubject());
		}
		return vo;
	}

	private Map<String, String> getNegotiationPlaceholders(NegotiationVO vo) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{USER_NAME}", "");
		return placeHolder;
	}

	@Override
	public String deleteNegotiationLocation(NegotiationVO vo) {
		List<NegotiationsComment> negotiationsComments = negotiationAgreementDao.fetchNegotiationCommentBasedOnParams(vo.getNegotiationLocationId());
		if (negotiationsComments != null && !negotiationsComments.isEmpty() ) {
			negotiationsComments.forEach(negotiationsComment -> {
				if (negotiationsComment.getNegotiationCommentAttachment() != null && !negotiationsComment.getNegotiationCommentAttachment().isEmpty()) {
					for (NegotiationCommentAttachment negotiationCommentAttachment : negotiationsComment.getNegotiationCommentAttachment()) {
						//commonDao.deleteFileData(commonDao.getNegotiationCommentFileDataById(negotiationCommentAttachment.getNegotiationAttachmentFileId()));
					}
				}
				negotiationAgreementDao.deleteNegotiationsComment(negotiationsComment);
			});
		}
		deleteNegotiationActivityBasedOnLocation(vo.getNegotiationLocationId());
		negotiationAgreementDao.deleteNegotiationLocation(negotiationAgreementDao.getNegotiationLocationById(vo.getNegotiationLocationId()));
		vo.setNegotiationsLocations(agreementService.prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(vo.getNegotiationId())));
		return commonDao.convertObjectToJSON(vo);
	}

	private void deleteNegotiationActivityBasedOnLocation(Integer negotiationLocationId) {
		List<NegotiationsActivity> negotiationsActivities = negotiationAgreementDao.getNegotiationsActivityByLocationId(negotiationLocationId);
		if (negotiationsActivities != null && !negotiationsActivities.isEmpty()) {
			NegotiationVO vo = new NegotiationVO();
			negotiationsActivities.stream().forEach(negotiationsActivity -> {
				vo.setNegotiationsActivity(negotiationsActivity);
				deleteActivity(vo);
			});
		}
	}

	public NegotiationVO sendNotificationForAgreementNegotiation(NegotiationVO vo, Integer notificationTypeId,
			Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
		emailServiceVO.setModuleItemKey(vo.getNegotiationsLocation().getAgreementRequestId().toString());
		emailServiceVO.setPlaceHolder(getPlaceholders(vo));
		emailServiceVO.setSubModuleCode((Constants.AGREEMENT_SUBMODULE_CODE).toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && ! emailServiceVO.getPrompted().equals(Boolean.TRUE)) {
			vo.setNotificationTypeId(notificationTypeId);
			vo.setBody(emailServiceVO.getBody());
			vo.setSubject(emailServiceVO.getSubject());
		}
		return vo;
	}

	private Map<String, String> getPlaceholders(NegotiationVO vo) {
		Map<String, String> placeHolder = new HashMap<>();
		if (vo.getNegotiationsLocation().getAssigneePersonId() != null) {
			placeHolder.put("{ASSIGNEE}",
					personDao.getPersonFullNameByPersonId(vo.getNegotiationsLocation().getAssigneePersonId()));
		} else {
			placeHolder.put("{ASSIGNEE}", "");
		}
		return placeHolder;
	}

	@Override
	public void loadNegotiationUserFullNames(Negotiations negotiation) {
		if (negotiation.getCreateUser() != null) {
			negotiation.setCreateUserFullName(personDao.getUserFullNameByUserName(negotiation.getCreateUser()));
		}
		if (negotiation.getUpdateUser() != null) {
			negotiation.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiation.getUpdateUser()));
		}
		if (negotiation.getNegotiationsActivities() != null && !negotiation.getNegotiationsActivities().isEmpty()) {
			for (NegotiationsActivity negotiationsActivity : negotiation.getNegotiationsActivities()) {
				if (negotiationsActivity.getUpdateUser() != null) {
					negotiationsActivity.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsActivity.getUpdateUser()));
				}
			}
		}
		if (negotiation.getNegotiationsAttachments() != null && !negotiation.getNegotiationsAttachments().isEmpty()) {
			for (NegotiationsAttachment negotiationsAttachment : negotiation.getNegotiationsAttachments()) {
				if (negotiationsAttachment.getUpdateUser() != null) {
					negotiationsAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsAttachment.getUpdateUser()));
				}
			}
		}
		if (negotiation.getNegotiationsAssociationDetails() != null
				&& !negotiation.getNegotiationsAssociationDetails().isEmpty()) {
			for (NegotiationsAssociationDetails associationDetails : negotiation.getNegotiationsAssociationDetails()) {
				if (associationDetails.getUpdateUser() != null) {
					associationDetails.setUpdateUserFullName(personDao.getUserFullNameByUserName(associationDetails.getUpdateUser()));
				}
			}
		}
	}

	@Override
	public NegotiationMode getNegotiationMode(Negotiations negotiation) {
		NegotiationMode negotiationMode = new NegotiationMode();
		if (negotiation.getNegotiationStatusCode().equalsIgnoreCase(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString())
				|| negotiation.getNegotiationStatusCode().equalsIgnoreCase(Constants.NEGOTIATION_MODE_WAITING_FOR_RESPONSE)) {
			negotiationMode.setMode("VIEW");
		} else {
			negotiationMode.setMode("EDIT");
		}
		negotiationMode.setStatus(negotiation.getNegotiationStatusCode());
		return negotiationMode;
	}

	@Override
	public String getAttachmentActivityDetails(NegotiationVO vo) {
		NegotiationsLocation location = new NegotiationsLocation();
		NegotiationsActivity activity = negotiationAgreementDao.getNegotiationsActivityByActivityId(vo.getNegotiationActivityId());
		if(vo.getNegotiationLocationId() != null) {
			location = negotiationAgreementDao.getNegotiationLocationById(vo.getNegotiationLocationId());
		}
		return commonDao.convertObjectToJSON(prepareAttachmentActivityDetail(activity, location));
	}

	public AttachmentActivityDetail prepareAttachmentActivityDetail(NegotiationsActivity activity, NegotiationsLocation location) {
		AttachmentActivityDetail activityDetail = new AttachmentActivityDetail();
		activityDetail.setLocation(location.getNegotiationsLocationType() != null ? location.getNegotiationsLocationType().getDescription() : null);
		activityDetail.setLocationDescription(location.getDescription());
		activityDetail.setAssignee(location.getAssigneePersonId() != null? personDao.getPersonFullNameByPersonId(location.getAssigneePersonId()) : null);
		activityDetail.setAssignedOn(location.getCreateTimestamp());
		activityDetail.setStatus(location.getNegotiationLocationStatus() != null ? location.getNegotiationLocationStatus().getDescription() : null);
		activityDetail.setActivityType(activity.getNegotiationsActivityType().getDescription());
		activityDetail.setActivityComment(activity.getDescription());
		activityDetail.setStartDate(activity.getStartDate());
		activityDetail.setEndDate(activity.getEndDate());
		activityDetail.setFollowupDate(activity.getFollowupDate());
		return activityDetail;
	}

}
