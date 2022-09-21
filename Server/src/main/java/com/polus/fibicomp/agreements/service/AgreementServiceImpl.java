package com.polus.fibicomp.agreements.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.dto.AgreementClausesGroup;
import com.polus.fibicomp.agreements.dto.AgreementHistory;
import com.polus.fibicomp.agreements.dto.PlaceHolderDTO;
import com.polus.fibicomp.agreements.dto.QuestionnaireAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementActionLog;
import com.polus.fibicomp.agreements.pojo.AgreementActionType;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationDetail;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationLink;
import com.polus.fibicomp.agreements.pojo.AgreementAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentFile;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementClausesGroupMapping;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementNote;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementPlaceHolder;
import com.polus.fibicomp.agreements.pojo.AgreementReviewType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContact;
import com.polus.fibicomp.agreements.pojo.AgreementType;
import com.polus.fibicomp.agreements.pojo.AgreementTypeHistory;
import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.agreements.pojo.ClausesBank;
import com.polus.fibicomp.agreements.pojo.ClausesGroup;
import com.polus.fibicomp.agreements.vo.AgreementLinkModuleVO;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.agreements.vo.TemplateManagementVO;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.general.dao.GeneralInformationDao;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.inbox.vo.InboxVO;
import com.polus.fibicomp.negotiation.dao.NegotiationAgreementDao;
import com.polus.fibicomp.negotiation.dto.AttachmentData;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.person.service.PersonService;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.prereview.dao.PreReviewDao;
import com.polus.fibicomp.print.agreement.service.AgreementPrintService;
import com.polus.fibicomp.questionnaire.dao.QuestionnaireDAO;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

import fr.opensagres.poi.xwpf.converter.pdf.PdfConverter;
import fr.opensagres.poi.xwpf.converter.pdf.PdfOptions;

@Transactional
@Service(value = "agreementService")
public class AgreementServiceImpl implements AgreementService {

	protected static Logger logger = LogManager.getLogger(AgreementServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private QuestionnaireDAO questionnaireDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AuthorizationService authorizationService;

	@Autowired
	private InboxDao inboxDao;

	@Autowired
	private InboxService inboxService;

	@Autowired
	private AgreementPrintService agreementPrintService;

	@Autowired
	private AgreementCommentService agreementCommentService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	private BusinessRuleDao businessRuleDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private AgreementWorkflowService agreementWorkFlowService;

	@Autowired
	private PreReviewDao preReviewDao;

	@Autowired
	private GeneralInformationDao generalInformationDao;

	@Autowired
	private PersonService personService;

	@Autowired
	private NegotiationAgreementDao negotiationAgreementDao;
	
	@Autowired
	private RolodexDao rolodexDao;

	@Override
	public String addAgreementAttachment(MultipartFile[] files, String formDataJSON, HttpServletResponse response) {
		AgreementVO agreementVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			agreementVO = mapper.readValue(formDataJSON, AgreementVO.class);
			List<AgreementAttachment> attachments = new ArrayList<>();
			if (Boolean.TRUE.equals(agreementVO.getIsGeneratedAgreement())) {
				attachments = agreementDao.fetchAgreementAttachmentsBasedOnAgreementId(agreementVO.getAgreementRequestId(), true);
			} else {
				attachments = agreementDao.fetchAgreementAttachmentsBasedOnAgreementId(agreementVO.getAgreementRequestId(), false);
			}
			Integer documentId = 0;
			if (attachments != null && !attachments.isEmpty()) {
				Collections.sort(attachments, (attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
								: attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
				documentId = attachments.get(0).getDocumentId();
			}
			List<AgreementAttachment> newAttachments = agreementVO.getNewAttachments();
			Integer versionNumber = 0;
			for (int i = 0; i < files.length; i++) {
				for (AgreementAttachment newAttachment : newAttachments) {
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
					if (newAttachment.getAgreementAttachmentId() != null) {
						for (AgreementAttachment attachment : attachments) {
							if (attachment.getAgreementAttachmentId() != null && attachment.getAgreementAttachmentId()
									.equals(newAttachment.getAgreementAttachmentId())) {
								File file = new File(files[i].getOriginalFilename());
								String fileName = file.getName();
								attachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
								attachment.setDocumentStatus(
										commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
								versionNumber = attachment.getVersionNumber();
								documentId = attachment.getDocumentId();
								AgreementAttachment agreementAttachment = addNewAgreementAttachment(newAttachment, files[i], fileName, versionNumber, documentId, agreementVO.getAgreementRequestId(), replaceFileName);
								agreementAttachment.setAgreementRequestId(agreementVO.getAgreementRequestId());
								if (agreementVO.getComment() != null) {
									agreementAttachment.setDescription(agreementVO.getComment());
								}
								agreementDao.saveOrUpdateAgreementAttachment(agreementAttachment);
								if (attachment.getAgreementAttachmentTypeCode().equals(Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT)) {
									addActionLogEntry(agreementVO.getAgreementRequestId(), Constants.ACTION_LOG_REPLACE_AGREEMENT, agreementVO.getUpdateUser(), null);
									Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
									sendNotificationForAgreement(agreementVO,Constants.REPLACE_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
								}
							}
						}
					} else {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						if (newAttachment.getFileName().equals(fileName)) {
							if (newAttachment.getAgreementAttachmentTypeCode().equals(Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT)) {
								documentId = 0;
							} else {
								documentId = documentId + 1;
							}
							AgreementAttachment agreementAttachment = addNewAgreementAttachment(newAttachment, files[i],
									fileName, versionNumber, documentId, agreementVO.getAgreementRequestId(),
									replaceFileName);
							agreementAttachment = agreementDao.saveOrUpdateAgreementAttachment(agreementAttachment);
						}
						i++;
					}
				}
			}
			if (Boolean.TRUE.equals(agreementVO.getIsGeneratedAgreement())) {
				agreementVO.setAgreementAttachments(
						getAgreementAttachmentsByAgreementId(agreementVO.getAgreementRequestId(), true));
			} else {
				agreementVO.setAgreementAttachments(
						getAgreementAttachmentsByAgreementId(agreementVO.getAgreementRequestId(), false));
			}
			setAgreementUpdateUser(agreementVO);
		} catch (Exception e) {
			logger.error("Exception in addAgreementAttachment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(agreementVO);
	}

	private void setAgreementUpdateUser(AgreementVO agreementVO) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementVO.getAgreementRequestId());
		agreementHeader.setUpdateUser(AuthenticatedUser.getLoginUserName());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateAgreement(agreementHeader);
	}

	private List<AgreementAttachment> getAgreementAttachmentsByAgreementId(Integer agreementId, Boolean isGenerated) {
		List<AgreementAttachment> agreementAttachments = agreementDao
				.fetchAgreementAttachmentsBasedOnAgreementId(agreementId, isGenerated);
		if (agreementAttachments != null && !agreementAttachments.isEmpty()) {
			agreementAttachments.forEach(agreementAttachment -> {
				if (agreementAttachment.getUpdateUser() != null) {
					agreementAttachment.setUpdateUserFullName(
							personDao.getUserFullNameByUserName(agreementAttachment.getUpdateUser()));
				}
			});
		}
		return agreementAttachments;
	}

	private AgreementAttachment addNewAgreementAttachment(AgreementAttachment attachment, MultipartFile file,
			String fileName, Integer versionNumber, Integer documentId, Integer agreementId, String replacedFileName) {
		AgreementAttachment agreementAttachment = new AgreementAttachment();
		try {
			if (attachment.getFileName().equals(fileName)) {
				agreementAttachment.setAgreementAttachmentType(attachment.getAgreementAttachmentType());
				agreementAttachment.setAgreementAttachmentTypeCode(attachment.getAgreementAttachmentTypeCode());
				agreementAttachment.setDescription(attachment.getDescription());
				agreementAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				agreementAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
				agreementAttachment.setFileName(replacedFileName);
				agreementAttachment.setContentType(file.getContentType());
				agreementAttachment.setVersionNumber(versionNumber + 1);
				if (attachment.getAgreementAttachStatusCode() != null) {
					agreementAttachment.setAgreementAttachStatusCode(attachment.getAgreementAttachStatusCode());
				} else {
					agreementAttachment.setAgreementAttachStatusCode("I");
				}
				AgreementAttachmentFile fileData = new AgreementAttachmentFile();
				fileData.setFileData(file.getBytes());
				fileData = agreementDao.saveFileData(fileData);
				agreementAttachment.setAgreementAttachmentFileId(fileData.getAgreementAttachmentFileId());
				agreementAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
				agreementAttachment
						.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
				agreementAttachment.setDocumentId(documentId);
				agreementAttachment.setAttachment(file.getBytes());
				agreementAttachment.setAgreementRequestId(agreementId);
			}
		} catch (Exception e) {
			logger.error("Exception in addNewAgreementAttachment: {} ", e.getMessage());
		}
		return agreementAttachment;
	}

	private boolean checkForDuplication(String fileName, List<AgreementAttachment> attachments) {
		for (AgreementAttachment attachment : attachments) {
			if (fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
	}

	@Override
	public String createAgreement(String personId) {
		AgreementVO agreementVO = new AgreementVO();
		setAgreementHomeUnit(personId, agreementVO);
		agreementVO.getAgreementHeader().setAgreementStatusCode(Constants.AGREEMENT_STATUS_INPROGRESS);
		agreementVO.getAgreementHeader().setAgreementStatus(agreementDao.fetchAgreementStatusByStatusCode(Constants.AGREEMENT_STATUS_INPROGRESS));
		loadInitialData(agreementVO);
		return commonDao.convertObjectToJSON(agreementVO);
	}

	@Override
	public void setAgreementHomeUnit(String personId, AgreementVO agreementVO) {
		if (personId != null) {
			Person person = personDao.getPersonDetailById(personId);
			if (person != null && person.getHomeUnit() != null) {
				Unit unit = commonDao.getLeadUnitByUnitNumber(person.getHomeUnit());
				if (unit != null) {
					agreementVO.getAgreementHeader().setUnit(unit);
					agreementVO.getAgreementHeader().setUnitNumber(unit.getUnitNumber());
					agreementVO.getAgreementHeader().setUnitName(unit.getUnitName());
				}
			}
		}
	}

	@Override
	public void loadInitialData(AgreementVO agreementVO) {
		agreementVO.setAgreementStatuses(agreementDao.fetchAgreementStatusTypes());
		agreementVO.setAgreementTypes(agreementDao.fetchAgreementTypes());
		agreementVO.setAgreementCategories(agreementDao.fetchAllAgreementCategory());
		agreementVO.setCurrencies(commonDao.fetchCurrencyDetails());
		agreementVO.setAgreementAttachmentTypes(agreementDao.fetchAgreementAttachmentTypes());
		agreementVO.setNegotiationsPersonnelTypes(negotiationAgreementDao.getNegotiationsPersonnelTypes());
		agreementVO.setAgreementSponsorContactTypes(agreementDao.fetchAgreementSponsorContactTypes());
		agreementVO.setAgreementPeopleType(agreementDao.fetchAgreementPeopleTypes());
		agreementVO.setPreReviewClarifications(preReviewDao.fetchAllPreReviewSectionTypes("2"));
		agreementVO.setSponsorRoles(agreementDao.fetchSponsorRoles());
		agreementVO.setAgreementSponsorTypes(agreementDao.fetchAgreementSponsorTypes());
		agreementVO.setAgreementReviewTypes(agreementDao.fetchAgreementReviewTypes());
		agreementVO.setSponsorTypes(generalInformationDao.fetchSponsorTypeLookup());
		agreementVO.setPersons(personService.getPersonBasedOnRoleAndRight());
		agreementVO.setModuleList(commonDao.fetchModules());
		agreementVO.setAgreementAdminGroups(commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.AGREEMENT_MODULE_CODE));
	}

	@Override
	public String saveOrUpdateAgreement(AgreementVO agreementVO) {
		AgreementHeader agreementHeader = agreementVO.getAgreementHeader();
		List<AgreementAssociationDetail> agreementAssociationDetails = agreementVO.getAgreementAssociationDetails();
		String previousAgreementTypeCode = "";
		Boolean isFirstTimeCreation = false;
		if (agreementHeader.getAgreementRequestId() == null) {
			agreementHeader.setCreateTimeStamp(commonDao.getCurrentTimestamp());
			Person person = personDao.getPersonDetailById(personDao.getPersonIdByUserName(agreementHeader.getCreateUser()));
			agreementHeader.setRequestorName(person.getFullName());
			agreementHeader.setRequestorPersonId(person.getPersonId());
			isFirstTimeCreation = true;
			List<Integer> adminGroupIds = commonDao.getAdminGroupIdsBasedOnPersonId(person.getPersonId());
			if (adminGroupIds != null && !adminGroupIds.isEmpty()) {
				agreementHeader.setAdminGroupId(adminGroupIds.size() == 1 ? adminGroupIds.get(0) : null);
				agreementHeader.setAgreementAdminGroup(adminGroupIds.size() == 1 ? commonDao.getAdminGroupByGroupId(adminGroupIds.get(0)) : null);
			}
		} else {
			previousAgreementTypeCode = agreementDao.getAgreementTypeCodeByAgreementId(agreementHeader.getAgreementRequestId());
		}
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		if (agreementHeader.getUnitNumber() != null && agreementHeader.getUnit() != null) {
			agreementHeader.setUnitName(agreementHeader.getUnit().getUnitName());
		}
		/*if (agreementVO.getAgreementPeople() != null) {
			AgreementPeople agreementPeople = agreementVO.getAgreementPeople();
			agreementHeader.setAdminName(agreementPeople.getFullName());
			agreementHeader.setAdminPersonId(agreementPeople.getPersonId());
			updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS);
			agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS);
			agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS));
		}*/
		agreementHeader = agreementDao.saveOrUpdateAgreement(agreementHeader);
		AgreementAssociationDetail agreementAssociationDetail = new AgreementAssociationDetail();
		if (agreementAssociationDetails != null && !agreementAssociationDetails.isEmpty()) {
			agreementAssociationDetail = prepareAgreementAssociationDetail(agreementAssociationDetails.get(0).getModuleCode(), agreementAssociationDetails.get(0).getModuleItemKey());
			if ((agreementAssociationDetail.getPersonId() != null || agreementAssociationDetail.getRolodexId() != null) && Constants.IMPORT_PI_DETAILS) {
				prepareAndSaveAgreementPIDetails(agreementHeader.getAgreementRequestId(), agreementAssociationDetail.getPersonId(), agreementAssociationDetail.getRolodexId(), agreementHeader.getUpdateUser());
			}
			if (agreementAssociationDetail.getSponsorCode() != null && Constants.IMPORT_SPONSOR_DETAILS) {
				prepareAndSaveAgreementSponsor(agreementHeader.getAgreementRequestId(), agreementAssociationDetail.getSponsorCode(), agreementHeader.getUpdateUser());
			}
			if (agreementAssociationDetail.getContractAdminPersonId() != null && Constants.IMPORT_CA_DETAILS) {
				prepareAndSaveContractAdminDetails(agreementHeader.getAgreementRequestId(), agreementAssociationDetail.getContractAdminPersonId(), agreementHeader.getUpdateUser());
			}
			if (agreementAssociationDetail.getPrimeSponsorCode() != null && !agreementAssociationDetail.getPrimeSponsorCode().equals("") && Constants.IMPORT_PRIME_SPONSOR_DETAILS) {
				prepareAndSavePrimeSponsorDetails(agreementHeader.getAgreementRequestId(), agreementAssociationDetail.getPrimeSponsorCode(), agreementHeader.getUpdateUser());
			}
			Integer agreementRequestId = agreementHeader.getAgreementRequestId();
			agreementAssociationDetails.stream().forEach(associationDetail -> {
				AgreementAssociationLink moduleLink = new AgreementAssociationLink();
				moduleLink.setAgreementRequestId(agreementRequestId);
				moduleLink.setModuleCode(associationDetail.getModuleCode());
				moduleLink.setModuleItemKey(associationDetail.getModuleItemKey());
				moduleLink.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				moduleLink.setUpdateUser(agreementVO.getUpdateUser());
				agreementDao.saveOrUpdateAgreementAssociationLink(moduleLink);
			});
		}
		/*if (agreementVO.getAgreementPeople() != null) {
			AgreementActionType agreementActionType = agreementDao.fetchAgreementActionTypeById(Constants.ACTION_LOG_ASSIGNED_TO_ADMIN);
			String message = agreementActionType.getMessage();
			message = message.replace("{ADMIN_ONE}", agreementHeader.getAdminName());
			addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_ASSIGNED_TO_ADMIN, agreementHeader.getUpdateUser(), message);
		}*/
		loadAgreementUserFullNames(agreementHeader);
		List<AgreementClauses> agreementClauses = new ArrayList<>();
		if (Boolean.TRUE.equals(isFirstTimeCreation)) {
			addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_CREATED, agreementHeader.getUpdateUser(), null);
			agreementHeader.setNegotiationId(createNewNegotiation(agreementHeader).getNegotiationId());
			agreementClauses = addAgreementClausesBasedOnAgreementType(agreementHeader.getAgreementTypeCode(), agreementHeader.getAgreementRequestId(), agreementHeader.getUpdateUser());
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			agreementVO.setAgreementRequestId(agreementHeader.getAgreementRequestId());
			sendNotificationForAgreement(agreementVO, Constants.CREATE_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
			if (agreementHeader.getAdminPersonId() != null && agreementHeader.getAdminName() != null) {
				AgreementActionType agreementActionType = agreementDao.fetchAgreementActionTypeById(Constants.ACTION_LOG_ASSIGNED_TO_ADMIN);
				String message = agreementActionType.getMessage();
				message = message.replace("{ADMIN_ONE}", agreementHeader.getAdminName());
				addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_ASSIGNED_TO_ADMIN, agreementVO.getUpdateUser(), message);
			}
			if (agreementHeader.getAdminGroupId() != null) {
				Set<NotificationRecipient> dynamicEmailrecipientsAddress = new HashSet<>();
				String emailAddresses = commonDao.getAdminGroupEmailAddressByAdminGroupId(agreementHeader.getAdminGroupId());
				if (emailAddresses != null && !emailAddresses.isEmpty()) {
					List<String> recipients = Arrays.asList(emailAddresses.split(","));
					for (String recipient : recipients) {
						commonService.setNotificationRecipientsforNonEmployees(recipient, "TO", dynamicEmailrecipientsAddress);
					}								
				}
				sendNotificationForAgreement(agreementVO, Constants.AGREEMENT_ASSIGN_ADMIN_GROUP_NOTIFICATION_CODE, dynamicEmailrecipientsAddress);
			}
		} else {
			agreementHeader
					.setNegotiationId(getNegotiationIdBasedOnAgreementId(agreementHeader.getAgreementRequestId()));
			if (!previousAgreementTypeCode.equals(agreementHeader.getAgreementTypeCode())) {
				deleteAllAgreementClauses(agreementHeader.getAgreementRequestId());
				agreementClauses = addAgreementClausesBasedOnAgreementType(agreementHeader.getAgreementTypeCode(),
						agreementHeader.getAgreementRequestId(), agreementHeader.getUpdateUser());
				if (!(agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_INPROGRESS)
						|| (agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_RETURNED)))) {
					if (Boolean.TRUE.equals(agreementVO.getIsTypeChanged())) {
						addToAgreementTypeHistory(previousAgreementTypeCode, agreementHeader.getAgreementRequestId(),
								agreementHeader.getUpdateUser());
					}
				}
			} else {
				agreementClauses = agreementDao
						.fetchAgreementClausesBasedOnAgreementId(agreementHeader.getAgreementRequestId());
			}
		}
		loadPersonDetailsInAgreement(agreementHeader);
		agreementVO.setAgreementHeader(agreementHeader);
		agreementVO.setAgreementClausesGroup(prepareAgreementClauses(agreementClauses));
		//agreementVO.setAgreementComments(
		//		agreementCommentService.prepareAgreementComment(agreementHeader.getAgreementRequestId()));
		if (agreementHeader.getUnitNumber() != null) {
			loadAgreementRights(agreementVO, agreementHeader.getUnitNumber(), agreementHeader.getAgreementRequestId());
		} else {
			loadAgreementRights(agreementVO, null, agreementHeader.getAgreementRequestId());
		}
		agreementVO.setAgreementTypeCodes(prepareAgreementTypeHistory(agreementHeader.getAgreementRequestId()));
		List<AgreementAssociationLink> linkDetails = agreementDao.getExternalModuleLinkDetails(agreementHeader.getAgreementRequestId());
		agreementVO.setAgreementAssociationDetails(linkDetails != null ? getExternalModuleDetails(linkDetails) : new ArrayList<>());
		agreementVO.setAgreementPeoples(preparePeopleDetails(agreementDao.getAllAgreementPeople(agreementHeader.getAgreementRequestId())));
		agreementVO.setAgreementSponsors(getAgreementSponsors(agreementHeader.getAgreementRequestId()));
		return commonDao.convertObjectToJSON(agreementVO);
	}

	private void loadAgreementRights(AgreementVO agreementVO, String unitNumber, Integer agreementRequestId) {
		if (unitNumber != null) {
			agreementVO.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AGREEMENT_MODULE_CODE,
					agreementVO.getPersonId(), unitNumber, agreementRequestId));
		} else {
			agreementVO.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AGREEMENT_MODULE_CODE,
					agreementVO.getPersonId(), Constants.ROOT_UNIT, agreementRequestId));
		}
	}

	@Override
	public List<AgreementClausesGroup> prepareAgreementClauses(List<AgreementClauses> agreementClauses) {
		Set<Integer> groupCodes = new HashSet<>();
		List<AgreementClausesGroup> agreementClausesGroups = new ArrayList<>();
		if (agreementClauses != null && !agreementClauses.isEmpty()) {
			agreementClauses.forEach(agreementClause -> {
				groupCodes.add(agreementClause.getClausesGroupCode());
			});
			if (!groupCodes.isEmpty()) {
				groupCodes.forEach(groupCode -> {
					AgreementClausesGroup agreementClausesGroup = new AgreementClausesGroup();
					agreementClauses.forEach(agreementClause -> {
						if (groupCode.equals(agreementClause.getClausesGroupCode())) {
							agreementClausesGroup.setClausesGroupCode(groupCode);
							agreementClausesGroup.setClausesGroup(agreementClause.getClausesGroup());
							agreementClausesGroup.getClauses().put(agreementClause.getAgreementClauseId(),
									agreementClause.getClauses());
						}
					});
					agreementClausesGroups.add(agreementClausesGroup);
				});
			}
		}
		return agreementClausesGroups;
	}

	private void deleteAllAgreementClauses(Integer agreementRequestId) {
		List<AgreementClauses> agreementClauses = agreementDao
				.fetchAgreementClausesBasedOnAgreementId(agreementRequestId);
		if (agreementClauses != null && !agreementClauses.isEmpty()) {
			agreementClauses.forEach(agreementClause -> {
				agreementDao.deleteAgreementClauses(agreementClause);
			});
		}
	}

	@Override
	public List<AgreementClauses> addAgreementClausesBasedOnAgreementType(String agreementTypeCode,
			Integer agreementRequestId, String updateUser) {
		List<AgreementClausesGroupMapping> agreementClausesGroups = agreementDao
				.fetchAllClausesGroupBasedOnAgreementType(agreementTypeCode);
		List<AgreementClauses> agreementClausesData = new ArrayList<>();
		if (agreementClausesGroups != null && !agreementClausesGroups.isEmpty()) {
			agreementClausesGroups.forEach(agreementClausesGroup -> {
				if (agreementClausesGroup.getClausesGroup() != null) {
					ClausesGroup clausesGroup = agreementClausesGroup.getClausesGroup();
					if (clausesGroup.getClauses() != null && !clausesGroup.getClauses().isEmpty()) {
						clausesGroup.getClauses().forEach(clause -> {
							AgreementClauses agreementClauses = new AgreementClauses();
							agreementClauses.setAgreementRequestId(agreementRequestId);
							agreementClauses.setClauses(clause.getDescription());
							agreementClauses.setClausesGroupCode(clausesGroup.getClauseGroupCode());
							agreementClauses.setClausesGroup(clausesGroup);
							agreementClauses.setUpdateTimestamp(commonDao.getCurrentTimestamp());
							agreementClauses.setUpdateUser(updateUser);
							agreementClauses = agreementDao.saveOrUpdateAgreementClauses(agreementClauses);
							agreementClausesData.add(agreementClauses);
						});
					}
				}
			});
		}
		return agreementClausesData;
	}

	@Override
	public Integer getNegotiationIdBasedOnAgreementId(Integer agreementRequestId) {
		Integer negotiationId = null;
		List<NegotiationsAssociation> negotiationsAssociations = agreementDao
				.getNegotiationIdBasedOnAgreementId(agreementRequestId);
		if (negotiationsAssociations != null && !negotiationsAssociations.isEmpty()) {
			negotiationId = negotiationsAssociations.get(0).getNegotiationId();
		}
		return negotiationId;
	}

	@Override
	public Negotiations createNewNegotiation(AgreementHeader agreementHeader) {
		Negotiations negotiation = new Negotiations();
		negotiation.setNegotiationStatus(Constants.NEGOTIATION_STATUS_CODE_INPROGRESS);
		negotiation.setWorkflowStatusCode(Constants.NEGOTIATION_WORKFLOW_INPROGRESS);
		negotiation.setAgreementTypeCode(Constants.NEGOTIATION_STATUS_CODE_INPROGRESS);
		negotiation = negotiationAgreementDao.saveNegotiationInfo(negotiation);
		NegotiationsAssociation negotiationsAssociation = new NegotiationsAssociation();
		negotiationsAssociation.setAssociatedProjectId(agreementHeader.getAgreementRequestId().toString());
		negotiationsAssociation.setNegotiations(negotiation);
		negotiationsAssociation.setNegotiationId(negotiation.getNegotiationId());
		negotiationsAssociation.setAssociationTypeCode(Constants.AGREEMENT_ASSOCIATION_TYPE);
		negotiationsAssociation.setUpdateUser(agreementHeader.getUpdateUser());
		negotiationsAssociation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		negotiationAgreementDao.saveNegotiationAssociation(negotiationsAssociation);
		return negotiation;
	}

	@Override
	public AgreementActionLog addActionLogEntry(Integer agreementRequestId, String actionTypeCode, String updateUser, String message) {
		AgreementActionLog agreementActionLog = new AgreementActionLog();
		agreementActionLog.setAgreementRequestId(agreementRequestId);
		agreementActionLog.setActionTypeCode(actionTypeCode);
		AgreementActionType agreementActionType = agreementDao.fetchAgreementActionTypeById(actionTypeCode);
		agreementActionLog.setAgreementActionType(agreementActionType);
		if (message != null) {
			agreementActionLog.setDescription(message);
		} else {
			agreementActionLog.setDescription(agreementActionType.getMessage());
		}
		agreementActionLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementActionLog.setUpdateUser(updateUser);
		return agreementDao.createAgreementActionLog(agreementActionLog);
	}

	@Override
	public void loadAgreementUserFullNames(AgreementHeader agreementHeader) {
		if (agreementHeader.getCreateUser() != null) {
			agreementHeader.setCreateUserFullName(personDao.getUserFullNameByUserName(agreementHeader.getCreateUser()));
		}
		if (agreementHeader.getUpdateUser() != null) {
			agreementHeader.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementHeader.getUpdateUser()));
		}
		if (agreementHeader.getSubmitUser() != null) {
			agreementHeader.setSubmitUserFullName(personDao.getUserFullNameByUserName(agreementHeader.getSubmitUser()));
		}
	}

	@Override
	public String loadAgreementById(AgreementVO agreementVO) {
		Integer agreementRequestId = agreementVO.getAgreementRequestId();
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementRequestId);
		loadInitialData(agreementVO);
		loadPersonDetailsInAgreement(agreementHeader);
		agreementVO.setAgreementHeader(agreementHeader);
		if (agreementHeader.getUnitNumber() != null) {
			loadAgreementRights(agreementVO, agreementHeader.getUnitNumber(), agreementHeader.getAgreementRequestId());
		} else {
			loadAgreementRights(agreementVO, null, agreementHeader.getAgreementRequestId());
		}
		loadAgreementUserFullNames(agreementVO.getAgreementHeader());
		if (!agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_INPROGRESS)
				&& !agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_FINAL)
				&& !agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_TERMINATED)
				&& !agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_TRASFERRED)
				&& !agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_ABANDONED)) {
			getAllEditableSections(agreementVO);
		}
		List<AgreementAssociationLink> linkDetails = agreementDao.getExternalModuleLinkDetails(agreementVO.getAgreementRequestId());
		agreementVO.setAgreementAssociationDetails(linkDetails != null ? getExternalModuleDetails(linkDetails) : new ArrayList<>());
		agreementVO.getAgreementHeader().setNegotiationId(getNegotiationIdBasedOnAgreementId(agreementRequestId));
		agreementVO.setIsUserHaveReview(negotiationAgreementDao.getUserHaveReview(agreementVO.getAgreementHeader().getNegotiationId(), agreementVO.getPersonId()));
		agreementVO.setAgreementClausesGroup(prepareAgreementClauses(agreementDao.fetchAgreementClausesBasedOnAgreementId(agreementVO.getAgreementRequestId())));
		agreementVO.setAgreementSponsors(getAgreementSponsors(agreementRequestId));
		agreementVO.setAgreementAttachments(agreementDao.fetchAllAgreementAttachmentsBasedOnParams(agreementRequestId, Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT));
		if (!agreementVO.getAgreementAttachments().isEmpty()) {
			getUpdateUserFullName(agreementVO);
			agreementVO.setIsAgreementCreatedOnce(true);
		}
		//agreementVO.setAgreementComments(agreementCommentService.prepareAgreementComment(agreementRequestId));
		agreementVO.setNegotiationsLocations(prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(agreementHeader.getNegotiationId())));
		agreementVO.setAgreementPeoples(preparePeopleDetails(agreementDao.getAllAgreementPeople(agreementRequestId)));
		agreementVO.setAgreementTypeCodes(prepareAgreementTypeHistory(agreementRequestId));
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(agreementRequestId.toString(), Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		if (workflow != null) {
			workflowService.prepareWorkflowDetails(workflow);
			agreementVO.setWorkflow(workflow);
			List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(agreementRequestId.toString(),
					Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
			if (workFlows != null && !workFlows.isEmpty()) {
				workflowService.prepareWorkflowDetailsList(workFlows);
				Collections.sort(workFlows, new WorkflowComparator());
				agreementVO.setWorkflowList(workFlows);
			}
		}
		agreementWorkFlowService.canTakeRoutingAction(agreementVO);
		if (agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_ROUTING)) {
			Integer canApproveRouting = businessRuleDao.canApproveRouting(agreementRequestId.toString(),
					agreementVO.getPersonId(), Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
					Constants.AGREEMENT_SUBMODULE_CODE);
			agreementVO.setCanApproveRouting(canApproveRouting.toString());
			agreementVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(agreementRequestId.toString(),
					agreementVO.getPersonId(), Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
					Constants.AGREEMENT_SUBMODULE_CODE));
		}
		return commonDao.convertObjectToJSON(agreementVO);
	}

	@Override
	public Set<String> prepareAgreementTypeHistory(Integer agreementRequestId) {
		List<AgreementTypeHistory> agreementTypeHistories = agreementDao.getAgreementTypeHistory(agreementRequestId);
		Set<String> typeCodes = new HashSet<>();
		if (agreementTypeHistories != null && !agreementTypeHistories.isEmpty()) {
			agreementTypeHistories.forEach(agreementTypeHistory -> {
				typeCodes.add(agreementTypeHistory.getAgreementTypeCode());
			});
		}
		return typeCodes;
	}

	private void getUpdateUserFullName(AgreementVO agreementVO) {
		agreementVO.getAgreementAttachments().forEach(agreementAttachment -> {
			if (agreementAttachment.getUpdateUser() != null) {
				agreementAttachment.setUpdateUserFullName(
						personDao.getUserFullNameByUserName(agreementAttachment.getUpdateUser()));
			}
		});
	}

	@Override
	public void loadPersonDetailsInAgreement(AgreementHeader agreementHeader) {
		if (agreementHeader != null && agreementHeader.getRequestorPersonId() != null) {
			Person person = personDao.getPersonDetailById(agreementHeader.getRequestorPersonId());
			agreementHeader.setRequestorEmail(person.getEmailAddress());
			if (person.getHomeUnit() != null) {
				Unit unit = commonDao.getUnitByUnitNumber(person.getHomeUnit());
				if (unit != null) {
					agreementHeader.setRequestorDepartment(unit.getUnitName());
				}
			}
			agreementHeader.setRequestorPhoneNumber(person.getMobileNumber());
		}
	}

	@Override
	public List<AgreementSponsor> getAgreementSponsors(Integer agreementRequestId) {
		List<AgreementSponsor> agreementSponsors = new ArrayList<>();
		agreementSponsors = agreementDao.getAgreementSponsors(agreementRequestId);
		if (agreementSponsors != null && !agreementSponsors.isEmpty()) {
			agreementSponsors.forEach(agreementSponsor -> {
				List<AgreementSponsorContact> agreementSponsorContacts = agreementDao
						.getAgreementSponsorContacts(agreementSponsor.getAgreementSponsorId());
				agreementSponsor.getAgreementSponsorContacts().addAll(agreementSponsorContacts);
			});
		}
		return agreementSponsors;
	}

	@Override
	public String deleteAgreementAttachment(AgreementVO vo) {
		List<AgreementAttachment> agreementAttachments = agreementDao
				.fetchAgreementAttachmentsBasedOnAgreementIdAndDocumentId(vo.getAgreementRequestId(),
						vo.getDocumentId());
		if (agreementAttachments != null && !agreementAttachments.isEmpty()) {
			agreementAttachments.forEach(agreementAttachment -> {
				agreementDao.deleteFileData(
						agreementDao.getFileDataById(agreementAttachment.getAgreementAttachmentFileId()));
				agreementDao.deleteAgreementAttachments(agreementAttachment);
			});
		}
		setAgreementUpdateUser(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<byte[]> downloadAgreementAttachment(Integer attachmentId, String exportType) {
		AgreementAttachment attachment = agreementDao.fetchAttachmentById(attachmentId);
		//AgreementHeader agreementHeader = agreementDao.getAgreementById(attachment.getAgreementRequestId());
		ResponseEntity<byte[]> attachmentData = null;
		try {
			AgreementAttachmentFile fileData = agreementDao.getFileDataById(attachment.getAgreementAttachmentFileId());
			byte[] data = fileData.getFileData();
			String filename = attachment.getFileName();
			HttpHeaders headers = new HttpHeaders();
			if (exportType.equals("pdf") && !attachment.getContentType().equals("application/pdf")) {
				InputStream isFromFirstData = new ByteArrayInputStream(data);
				XWPFDocument document = new XWPFDocument(isFromFirstData);
				PdfOptions options = PdfOptions.create();
				ByteArrayOutputStream pdf = new ByteArrayOutputStream();
				PdfConverter.getInstance().convert(document, pdf, options);
				document.write(pdf);
				document.close();
				byte[] datas = pdf.toByteArray();
//				if (exportType.equals("pdf")) {
//					datas = watermarkService.generateTimestampAndUsernameForPdf(datas, agreementHeader, attachment);
//				}
				headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
				headers.setContentLength(datas.length);
				headers.setContentDispositionFormData("ab.pdf", filename);
				headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
				headers.setPragma("public");
				attachmentData = new ResponseEntity<>(datas, headers, HttpStatus.OK);
			} else {
//				if (exportType.equals("docx")) {
//					data = watermarkService.generateTimestampAndUsernameForDocx(data, agreementHeader, attachment);
//				}
				headers.setContentType(MediaType.parseMediaType(attachment.getContentType()));
				headers.setContentDispositionFormData(filename, filename);
				headers.setContentLength(data.length);
				headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
				headers.setPragma("public");
				attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
			}
		} catch (Exception e) {
			logger.error("Exception in downloadAgreementAttachment: {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String saveOrUpdateOrganisation(AgreementVO vo) {
		AgreementSponsor agreementSponsor = vo.getAgreementSponsor();
		if (agreementSponsor.getSponsorCode() != null) {
			Sponsor sponsor = commonDao.getSponsorById(agreementSponsor.getSponsorCode());
			if (sponsor != null) {
				agreementSponsor.setSponsor(sponsor);
			}
		}
		if (agreementSponsor.getAgreementSponsorContacts() != null && !agreementSponsor.getAgreementSponsorContacts().isEmpty()) {
			agreementSponsor.getAgreementSponsorContacts().clear();
		}
		agreementSponsor.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		setAgreementUpdateUser(vo);
		agreementDao.saveOrUpdateAgreementSponsor(agreementSponsor);
		vo.setAgreementSponsors(getAgreementSponsors(agreementSponsor.getAgreementRequestId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAgreementAttachments(AgreementVO vo) {
		AgreementHeader agreement = agreementDao.getAgreementById(vo.getAgreementRequestId());
		vo.setAgreementAttachments(getAgreementAttachmentsByAgreementId(vo.getAgreementRequestId(), false));
		vo.setNegotiationsAttachments(getNegotiationsActivityAttachment(vo.getNegotiationId()));
		List<QuestionnaireAttachment> questionnnaireAttachments = new ArrayList<>();
		if (agreement != null && agreement.getAgreementTypeCode() != null) {
			List<Object[]> objects = agreementDao.getQuestionnaireAttachments(Constants.AGREEMENT_MODULE_CODE,
					Integer.parseInt(agreement.getAgreementTypeCode()), vo.getAgreementRequestId().toString());
			objects.forEach(obj -> {
				QuestionnaireAttachment attachment = new QuestionnaireAttachment();
				if ((Integer) obj[0] != null) {
					attachment.setAnswerAttachmentId((Integer) obj[0]);
				}
				if ((String) obj[1] != null) {
					attachment.setFileName((String) obj[1]);
				}
				if ((String) obj[2] != null) {
					attachment.setContentType((String) obj[2]);
				}
				if ((String) obj[3] != null) {
					attachment.setUpdateUser(personDao.getUserFullNameByUserName((String) obj[3]));
				}
				if ((Timestamp) obj[4] != null) {
					attachment.setUpdateTimestamp((Timestamp) obj[4]);
				}
				questionnnaireAttachments.add(attachment);
			});
		}
		vo.setQuestionnnaireAttachment(questionnnaireAttachments);
		return commonDao.convertObjectToJSON(vo);
	}

	private List<NegotiationsAttachment> getNegotiationsActivityAttachment(Integer negotiationId) {
		List<NegotiationsAttachment> negotiationAttachments = new ArrayList<>();
		List<NegotiationsActivity> negotiationsActivities = negotiationAgreementDao.getNegotiationsActivityByNegotiationId(negotiationId);
		if (negotiationsActivities != null && !negotiationsActivities.isEmpty()) {
			negotiationsActivities.stream().forEach(negotiationsActivity -> {
				List<NegotiationsAttachment> attachmentList = negotiationAgreementDao.getAttachmentData(negotiationsActivity.getNegotiationsActivityId());
				if (attachmentList != null && !attachmentList.isEmpty()) {
					attachmentList.parallelStream().forEach( attachment -> {
						attachment.setNegotiationsActivity(negotiationsActivity);
					});
					negotiationAttachments.addAll(attachmentList);
				}
			});
		}
		if (!negotiationAttachments.isEmpty()) {
			negotiationAttachments.parallelStream().forEach(attachments -> {
				if (attachments.getUpdateUser() != null) {
					attachments.setUpdateUserFullName(personDao.getUserFullNameByUserName(attachments.getUpdateUser()));
				}
			});
		}
		return negotiationAttachments;
	}

	@Override
	public String submitAgreement(AgreementVO vo) {
		Integer agreementRequestId = vo.getAgreementRequestId();
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		String agreementStatusCode = agreementHeader.getAgreementStatusCode();
		AgreementActionLog agreementActionLog = new AgreementActionLog();
		loadPersonDetailsInAgreement(agreementHeader);
		addToAgreementTypeHistory(agreementHeader.getAgreementTypeCode(), agreementHeader.getAgreementRequestId(), vo.getUpdateUser());
		agreementHeader.setUpdateUser(vo.getUpdateUser());
		agreementHeader.setSubmitUser(vo.getUpdateUser());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_ROUTING);
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementHeader.getAgreementRequestId().toString(), Constants.MESSAGE_TYPE_RETURNED);
		buildAgreementWorkflow(vo);
		fetchPreviousWorkFlowsList(vo);
		Workflow workflow = workflowDao.fetchActiveWorkflowByParams(agreementRequestId.toString(), Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		workflowService.prepareWorkflowDetails(workflow);
		vo.setWorkflow(workflow);
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(agreementRequestId.toString(), Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			vo.setWorkflowList(workFlows);
		}
		if (vo.getWorkflow() != null && vo.getWorkflow().getWorkflowDetails().isEmpty() || vo.getWorkflow() == null) {
			if (agreementStatusCode.equals(Constants.AGREEMENT_STATUS_RETURNED) && agreementHeader.getAdminPersonId() != null) {
				updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS);
				agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS);
				agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS));
			} else {
				if (agreementHeader.getAdminPersonId() != null && agreementHeader.getAdminName() != null) {
					updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS);
					agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS);
					agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS));
				} else {
					updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_SUBMITTED);
				}
			}
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			String submitToEmail = commonDao.getParameterValueAsString(Constants.SUBMIT_AGREEMENT_EMAIL);
			commonService.setNotificationRecipientsforNonEmployees(submitToEmail, "TO", dynamicEmailrecipients);
			sendNotificationForAgreement(vo, Constants.SUBMIT_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
			agreementActionLog = addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SUBMITTED, agreementHeader.getUpdateUser(), null);
			String userMessage = "#" + agreementHeader.getAgreementRequestId().toString() + " - " + agreementHeader.getTitle() + " - " + agreementHeader.getAgreementType().getDescription();
			Set<String> personIds = new HashSet<>();
			if (agreementHeader.getAdminPersonId() == null) {
				List<PersonRoleRT> personRoles = new ArrayList<>();
				if (agreementHeader.getUnitNumber() != null) {
					personRoles = personDao.fetchPersonRoleRTByRightNameAndUnitNumber(agreementHeader.getUnitNumber(), "AGREEMENT_ADMINISTRATOR");
				} else {
					personRoles = personDao.fetchPersonRoleRTByRightNameAndUnitNumber(Constants.ROOT_UNIT, "AGREEMENT_ADMINISTRATOR");
				}
				if (personRoles != null && !personRoles.isEmpty()) {
					personRoles.forEach(personRole -> {
						personIds.add(personRole.getPersonRoleRTAttributes().getPersonId());
					});
					personIds.forEach(personId -> {
						inboxService.addAgreementMessageToInbox(agreementHeader.getAgreementRequestId().toString(),
								personId, vo.getUpdateUser(), Constants.MESSAGE_TYPE_SUBMITTED, "P", 0,
								Constants.AGREEMENT_SUBMODULE_CODE, userMessage);
					});
				}
			} else {
				if (agreementHeader.getAdminGroupId() != null) {
					personIds.addAll(personDao.getGroupAdminPersonIdsByRightName(Constants.ADMIN_GROUP_RIGHT, agreementHeader.getAdminGroupId()));
					if (!personIds.contains(agreementHeader.getAdminPersonId())) {
						personIds.add(agreementHeader.getAdminPersonId());
					}
				} else {
					personIds.add(agreementHeader.getAdminPersonId());
				}
				if (personIds != null && !personIds.isEmpty()) {
					personIds.forEach(personId -> {
						inboxService.addAgreementMessageToInbox(agreementHeader.getAgreementRequestId().toString(),
								personId, vo.getUpdateUser(), Constants.MESSAGE_TYPE_LOCATION_ASSIGNMENT, "P", 0,
								Constants.AGREEMENT_SUBMODULE_CODE, userMessage);
					});
				}
			}
		} else {
			agreementActionLog = addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_ROUTING, agreementHeader.getUpdateUser(), null);
		}
		/*
		 * Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		 * commonService.setNotificationRecipients(agreementHeader.getRequestorPersonId(
		 * ), "TO", dynamicEmailrecipients); sendNotificationForAgreement(vo,
		 * Constants.SUBMIT_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
		 * addActionLogEntry(agreementHeader.getAgreementRequestId(),
		 * Constants.ACTION_LOG_AGREEMENT_SUBMITTED, agreementHeader.getUpdateUser(),
		 * "Agreement Submitted for Review");
		 */
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		vo.setAgreementHeader(agreementDao.saveOrUpdateAgreement(agreementHeader));
		loadAgreementById(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void addToAgreementTypeHistory(String agreementTypeCode, Integer agreementRequestId, String updateUser) {
		AgreementTypeHistory agreementTypeHistory = new AgreementTypeHistory();
		agreementTypeHistory.setAgreementRequestId(agreementRequestId);
		agreementTypeHistory.setAgreementTypeCode(agreementTypeCode);
		agreementTypeHistory.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementTypeHistory.setUpdateUser(updateUser);
		agreementDao.saveOrUpdateAgreementTypeHistory(agreementTypeHistory);
	}

	@Override
	public void sendNotificationForAgreement(AgreementVO agreementVO, Integer notificationTypeId,
			Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
		emailServiceVO.setModuleItemKey(agreementVO.getAgreementRequestId().toString());
		emailServiceVO.setPlaceHolder(getPlaceholders(agreementVO));
		emailServiceVO.setSubModuleCode((Constants.AGREEMENT_SUBMODULE_CODE).toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		if (emailServiceVO.getPrompted() != null && emailServiceVO.getPrompted() != true) {
			agreementVO.setNotificationTypeId(notificationTypeId);
			agreementVO.setBody(emailServiceVO.getBody());
			agreementVO.setSubject(emailServiceVO.getSubject());
		}
	}

	private Map<String, String> getPlaceholders(AgreementVO vo) {
		Map<String, String> placeHolder = new HashMap<>();
		if (vo.getAssigneePersonId() != null) {
			placeHolder.put("{ASSIGNEE}", personDao.getPersonFullNameByPersonId(vo.getAssigneePersonId()));
		} else {
			placeHolder.put("{ASSIGNEE}", "");
		}
		placeHolder.put("{WORKFLOW_COMMENT}", vo.getApproveComment() != null ? vo.getApproveComment() : "No Comments");
		String stopName =commonService.getPlaceHolderDataForRouting(vo.getApproverStopNumber(),vo.getMapId(),vo.getWorkflowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	@Override
	public String deleteAgreementSponsor(Integer agreementSponsorId) {
		try {
			AgreementSponsor agreementSponsor = agreementDao.fetchAgreementSponsorBasedOnId(agreementSponsorId);
			if (agreementSponsor != null) {
				AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementSponsor.getAgreementRequestId());
				agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				agreementDao.saveOrUpdateAgreement(agreementHeader);
			}
			return commonDao.convertObjectToJSON(deleteAgreementSponsor(agreementSponsor));
		} catch (Exception e) {
			return commonDao.convertObjectToJSON("Error Occured");
		}
	}

	private String deleteAgreementSponsor(AgreementSponsor agreementSponsor) {
		try {
			List<AgreementSponsorContact> agreementSponsorContacts = agreementDao.getAgreementSponsorContacts(agreementSponsor.getAgreementSponsorId());
			if (agreementSponsorContacts != null && !agreementSponsorContacts.isEmpty()) {
				for (AgreementSponsorContact agreementSponsorContact : agreementSponsorContacts) {
					agreementDao.deleteAgreementSponsorContact(agreementSponsorContact);
				}
			}
			return agreementDao.deleteAgreementSponsor(agreementSponsor);
		} catch (Exception e) {
			return "Error Occured";
		}
	}

	@Override
	public String loadAgreementNegotiation(AgreementVO vo) {
		if (vo.getNegotiationId() == null) {
			vo.setNegotiationId(getNegotiationIdBasedOnAgreementId(vo.getAgreementRequestId()));
		}
		vo.setNegotiationsLocationTypes(negotiationAgreementDao.getNegotiationsLocationTypes());
		vo.setNegotiationLocationStatuses(negotiationAgreementDao.getNegotiationLocationStatus());
		vo.setNegotiationsLocations(prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(vo.getNegotiationId())));
		vo.setNegotiationsActivityTypes(negotiationAgreementDao.getNegotiationsActivityTypes());
		vo.setNegotiationsPersonnelTypes(negotiationAgreementDao.getNegotiationsPersonnelTypes());
		vo.setNegotiationsActivities(prepareNegotiationActivities(negotiationAgreementDao.getNegotiationsActivityByNegotiationId(vo.getNegotiationId())));
		//vo.setAgreementComments(agreementCommentService.prepareAgreementComment(vo.getAgreementRequestId()));
		return commonDao.convertObjectToJSON(vo);
	}

	private List<NegotiationsActivity> prepareNegotiationActivities(List<NegotiationsActivity> negotiationsActivities) {
		if (negotiationsActivities != null && !negotiationsActivities.isEmpty()) {
			negotiationsActivities.forEach(negotiationsActivity -> {
				if (negotiationsActivity.getEndDate() != null && negotiationsActivity.getStartDate() != null) {
					Long noOfDays = commonDao.getNumberOfDays(negotiationsActivity.getStartDate(),
							negotiationsActivity.getEndDate());
					negotiationsActivity.setNoOfDays(noOfDays);
				} else if (negotiationsActivity.getStartDate() != null) {
					Long noOfDays = commonDao.getNumberOfDays(negotiationsActivity.getStartDate(),
							commonDao.getCurrentTimestamp());
					negotiationsActivity.setNoOfDays(noOfDays);
				}
				if (negotiationsActivity.getUpdateUser() != null) {
					negotiationsActivity.setUpdateUserFullName(
							personDao.getUserFullNameByUserName(negotiationsActivity.getUpdateUser()));
				}
				List<AttachmentData> attachmentDataList = new ArrayList<>();
				List<NegotiationsAttachment> attachmentList = negotiationAgreementDao
						.getAttachmentData(negotiationsActivity.getNegotiationsActivityId());
				if (attachmentList != null && !attachmentList.isEmpty()) {
					attachmentList.forEach(attachment -> {
						AttachmentData data = new AttachmentData();
						data.setFileName(attachment.getFileName());
						data.setNegotiationAttachmentId(attachment.getNegotiationsAttachmentId());
						attachmentDataList.add(data);
					});
				}
				negotiationsActivity.setAttachmentDataList(attachmentDataList);
			});
		}
		return negotiationsActivities;
	}

	@Override
	public List<NegotiationsLocation> prepareNegotiationsLocations(List<NegotiationsLocation> negotiationsLocations) {
		if (negotiationsLocations != null && !negotiationsLocations.isEmpty()) {
			negotiationsLocations.forEach(negotiationsLocation -> {
				Long numberOfDays = 0L;
				if (negotiationsLocation.getLocationStatusCode().equals(Constants.LOCATION_STATUS_CODE_COMPLETED)) {
					numberOfDays = commonDao.getNumberOfDays(negotiationsLocation.getCreateTimestamp(), negotiationsLocation.getUpdateTimestamp());
				} else {
					numberOfDays = commonDao.getNumberOfDays(negotiationsLocation.getCreateTimestamp(), commonDao.getCurrentTimestamp());
				}
				negotiationsLocation.setNumberOfDays(numberOfDays.intValue());
				if (negotiationsLocation.getCreateUser() != null) {
					negotiationsLocation.setCreateUserFullName(personDao.getUserFullNameByUserName(negotiationsLocation.getCreateUser()));
				}
				if (negotiationsLocation.getUpdateUser() != null) {
					negotiationsLocation.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsLocation.getUpdateUser()));
				}
				if (negotiationsLocation.getAssigneePersonId() != null) {
					negotiationsLocation.setPerson(personDao.getPersonDetailById(negotiationsLocation.getAssigneePersonId()));
				}
				negotiationsLocation.setTotalComments(negotiationAgreementDao.fetchLocationCommentCount(negotiationsLocation.getNegotiationLocationId()));
			});
		}
		return negotiationsLocations;
	}

	@Override
	public String addAgreementTemplate(MultipartFile[] files, String formDataJSON) {
		AgreementVO agreementVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			agreementVO = mapper.readValue(formDataJSON, AgreementVO.class);
			List<AgreementTypeTemplate> attachments = agreementDao
					.fetchAgreementAttachmentsBasedOnAgreementType(agreementVO.getAgreementTypeCode());
			Integer documentId = 0;
			Integer maxVersionNumber = agreementDao.getMaxVersionNumber(agreementVO.getAgreementTypeCode());
			if (attachments != null && !attachments.isEmpty()) {
				Collections.sort(attachments,
						(attachment1, attachment2) -> attachment1.getDocumentId() > attachment2.getDocumentId() ? -1
								: attachment1.getDocumentId() == attachment2.getDocumentId() ? 0 : 1);
				documentId = attachments.get(0).getDocumentId();
			}
			List<AgreementTypeTemplate> newAttachments = agreementVO.getNewAgreementTypeTemplate();
			for (int i = 0; i < files.length; i++) {
				for (AgreementTypeTemplate newAttachment : newAttachments) {
					String replaceFileName = newAttachment.getFileName();
					boolean isRenameRequired = false;
					int count = 1;
					isRenameRequired = checkForDuplicationInTemplate(newAttachment.getFileName(), attachments);
					while (isRenameRequired) {
						replaceFileName = newAttachment.getFileName();
						replaceFileName = generateFileName(replaceFileName, count);
						count = count + 1;
						isRenameRequired = checkForDuplicationInTemplate(replaceFileName, attachments);
					}
					if (attachments != null && !attachments.isEmpty()) {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						AgreementTypeTemplate agreementTypeTemplate = agreementDao
								.fetchAgreementTypeTemplateBasedOnParams(agreementVO.getAgreementTypeCode());
						if (agreementTypeTemplate != null) {
							agreementTypeTemplate.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
							agreementTypeTemplate.setDocumentStatus(
									commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
							agreementTypeTemplate.setFinal(false);
							documentId = agreementTypeTemplate.getDocumentId();
							agreementDao.saveOrUpdateAgreementTypeTemplate(agreementTypeTemplate);
						}
						AgreementTypeTemplate newAgreementTypeTemplate = addNewAgreementAttachment(newAttachment,
								files[i], fileName, maxVersionNumber, documentId, replaceFileName);
						agreementDao.saveOrUpdateAgreementTypeTemplate(newAgreementTypeTemplate);
					} else {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						if (newAttachment.getFileName().equals(fileName)) {
							documentId = documentId + 1;
							AgreementTypeTemplate agreementAttachment = addNewAgreementAttachment(newAttachment,
									files[i], fileName, maxVersionNumber, documentId, replaceFileName);
							agreementDao.saveOrUpdateAgreementTypeTemplate(agreementAttachment);
						}
					}
				}
			}
			agreementVO.setAgreementTypeTemplates(prepareAgreementTypeTemplates(
					agreementDao.fetchAgreementAttachmentsBasedOnAgreementType(agreementVO.getAgreementTypeCode())));
		} catch (Exception e) {
			logger.error("Exception in addAgreementAttachment: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(agreementVO);
	}

	private List<AgreementTypeTemplate> prepareAgreementTypeTemplates(
			List<AgreementTypeTemplate> agreementTypeTemplates) {
		if (agreementTypeTemplates != null && !agreementTypeTemplates.isEmpty()) {
			agreementTypeTemplates.forEach(agreementTypeTemplate -> {
				if (agreementTypeTemplate.getUpdateUser() != null) {
					agreementTypeTemplate.setCreateUserFullName(
							personDao.getUserFullNameByUserName(agreementTypeTemplate.getUpdateUser()));
				}
			});
		}
		return agreementTypeTemplates;
	}

	private boolean checkForDuplicationInTemplate(String fileName, List<AgreementTypeTemplate> attachments) {
		for (AgreementTypeTemplate attachment : attachments) {
			if (fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	public AgreementTypeTemplate addNewAgreementAttachment(AgreementTypeTemplate attachment, MultipartFile file,
			String fileName, Integer versionNumber, Integer documentId, String replacedFileName) {
		AgreementTypeTemplate agreementAttachment = new AgreementTypeTemplate();
		try {
			if (attachment.getFileName().equals(fileName)) {
				agreementAttachment.setAgreementTypeCode(attachment.getAgreementTypeCode());
				agreementAttachment.setDescription(attachment.getDescription());
				agreementAttachment.setUpdateTimestamp(attachment.getUpdateTimestamp());
				agreementAttachment.setUpdateUser(attachment.getUpdateUser());
				agreementAttachment.setFileName(replacedFileName);
				agreementAttachment.setContentType(file.getContentType());
				agreementAttachment.setVersionNumber(versionNumber);
				agreementAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
				agreementAttachment
						.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
				agreementAttachment.setDocumentId(documentId);
				agreementAttachment.setFinal(true);
				agreementAttachment.setTemplate(file.getBytes());
			}
		} catch (Exception e) {
			logger.error("Exception in addNewAgreementAttachment: {} ", e.getMessage());
		}
		return agreementAttachment;
	}

	@Override
	public String saveAgreementPerson(AgreementVO vo) {
		AgreementHeader agreementHeader = vo.getAgreementHeader();
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementHeader = agreementDao.saveOrUpdateAgreement(agreementHeader);
		loadPersonDetailsInAgreement(agreementHeader);
		return commonDao.convertObjectToJSON(agreementHeader);
	}

	@Override
	public String addClausesGroup(TemplateManagementVO vo) {
		ClausesGroup clausesGroup = vo.getClausesGroup();
		clausesGroup.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		if (clausesGroup.getClauses() != null && !clausesGroup.getClauses().isEmpty()) {
			clausesGroup.getClauses().forEach(clause -> {
				clause.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			});
		}
		vo.setClausesGroup(agreementDao.saveOrUpdateClausesGroup(clausesGroup));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String linkClausesGroupToAgreementType(TemplateManagementVO vo) {
		AgreementClausesGroupMapping agreementClausesGroupMapping = vo.getAgreementClausesGroupMapping();
		agreementClausesGroupMapping.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		vo.setAgreementClausesGroupMapping(
				agreementDao.saveOrUpdateAgreementClausesGroup(agreementClausesGroupMapping));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAllFormPlaceHolders() {
		TemplateManagementVO vo = new TemplateManagementVO();
		vo.setAgreementPlaceHolder(agreementDao.getAllActiveAgreementPlaceHolders());
		vo.setAgreementCategories(agreementDao.fetchAllAgreementCategory());
		vo.setAgreementTypes(agreementDao.fetchAgreementTypes());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAllClauses() {
		TemplateManagementVO vo = new TemplateManagementVO();
		List<ClausesGroup> clausesDetails = agreementDao.getClausesDetails();
		if (clausesDetails != null && !clausesDetails.isEmpty()) {
			clausesDetails.forEach(clausesDetail -> {
				clausesDetail.setAgreementTypes(prepareAgreementTypes(clausesDetail.getClauseGroupCode()));
			});
		}
		vo.setClausesDetails(agreementDao.getClausesDetails());
		vo.setAgreementTypes(agreementDao.fetchAgreementTypes());
		return commonDao.convertObjectToJSON(vo);
	}

	private List<AgreementType> prepareAgreementTypes(Integer clauseGroupCode) {
		List<AgreementClausesGroupMapping> agreementClausesGroups = agreementDao
				.fetchAllAgreementTypeBasedOnClausesGroup(clauseGroupCode);
		List<AgreementType> agreementTypes = new ArrayList<>();
		if (agreementClausesGroups != null && !agreementClausesGroups.isEmpty()) {
			agreementClausesGroups.forEach(agreementClausesGroup -> {
				agreementTypes.add(agreementClausesGroup.getAgreementType());
			});
		}
		return agreementTypes;
	}

	@Override
	public String loadAllTemplates(TemplateManagementVO vo) {
		vo.setAgreementCategories(agreementDao.fetchAllAgreementCategory());
		vo.setAgreementTypes(agreementDao.fetchAgreementTypes());
		if (vo.getAgreementTypeCode() != null) {
			vo.setAgreementTypeTemplates(prepareAgreementTypeTemplates(
					agreementDao.fetchAgreementAttachmentsBasedOnAgreementType(vo.getAgreementTypeCode())));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAllClausesTemplates(TemplateManagementVO vo) {
		List<AgreementClausesGroupMapping> agreementClauses = agreementDao
				.getClausesDetailsByAgreementTypeCode(vo.getAgreementTypeCode());
		List<ClausesGroup> clauses = new ArrayList<>();
		List<PlaceHolderDTO> placeHolders = new ArrayList<>();
		if (agreementClauses != null && !agreementClauses.isEmpty()) {
			agreementClauses.forEach(agreementClause -> {
				clauses.add(agreementClause.getClausesGroup());
			});
		}
		if (!clauses.isEmpty()) {
			clauses.forEach(clausesGroup -> {
				PlaceHolderDTO placeHolder = new PlaceHolderDTO();
				placeHolder.setPlaceHolderName("CL-" + clausesGroup.getClauseGroupCode());
				placeHolder.setDescription(clausesGroup.getDescription());
				placeHolders.add(placeHolder);
			});
		}
		vo.setPlaceHolders(placeHolders);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteClauses(TemplateManagementVO vo) {
		agreementDao.deleteClauses(agreementDao.fetchClausesById(vo.getClausesCode()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String unlinkClausesGroupToAgreementType(TemplateManagementVO vo) {
		AgreementClausesGroupMapping agreementClausesGroup = agreementDao
				.fetchAgreementGroupMapping(vo.getAgreementTypeCode(), vo.getClausesGroupCode());
		if (agreementClausesGroup != null) {
			agreementDao.deleteAgreementGroupMapping(agreementClausesGroup);
			return commonDao.convertObjectToJSON(vo);
		} else {
			return commonDao.convertObjectToJSON(vo);
		}
	}

	@Override
	public ResponseEntity<byte[]> downloadAgreementTemplate(Integer templateId) {
		AgreementTypeTemplate agreementTypeTemplate = agreementDao.fetchAgreementTypeTemplateById(templateId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			byte[] data = agreementTypeTemplate.getTemplate();
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType(agreementTypeTemplate.getContentType()));
			String filename = agreementTypeTemplate.getFileName();
			headers.setContentDispositionFormData(filename, filename);
			headers.setContentLength(data.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Exception in downloadAgreementTemplate: {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String deleteAgreementTemplate(TemplateManagementVO vo) {
		agreementDao.deleteAgreementTypeTemplate(agreementDao.fetchAgreementTypeTemplateById(vo.getTemplateId()));
		vo.setAgreementTypeTemplates(prepareAgreementTypeTemplates((prepareAgreementTypeTemplates(
				agreementDao.fetchAgreementAttachmentsBasedOnAgreementType(vo.getAgreementTypeCode())))));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAllQuestionsPlaceHolders(TemplateManagementVO vo) {
		Integer agreementTypeCode = Integer.parseInt(vo.getAgreementTypeCode());
		List<AgreementPlaceHolder> agreementPlaceHolders = new ArrayList<>();
		List<HashMap<String, Object>> output = questionnaireDao.getQuestionsByModule(Constants.AGREEMENT_MODULE_CODE,
				agreementTypeCode);
		if (output != null && !output.isEmpty()) {
			output.forEach(hmResult -> {
				AgreementPlaceHolder agreementPlaceHolder = new AgreementPlaceHolder();
				agreementPlaceHolder.setPlaceHolderName(hmResult.get("QN_NUMBER").toString());
				agreementPlaceHolder.setDescription(hmResult.get("QUESTION").toString());
				agreementPlaceHolders.add(agreementPlaceHolder);
			});
		}
		vo.setAgreementPlaceHolder(agreementPlaceHolders);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAgreementHistory(Integer agreementRequestId) {
		AgreementVO agreementVO = new AgreementVO();
		List<AgreementActionLog> agreementActionLogs = agreementDao.fetchAgreementActionLogsBasedOnAgreementId(agreementRequestId);
		//List<AgreementActionType> agreementActionTypes = agreementDao.fetchAgreementActionTypes();
		List<AgreementHistory> agreementHistories = new ArrayList<>();
		agreementActionLogs.forEach(agreementActionLog -> {
			AgreementHistory agreementHistory = new AgreementHistory();
			agreementHistory.setUpdateTimestamp(agreementActionLog.getUpdateTimestamp());
			if (agreementActionLog.getUpdateUser() != null) {
				agreementHistory
						.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementActionLog.getUpdateUser()));
			}
			agreementHistory.setActionTypeCode(agreementActionLog.getActionTypeCode());
			agreementHistory.setAgreementNotes(agreementDao.fetchAgreementNoteBasedOnActionLogId(agreementActionLog.getActionLogId()));
			agreementHistory.setMessage(agreementActionLog.getDescription());
			agreementHistories.add(agreementHistory);
		});
		agreementVO.setAgreementHistories(agreementHistories);
		return commonDao.convertObjectToJSON(agreementVO);
	}

	@Override
	public List<ClausesBank> findClauses(String searchString) {
		return agreementDao.findClauses(searchString);
	}

	@Override
	public String saveOrUpdateAgreementClauses(AgreementVO vo) {
		AgreementClauses agreementClauses = vo.getAgreementClause();
		if (agreementClauses.getAgreementHeader() != null) {
			AgreementHeader agreementHeader = agreementClauses.getAgreementHeader();
			agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementDao.saveOrUpdateAgreement(agreementHeader);
		}
		agreementClauses.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementClauses = agreementDao.saveOrUpdateAgreementClauses(agreementClauses);
		vo.setAgreementClausesGroup(prepareAgreementClauses(
				agreementDao.fetchAgreementClausesBasedOnAgreementId(agreementClauses.getAgreementRequestId())));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAgreementClauses(AgreementVO vo) {
		agreementDao.deleteAgreementClauses(agreementDao.fetchAgreementClausesBasedOnId(vo.getAgreementClauseId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAgreementClausesByAgreementId(AgreementVO vo) {
		vo.setAgreementClauses(agreementDao.fetchAgreementClausesBasedOnAgreementId(vo.getAgreementRequestId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String readyToExecute(AgreementVO vo) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		loadPersonDetailsInAgreement(agreementHeader);
		updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_EXECUTED);
		agreementHeader.setUpdateUser(vo.getUpdateUser());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_COMPLETED));
		agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_COMPLETED);
		Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
		sendNotificationForAgreement(vo, Constants.READY_TO_EXECUTE_NOTIFICATION_CODE, dynamicEmailrecipients);
		AgreementActionLog agreementActionLog = addActionLogEntry(agreementHeader.getAgreementRequestId(), Constants.ACTION_LOG_EXECUTED, agreementHeader.getUpdateUser(), null);
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		vo.setAgreementHeader(agreementDao.saveOrUpdateAgreement(agreementHeader));
		loadAgreementById(vo);
		inboxDao.markAsExpiredFromActionList(Constants.MODULE_CODE_AGREEMENT,agreementHeader.getAgreementRequestId().toString(), Constants.SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String finalizeAgreement(MultipartFile[] files, String formDataJSON, HttpServletResponse response) {
		AgreementVO vo = null;
		AgreementHeader agreementHeader = new AgreementHeader();
		try {
			ObjectMapper mapper = new ObjectMapper();
			vo = mapper.readValue(formDataJSON, AgreementVO.class);
			AgreementActionLog agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_FINAL_AGREEMENT, vo.getUpdateUser(),null);
			if (vo.getNewAttachments() != null && !vo.getNewAttachments().isEmpty()) {
				addAgreementAttachment(files, formDataJSON, response);
			}
			AgreementNote agreementNote = new AgreementNote();
			agreementNote.setNote(vo.getComment());
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementNote.setUpdateUser(vo.getUpdateUser());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
			agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
			loadPersonDetailsInAgreement(agreementHeader);
			updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_FINAL);
			agreementHeader.setUpdateUser(vo.getUpdateUser());
			agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_COMPLETED));
			agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_COMPLETED);
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			List<AgreementPeople> agreementPeoples = agreementDao
					.getAllAgreementPeople(agreementHeader.getAgreementRequestId());
			if (agreementPeoples != null && !agreementPeoples.isEmpty()) {
				for (AgreementPeople agreementPeople : agreementPeoples) {
					if (agreementPeople.getPeopleTypeId().equals(1) && agreementPeople.getPersonId() != null) {
						commonService.setNotificationRecipients(agreementPeople.getPersonId(),
								Constants.NOTIFICATION_RECIPIENT_TYPE_CC, dynamicEmailrecipients);
					}
				}
			}
			sendNotificationForAgreement(vo, Constants.AGREEMENT_FINAL_NOTIFICATION_CODE, dynamicEmailrecipients);
			loadAgreementUserFullNames(agreementHeader);
		} catch (Exception e) {
			logger.error("Exception in finalize Agreement: {} ", e.getMessage());
		}
		Integer versionNumber = agreementDao.fetchMaxVersionAgreementTemplateBasedOnParams(vo.getAgreementTypeCode());
		if (versionNumber != 0) {
			vo.setAgreementTypeCode(agreementHeader.getAgreementTypeCode());
			vo.setIsAgreementCreatedOnce(true);
			vo.setAgreementType(agreementHeader.getAgreementType().getDescription());
			AgreementTypeTemplate agreementTypeTemplate = agreementDao
					.getAgreementTypeTemplateBasedOnParams(vo.getAgreementTypeCode(), versionNumber);
			byte[] mergedOutput = agreementPrintService.mergePlaceHoldersOfAgreement(agreementTypeTemplate.getTemplate(),
					vo.getAgreementRequestId());
			saveTemplateAsAttachments(vo, mergedOutput);
		}
		vo.setAgreementHeader(agreementDao.saveOrUpdateAgreement(agreementHeader));
		vo.setAgreementAttachments(agreementDao.fetchAllAgreementAttachmentsBasedOnParams(
				agreementHeader.getAgreementRequestId(), Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT));
		if (vo.getAgreementAttachments() != null && !vo.getAgreementAttachments().isEmpty()) {
			getUpdateUserFullName(vo);
		}
		//vo.setAgreementComments(agreementCommentService.prepareAgreementComment(agreementHeader.getAgreementRequestId()));
		vo.getAgreementHeader().setNegotiationId(getNegotiationIdBasedOnAgreementId(vo.getAgreementRequestId()));
		inboxDao.markAsExpiredFromActionList(Constants.MODULE_CODE_AGREEMENT,agreementHeader.getAgreementRequestId().toString(), Constants.SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);		
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void saveTemplateAsAttachments(AgreementVO agreementVO, byte[] mergedOutput) {
		AgreementAttachment agreementAttachment = new AgreementAttachment();
		agreementAttachment.setAgreementRequestId(agreementVO.getAgreementRequestId());
		agreementAttachment.setAgreementAttachmentTypeCode(Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT);
		AgreementAttachmentFile fileData = new AgreementAttachmentFile();
		fileData.setFileData(mergedOutput);
		fileData = agreementDao.saveFileData(fileData);
		agreementAttachment.setAgreementAttachmentFileId(fileData.getAgreementAttachmentFileId());
		agreementAttachment.setDescription(agreementVO.getComment());
		agreementAttachment.setContentType("application/octet-stream");
		AgreementActionLog agreementActionLog = null;
		if (Boolean.TRUE.equals(agreementVO.getIsAgreementCreatedOnce())) {
			agreementActionLog = addActionLogEntry(agreementVO.getAgreementRequestId(), Constants.ACTION_LOG_REGENERATE_AGRREMENT, agreementVO.getUpdateUser(), null);
			Integer maxVersionNumber = agreementDao.fetchMaxVersionAgreementAttachmentBasedOnParams(
					agreementVO.getAgreementRequestId(), Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT);
			AgreementAttachment previousAgreementAttachment = agreementDao.getAgreementAgreementAttachmentBasedOnParams(
					agreementVO.getAgreementRequestId(), maxVersionNumber);
			Integer newVersionNumber = previousAgreementAttachment.getVersionNumber() + 1;
			previousAgreementAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_ARCHIVED);
			previousAgreementAttachment
					.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_ARCHIVED));
			agreementAttachment.setVersionNumber(newVersionNumber);
			agreementAttachment.setFileName(generateFileName(agreementVO) + "(Version " + newVersionNumber + ")");
			agreementDao.saveOrUpdateAgreementAttachment(previousAgreementAttachment);
		} else {
			agreementActionLog = addActionLogEntry(agreementVO.getAgreementRequestId(), Constants.ACTION_LOG_GENERATE_AGREEMENT, agreementVO.getUpdateUser(), null);
			agreementAttachment.setFileName(generateFileName(agreementVO) + "(Version 1)");
			agreementAttachment.setVersionNumber(1);
		}
		agreementAttachment.setActionLogId(agreementActionLog.getActionLogId());
		agreementAttachment.setAgreementAttachStatusCode(Constants.NARRATIVE_STATUS_CODE_INCOMPLETE);
		agreementAttachment.setAgreementActionLog(agreementActionLog);
		agreementAttachment.setDocumentId(0);
		agreementAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
		agreementAttachment.setDocumentStatus(commonDao.getDocumentStatusById(Constants.DOCUMENT_STATUS_CODE_DRAFT));
		agreementAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementAttachment.setUpdateUser(agreementVO.getUpdateUser());
		agreementAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementVO.getUpdateUser()));
		agreementDao.saveOrUpdateAgreementAttachment(agreementAttachment);
		agreementVO.setIsAgreementCreatedOnce(true);
		agreementVO.setAgreementAttachments(agreementDao.fetchAllAgreementAttachmentsBasedOnParams(
				agreementVO.getAgreementRequestId(), Constants.ATTACHMENT_TYPE_CODE_GENERATE_AGREEMENT));
	}

	private String generateFileName(AgreementVO agreementVO) {
		String fileName = "";
		String agreementCategoryName = "";
		String primaryOrganisationName = "";
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementVO.getAgreementRequestId());
		if (agreementHeader != null && agreementHeader.getAgreementCategory() != null
				&& agreementHeader.getAgreementCategory().getDescription() != null) {
			agreementCategoryName = agreementHeader.getAgreementCategory().getDescription();
		}
		primaryOrganisationName = agreementDao.getPrimaryOrganisationName(agreementVO.getAgreementRequestId());
		if (primaryOrganisationName != null) {
			if (primaryOrganisationName.length() > 30) {
				primaryOrganisationName = primaryOrganisationName.substring(0, 30);
			}
			fileName = fileName.concat(primaryOrganisationName);
		}
		fileName = fileName.concat(Constants.AGREEMENT_REPORT_TEMPLATE_NAME);
		AgreementPeople agreementPeople = agreementDao.getAgreementPI(agreementVO.getAgreementRequestId());
		String piLastName = "";
		if (agreementPeople != null && agreementPeople.getPersonId() != null) {
			piLastName = personDao.getPersonDetailById(agreementPeople.getPersonId()).getLastName();
		}
		fileName = fileName + "(" + piLastName + ")-" + agreementCategoryName + "-"
				+ agreementVO.getAgreementRequestId();
		return fileName;
	}

	@Override
	public String submitAgreementReview(AgreementVO vo) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		markAllMessagesAsRead(agreementHeader.getAgreementRequestId().toString());
		AgreementActionLog agreementActionLog = new AgreementActionLog();
		if (vo.getAgreementReviewTypeCode() != null) {
			AgreementReviewType agreementReviewType = agreementDao.fetchAgreementReviewTypes(vo.getAgreementReviewTypeCode());
			agreementHeader.setWorkflowStatusCode(agreementReviewType.getWorkflowStatusCode());
			agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(agreementReviewType.getWorkflowStatusCode()));
			if (agreementReviewType.getLocationTypeCode() != null) {
				createNegotiationLocation(vo.getNegotiationId(), vo.getUpdateUser(), agreementReviewType.getLocationTypeCode());
			}
			switch (vo.getAgreementReviewTypeCode()) {
			case Constants.SUBMIT_TO_3RD_PARTY:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SENT_TO_SPONSOR, vo.getUpdateUser(), null);
				break;
			case Constants.SUBMIT_TO_LEGAL:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_LEGAL_REVIEW_INPROGRESS, vo.getUpdateUser(), null);
				break;
			case Constants.SUBMIT_TO_HOD:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_HOD_REVIEW_INPROGRESS, vo.getUpdateUser(), null);
				break;
			case Constants.PUT_ON_HOLD:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_PUT_ON_HOLD, vo.getUpdateUser(), null);
				break;
			case Constants.SUBMIT_TO_OTHER_PARTY:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SUBMIT_TO_OTHER_PARTY, vo.getUpdateUser(), null);
				break;
			case Constants.SUBMIT_TO_OGC:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SUBMIT_TO_OGC, vo.getUpdateUser(), null);
				break;
			case Constants.SUBMIT_TO_RAS:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SUBMIT_TO_RAS, vo.getUpdateUser(), null);
				break;
			case Constants.SUBMIT_TO_TLO:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SUBMIT_TO_TLO, vo.getUpdateUser(), null);
				break;
			case Constants.SET_INPROGREES:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_SET_INPROGRESS, vo.getUpdateUser(), null);
				break;
			case Constants.COMPLETE_REVIEW:
				agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_COMPLETE_REVIEW, vo.getUpdateUser(), null);
				break;
			default:
				agreementActionLog = null;
				break;
			}
		}
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			if (agreementActionLog != null) {
				agreementNote.setActionLogId(agreementActionLog.getActionLogId());
				agreementNote.setAgreementActionLog(agreementActionLog);
			}
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		vo.setAgreementHeader(agreementDao.saveOrUpdateAgreement(agreementHeader));
		vo.getAgreementHeader().setNegotiationId(getNegotiationIdBasedOnAgreementId(agreementHeader.getAgreementRequestId()));
		//vo.setAgreementComments(agreementCommentService.prepareAgreementComment(agreementHeader.getAgreementRequestId()));
		return commonDao.convertObjectToJSON(vo);
	}

	private void markAllMessagesAsRead(String agreementRequestId) {
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Constants.MESSAGE_TYPE_LEGAL_COMPLETED);
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Constants.MESSAGE_TYPE_HOD_COMPLETED);
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Constants.MESSAGE_TYPE_THIRD_PARTY_COMPLETED);
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Constants.MESSAGE_TYPE_SUBMITTED);
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Constants.MESSAGE_TYPE_ROUTING_IN_PROGRESS);
		inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Constants.MESSAGE_TYPE_RETURNED);
	}

	private void createNegotiationLocation(Integer negotiationId, String updateUser, String locationTypeCode) {
		NegotiationsLocation negotiationsLocation = new NegotiationsLocation();
		negotiationsLocation.setNegotiationId(negotiationId);
		negotiationsLocation.setUpdateUser(updateUser);
		negotiationsLocation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		negotiationsLocation.setLocationStatusCode("2");
		negotiationsLocation.setLocationTypeCode(locationTypeCode);
		negotiationsLocation.setCreateUser(updateUser);
		negotiationsLocation.setCreateTimestamp(commonDao.getCurrentTimestamp());
		negotiationAgreementDao.saveOrUpdateNegotiationLocation(negotiationsLocation);
	}

	@Override
	public String completeReview(AgreementVO vo) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		AgreementActionLog agreementActionLog = new AgreementActionLog();
		NegotiationsLocation negotiationsLocation = negotiationAgreementDao.getNegotiationLocationById(vo.getNegotiationLocationId());
		negotiationsLocation.setLocationStatusCode(Constants.LOCATION_STATUS_CODE_COMPLETED);
		negotiationsLocation.setNegotiationLocationStatus(negotiationAgreementDao.getNegotiationLocationStatusById(Constants.LOCATION_STATUS_CODE_COMPLETED));
		negotiationAgreementDao.saveOrUpdateNegotiationLocation(negotiationsLocation);
		agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_LOCATION_COMPLETE, vo.getUpdateUser(), negotiationsLocation.getNegotiationsLocationType().getDescription() + " review has been completed");
		InboxVO inboxVO = new InboxVO();
		inboxVO.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
		inboxVO.setModuleItemKey(agreementHeader.getAgreementRequestId().toString());
		inboxVO.setMessageTypeCode(Constants.MESSAGE_TYPE_LOCATION_ASSIGNMENT);
		inboxVO.setSubModuleCode(Constants.AGREEMENT_SUBMODULE_CODE);
		inboxVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		inboxVO.setToPersonId(vo.getPersonId());
		inboxDao.markReadMessage(inboxVO);
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			if (agreementActionLog.getActionLogId() != null) {
				agreementNote.setActionLogId(agreementActionLog.getActionLogId());
				agreementNote.setAgreementActionLog(agreementActionLog);
			}
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		vo.setAgreementHeader(agreementDao.saveOrUpdateAgreement(agreementHeader));
		//vo.setAgreementComments(agreementCommentService.prepareAgreementComment(agreementHeader.getAgreementRequestId()));
		vo.getAgreementHeader().setNegotiationId(getNegotiationIdBasedOnAgreementId(agreementHeader.getAgreementRequestId()));
		vo.setNegotiationsLocations(prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(vo.getNegotiationId())));
		loadAgreementUserFullNames(agreementHeader);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAgreementGroup(AgreementVO vo) {
		List<AgreementClauses> agreementClauses = agreementDao .fetchAgreementClausesBasedOnParams(vo.getAgreementRequestId(), vo.getClausesGroupCode());
		if (agreementClauses != null && !agreementClauses.isEmpty()) {
			for (AgreementClauses clauses : agreementClauses) {
				agreementDao.deleteAgreementClauses(clauses);
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String startReview(AgreementVO vo) {
		NegotiationsLocation negotiationsLocation = negotiationAgreementDao.getNegotiationLocationById(vo.getNegotiationLocationId());
		negotiationsLocation.setLocationStatusCode(Constants.lOCATION_STATUS_CODE_INPROGRESS);
		negotiationsLocation.setNegotiationLocationStatus(negotiationAgreementDao.getNegotiationLocationStatusById(Constants.lOCATION_STATUS_CODE_INPROGRESS));
		negotiationsLocation.setUpdateUser(vo.getUpdateUser());
		negotiationsLocation.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		negotiationAgreementDao.saveOrUpdateNegotiationLocation(negotiationsLocation);
		vo.setNegotiationsLocations(prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(vo.getNegotiationId())));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String markAttachmentAsFinal(AgreementVO vo) {
		AgreementAttachment agreementAttachment = agreementDao.fetchAttachmentById(vo.getAgreementAttachmentId());
		if (Boolean.TRUE.equals(vo.getIsFinal())) {
			agreementAttachment.setAgreementAttachStatusCode("C");
			agreementAttachment.setAgreementAttachmentStatus(agreementDao.getAgreementAttachmentStatusByCode("C"));
		} else {
			agreementAttachment.setAgreementAttachStatusCode("I");
			agreementAttachment.setAgreementAttachmentStatus(agreementDao.getAgreementAttachmentStatusByCode("I"));
		}
		vo.setAgreementAttachment(agreementDao.saveOrUpdateAgreementAttachment(agreementAttachment));
		vo.setAgreementAttachments(agreementDao
				.fetchAgreementAttachmentsBasedOnAgreementId(agreementAttachment.getAgreementRequestId(), true));
		if (vo.getAgreementAttachments() != null && !vo.getAgreementAttachments().isEmpty()) {
			getUpdateUserFullName(vo);
		}
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementAttachment.getAgreementRequestId());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAgreementSponsorContact(AgreementVO vo) {
		agreementDao.deleteAgreementSponsorContact(
				agreementDao.fetchAgreementSponsorContactsBasedOnId(vo.getAgreementSponsorContactId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateOrganisationContact(AgreementVO vo) {
		AgreementSponsorContact agreementSponsorContact = vo.getAgreementSponsorContact();
		agreementSponsorContact.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		vo.setAgreementSponsorContact(agreementDao.saveOrUpdateAgreementSponsorContact(agreementSponsorContact));
		vo.setAgreementSponsors(getAgreementSponsors(agreementSponsorContact.getAgreementRequestId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String addToClausesBank(TemplateManagementVO vo) {
		ClausesBank clausesBank = vo.getClausesBank();
		clausesBank.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateClausesBank(clausesBank);
		vo.setClausesBanks(prepareClausesBank(agreementDao.getAllClausesBank()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadAllClausesBank() {
		TemplateManagementVO vo = new TemplateManagementVO();
		vo.setClausesBanks(prepareClausesBank(agreementDao.getAllClausesBank()));
		return commonDao.convertObjectToJSON(vo);
	}

	private List<ClausesBank> prepareClausesBank(List<ClausesBank> clausesBank) {
		if (clausesBank != null && !clausesBank.isEmpty()) {
			Collections.sort(clausesBank, (clausesBankOne,
					clausesBankTwo) -> clausesBankOne.getUpdateTimestamp().after(clausesBankTwo.getUpdateTimestamp())
							? -1
							: clausesBankOne.getUpdateTimestamp() == clausesBankTwo.getUpdateTimestamp() ? 0 : 1);
		}
		return clausesBank;
	}

	@Override
	public String deleteClausesById(Integer clauseCode) {
		TemplateManagementVO vo = new TemplateManagementVO();
		agreementDao.deleteClausesById(agreementDao.fetchClausesBankById(clauseCode));
		vo.setClausesBanks(prepareClausesBank(agreementDao.getAllClausesBank()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveAgreementPeople(AgreementVO vo) {
		AgreementPeople agreementPeople = vo.getAgreementPeople();
		Integer agreementRequestId = agreementPeople.getAgreementRequestId();
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementPeople.getAgreementRequestId());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		agreementPeople.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateAgreementPeople(agreementPeople);
		vo.setAgreementPeoples(preparePeopleDetails(agreementDao.getAllAgreementPeople(agreementRequestId)));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String agreementInvitation(EmailServiceVO emailServiceVO) {
		try {
			int limit = 0;
			int mailGroupRecipiantLimit = emailServiceVO.getRecipients().size() / 50;
			mailGroupRecipiantLimit = (int) Math.ceil(mailGroupRecipiantLimit / 100.0);
			while (limit <= mailGroupRecipiantLimit) {
				emailServiceVO.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
				emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
				emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
				emailServiceVO = emailService.sendEmail(emailServiceVO);
				limit++;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON("success");
	}

	@Override
	public List<AgreementPeople> preparePeopleDetails(List<AgreementPeople> agreementPeoples) {
		if (agreementPeoples != null && !agreementPeoples.isEmpty()) {
			for (AgreementPeople agreementPeople : agreementPeoples) {
				loadAgreementPeopleDetails(agreementPeople);
			}
		}
		return agreementPeoples;
	}

	private void loadAgreementPeopleDetails(AgreementPeople agreementPeople) {
		if (agreementPeople.getPersonId() != null) {
			Person person = personDao.getPersonDetailById(agreementPeople.getPersonId());
			if (person != null) {
				agreementPeople.setEmail(person.getEmailAddress());
				if (person.getHomeUnit() != null) {
						agreementPeople.setDepartment(person.getUnit().getUnitName());
				}
				agreementPeople.setPhoneNumber(person.getOfficePhone());
			}
		} else {
			Rolodex rolodex = rolodexDao.getRolodexDetailById(agreementPeople.getRolodexId());
			if (rolodex != null) {
				agreementPeople.setEmail(rolodex.getEmailAddress());
				if (rolodex.getOrganizations() != null) {
					agreementPeople.setDepartment(rolodex.getOrganizations().getOrganizationName());
				} else if (rolodex.getOrganizationName() != null) {
					agreementPeople.setDepartment(rolodex.getOrganizationName());
				}
				agreementPeople.setPhoneNumber(rolodex.getPhoneNumber());
			}
		}
	}

	@Override
	public String deleteAgreementPeople(AgreementVO vo) {
		AgreementPeople agreementPeople = agreementDao.getAgreementPeople(vo.getAgreementPeopleId());
		Integer agreementRequestId = agreementPeople.getAgreementRequestId();
		agreementDao.deleteAgreementPeople(agreementPeople);
		vo.setAgreementPeoples(preparePeopleDetails(agreementDao.getAllAgreementPeople(agreementRequestId)));
		return commonDao.convertObjectToJSON(vo);
	}

	private AgreementVO buildAgreementWorkflow(AgreementVO vo) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(Constants.AGREEMENT_MODULE_CODE);
		evaluateValidationRuleVO.setSubModuleCode(Constants.AGREEMENT_SUBMODULE_CODE);
		evaluateValidationRuleVO.setModuleItemKey(vo.getAgreementRequestId().toString());
		evaluateValidationRuleVO.setLogginPersonId(vo.getPersonId());
		evaluateValidationRuleVO.setUpdateUser(vo.getUpdateUser());
		evaluateValidationRuleVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			vo.setWorkflow(workflowDao.fetchActiveWorkflowByParams(vo.getAgreementRequestId().toString(),
					Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(),
				evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(),
				Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(),
				evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(),
				Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		vo.setCanApproveRouting(canApproveRouting.toString());
		vo.setIsFinalApprover(isFinalApprover);
		return vo;
	}

	private AgreementVO fetchPreviousWorkFlowsList(AgreementVO vo) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(vo.getAgreementRequestId().toString(),
				Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			vo.setWorkflowList(workFlows);
		}
		return vo;
	}

	@Override
	public String returnAgreement(MultipartFile[] files, String formDataJSON) {
		ObjectMapper mapper = new ObjectMapper();
		AgreementVO vo = null;
		try {
			vo = mapper.readValue(formDataJSON, AgreementVO.class);
			String updateUser = vo.getUpdateUser();
			String comment = vo.getComment();
			Integer agreementRequestId = vo.getAgreementRequestId();
			AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementRequestId);
			updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_RETURNED);
			agreementHeader.setWorkflowStatusCode(null);
			agreementHeader.setAgreementWorkflowStatus(null);
			agreementHeader.setNegotiationId(getNegotiationIdBasedOnAgreementId(vo.getAgreementRequestId()));
			vo.setAgreementHeader(agreementDao.saveOrUpdateAgreement(agreementHeader));
			if (agreementHeader.getUnitNumber() != null) {
				vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AGREEMENT_MODULE_CODE,
						vo.getPersonId(), agreementHeader.getUnitNumber(), agreementHeader.getAgreementRequestId()));
			} else {
				vo.setAvailableRights(authorizationService.allDepartmentPermission(Constants.AGREEMENT_MODULE_CODE,
						vo.getPersonId(), Constants.ROOT_UNIT, agreementHeader.getAgreementRequestId()));
			}
			AgreementActionLog agreementActionLog = addActionLogEntry(agreementRequestId, Constants.ACTION_LOG_AGREEMENT_ROUTING_RETURNED, updateUser, null);
			inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId.toString(),
					Constants.MESSAGE_TYPE_ROUTING_IN_PROGRESS);
			inboxDao.markAsReadBasedOnParams(Constants.AGREEMENT_MODULE_CODE, agreementRequestId.toString(),
					Constants.MESSAGE_TYPE_RETURNED);
			markAllMessagesAsRead(agreementRequestId.toString());
			inboxService.addAgreementMessageToInbox(agreementRequestId.toString(),
					agreementHeader.getRequestorPersonId(), updateUser, Constants.MESSAGE_TYPE_RETURNED, "R", 0, 0,
					"#" + agreementRequestId.toString() + " - " + agreementHeader.getTitle()+ " - " +agreementHeader.getAgreementType().getDescription());
			if (comment != null || (files != null && files.length > 0)) {
				AgreementNote agreementNote = new AgreementNote();
				agreementNote.setNote(vo.getComment());
				agreementNote.setUpdateUser(vo.getUpdateUser());
				if (updateUser != null) {
					agreementNote.setUpdateUserFullName(personDao.getUserFullNameByUserName(vo.getUpdateUser()));
				}
				agreementNote.setActionLogId(agreementActionLog.getActionLogId());
				agreementNote.setAgreementActionLog(agreementActionLog);
				agreementNote.setAgreementRequestId(agreementRequestId);
				agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				agreementCommentService.addAgreementAttachment(agreementNote, files);
				agreementDao.saveOrUpdateAgreementNotes(agreementNote);
			}
		} catch (Exception e) {
			logger.info("Exception while returning agreement");
		}
		/*if (vo != null && vo.getAgreementRequestId() != null) {
			vo.setAgreementComments(agreementCommentService.prepareAgreementComment(vo.getAgreementRequestId()));
		}*/
		return commonDao.convertObjectToJSON(vo);
	}

	private void getAllEditableSections(AgreementVO agreementVO) {
		Map<String, List<String>> sectionCodes = new HashMap<String, List<String>>();
		List<String> sections = agreementDao.getSectionCodesBasedOnRights(agreementVO.getAvailableRights());
		if (sections != null && !sections.isEmpty()) {
			sections.forEach(section -> {
				List<String> fields = agreementDao.getFieldCodesBasedOnSectionCodes(section);
				sectionCodes.put(section, fields);
			});
		}
		agreementVO.setSectionCodes(sectionCodes);
	}

	@Override
	public void printEntireAgreement(Integer agreementRequestId, HttpServletResponse response, String personId,
			String userName) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementRequestId);
		if (agreementHeader != null) {
			String fileName = "Agreement_Summary_#" +agreementRequestId;
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<AgreementAttachment> attachments = agreementDao
					.fetchAgreementAttachmentsBasedOnAgreementId(agreementRequestId, false);
			try {
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ZipOutputStream zos = new ZipOutputStream(baos);
				if (attachments != null && !attachments.isEmpty()) {
					for (AgreementAttachment attachment : attachments) {
						zos.putNextEntry(new ZipEntry(attachment.getFileName()));
						AgreementAttachmentFile fileData = agreementDao.getFileDataById(attachment.getAgreementAttachmentFileId());
						byte[] data = fileData.getFileData();
						zos.write(data);
					}
				}
				byte[] bFile = agreementPrintService.getPrintLetterTemplate(Constants.AGREEMENT_SUMMARY_LETTER_TEMPLATE_TYPE_CODE);
				byte[] mergedOutput = agreementPrintService.setMergePlaceHoldersOfAgreementSummary(bFile, agreementRequestId, personId, userName);
				zos.putNextEntry(new ZipEntry(fileName+ ".pdf"));
				zos.write(mergedOutput);
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
				logger.error("exception in printEntireProposal: {} ", e.getMessage());
			}
		}
	}

	@Override
	public String assignAgreementAdmin(AgreementVO vo, boolean isEntryNeeded) {
		String updateUser = vo.getUpdateUser();
		String loginUserId = vo.getLoginUserId();
		String assignePersonId = vo.getPersonId();
		Integer agreementRequestId = vo.getAgreementRequestId();
		markAllMessagesAsRead(agreementRequestId.toString());
		vo.setLoginUserName(updateUser);
		if (vo.getAgreementRequestId() != null && vo.getAgreementPeople() != null) {
			AgreementPeople agreementPeople = vo.getAgreementPeople();
			AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
			if (agreementHeader.getAdminPersonId() != null) {
				if (isEntryNeeded) {
					AgreementActionType agreementActionType = agreementDao.fetchAgreementActionTypeById(Constants.ACTION_LOG_REASSIGNED_TO_ADMIN);
					String message = agreementActionType.getMessage();
					message = message.replace("{ADMIN_ONE}", agreementHeader.getAdminName());
					message = message.replace("{ADMIN_TWO}", agreementPeople.getFullName());
					addActionLogEntry(agreementRequestId, Constants.ACTION_LOG_REASSIGNED_TO_ADMIN, updateUser, message);
				}
			} else {
				if (isEntryNeeded) {
					AgreementActionType agreementActionType = agreementDao.fetchAgreementActionTypeById(Constants.ACTION_LOG_ASSIGNED_TO_ADMIN);
					String message = agreementActionType.getMessage();
					message = message.replace("{ADMIN_ONE}", agreementPeople.getFullName());
					addActionLogEntry(agreementRequestId, Constants.ACTION_LOG_ASSIGNED_TO_ADMIN, updateUser, message);
				}
			}
			agreementHeader.setAdminName(agreementPeople.getFullName());
			agreementHeader.setAdminPersonId(agreementPeople.getPersonId());
			if (!agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_INPROGRESS)) {
				updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS);
				agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS);
				agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS));
			}
			agreementHeader.setUpdateUser(vo.getUpdateUser());
			agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementHeader.setAdminGroupId(vo.getAdminGroupId() != null ? vo.getAdminGroupId() : null);
			agreementHeader.setAgreementAdminGroup(vo.getAdminGroupId() != null ? commonDao.getAdminGroupByGroupId(vo.getAdminGroupId()) : null);
			agreementDao.saveOrUpdateAgreement(agreementHeader);
			if (!loginUserId.equals(assignePersonId)) {
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				commonService.setNotificationRecipients(assignePersonId, "TO", dynamicEmailrecipients);
				sendNotificationForAgreement(vo, Constants.AGREEMENT_ASSIGN_ADMIN_NOTIFICATION_CODE, dynamicEmailrecipients);
			}
			String userMessage = "#" + agreementHeader.getAgreementRequestId().toString() + " - " + agreementHeader.getTitle() + " - " + agreementHeader.getAgreementType().getDescription();
			Set<String> personIds = new HashSet<>();
			if (agreementHeader.getAdminGroupId() != null) {
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				String emailAddresses = commonDao.getAdminGroupEmailAddressByAdminGroupId(vo.getAdminGroupId());
				if (emailAddresses != null && !emailAddresses.isEmpty()) {
					List<String> recipients = Arrays.asList(emailAddresses.split(","));
					for (String recipient : recipients) {
						commonService.setNotificationRecipientsforNonEmployees(recipient, "TO", dynamicEmailrecipients);
					}								
				}
				sendNotificationForAgreement(vo, Constants.AGREEMENT_ASSIGN_ADMIN_GROUP_NOTIFICATION_CODE, dynamicEmailrecipients);
				if (agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS)) {
					personIds.addAll(personDao.getGroupAdminPersonIdsByRightName(Constants.ADMIN_GROUP_RIGHT, agreementHeader.getAdminGroupId()));					
					if (!personIds.contains(agreementHeader.getAdminPersonId())) {
						personIds.add(agreementHeader.getAdminPersonId());
					}
				}
			} else {
				if (agreementHeader.getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS) && !loginUserId.equals(assignePersonId)) {
					personIds.add(agreementHeader.getAdminPersonId());
				}
			}
			if (personIds != null && !personIds.isEmpty()) {
				personIds.forEach(personId -> {
					inboxService.addAgreementMessageToInbox(agreementHeader.getAgreementRequestId().toString(),
							personId, vo.getUpdateUser(), Constants.MESSAGE_TYPE_LOCATION_ASSIGNMENT, "P", 0,
							Constants.AGREEMENT_SUBMODULE_CODE, userMessage);
				});
			}
			//checkAgreementAdminExist(agreementPeople, vo.getUpdateUser());
		}
		return loadAgreementById(vo);
	}

//	private void checkAgreementAdminExist(AgreementPeople agreementPeople, String updateUser) {
//		AgreementPeople agreementPerson = agreementDao.checkPersonExist(agreementPeople.getAgreementRequestId(), agreementPeople.getPersonId(), agreementPeople.getPeopleTypeId());
//		if (agreementPerson != null) {
//			agreementPerson.setUpdateTimestamp(commonDao.getCurrentTimestamp());
//			agreementDao.saveOrUpdateAgreementPeople(agreementPerson);
//		} else {
//			agreementPeople.setUpdateTimestamp(commonDao.getCurrentTimestamp());
//			agreementDao.saveOrUpdateAgreementPeople(agreementPeople);
//		}
//	}

	@Override
	public String terminateAgreement(AgreementVO vo) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_TERMINATED);
		agreementHeader.setUpdateUser(vo.getUpdateUser());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementHeader.setWorkflowStatusCode(null);
		agreementHeader.setAgreementWorkflowStatus(null);
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		inboxDao.markAsExpiredFromActionList(Constants.MODULE_CODE_AGREEMENT,agreementHeader.getAgreementRequestId().toString(), Constants.SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
		AgreementActionLog agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(),Constants.ACTION_LOG_AGREEMENT_TERMINATED, vo.getUpdateUser(), null);
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		return loadAgreementById(vo);
	}

	@Override
	public String abandonAgreement(AgreementVO vo) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_ABANDONED);
		agreementHeader.setUpdateUser(vo.getUpdateUser());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementHeader.setWorkflowStatusCode(null);
		agreementHeader.setAgreementWorkflowStatus(null);
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		inboxDao.markAsExpiredFromActionList(Constants.MODULE_CODE_AGREEMENT,agreementHeader.getAgreementRequestId().toString(), Constants.SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
		AgreementActionLog agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(),Constants.ACTION_LOG_AGREEMENT_ABANDONED, vo.getUpdateUser(), null);
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		return loadAgreementById(vo);
	}

	@Override
	public String transferAgreement(AgreementVO vo) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_TRASFERRED);
		agreementHeader.setUpdateUser(vo.getUpdateUser());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementHeader.setWorkflowStatusCode(null);
		agreementHeader.setAgreementWorkflowStatus(null);
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		inboxDao.markAsExpiredFromActionList(Constants.MODULE_CODE_AGREEMENT,agreementHeader.getAgreementRequestId().toString(), Constants.SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);
		AgreementActionLog agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(),Constants.ACTION_LOG_AGREEMENT_TRANSFERRED, vo.getUpdateUser(), null);
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		return loadAgreementById(vo);
	}

	@Override
	public String reopenAgreement(AgreementVO vo) {
		AgreementActionLog agreementActionLog = new AgreementActionLog();
		AgreementActionType agreementActionType = agreementDao.fetchAgreementActionTypeById(Constants.ACTION_LOG_AGREEMENT_REOPEN);
		String message = agreementActionType.getMessage();
		message = message.replace("{ADMIN_NAME}", vo.getAgreementPeople().getFullName());
		agreementActionLog = addActionLogEntry(vo.getAgreementRequestId(), Constants.ACTION_LOG_AGREEMENT_REOPEN, vo.getUpdateUser(), message);
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		updateAgreementStatus(agreementHeader, Constants.AGREEMENT_STATUS_REVIEW_INPROGRESS);
		agreementHeader.setWorkflowStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS);
		agreementHeader.setAgreementWorkflowStatus(agreementDao.fetchAgreementWorkflowStatusByStatusCode(Constants.AGREEMENT_WORKFLOW_INPROGRESS));
		agreementHeader.setUpdateUser(vo.getUpdateUser());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		markAllMessagesAsRead(vo.getAgreementRequestId().toString());
		AgreementNote agreementNote = vo.getAgreementNote();
		if (agreementNote != null && agreementNote.getNote() != null) {
			agreementNote.setActionLogId(agreementActionLog.getActionLogId());
			agreementNote.setAgreementActionLog(agreementActionLog);
			agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			vo.setAgreementNote(agreementDao.saveOrUpdateAgreementNotes(agreementNote));
		}
		assignAgreementAdmin(vo, false);
		return loadAgreementById(vo);
	}

	@Override
	public void updateAgreementStatus(AgreementHeader agreementHeader, String agreementStatusCode) {
		agreementHeader.setAgreementStatusCode(agreementStatusCode);
		agreementHeader.setAgreementStatus(agreementDao.fetchAgreementStatusByStatusCode(agreementStatusCode));
	}
	
	@Override
	public String linkModuleToAgreement(AgreementLinkModuleVO vo) {
		if(vo.getModuleCode() != null && vo.getModuleItemKey() != null && vo.getAgreementRequestId() != null) {
			Boolean isAlreadyLinked = agreementDao.checkIfExternalModuleLinked(vo.getAgreementRequestId());
			String acType = vo.getAcType();
			if (acType != null && acType.equalsIgnoreCase(Constants.acTypeDelete)) {
				agreementDao.deleteAgreementLink(vo.getModuleCode(), vo.getModuleItemKey(), vo.getAgreementRequestId());
			} else {
				AgreementAssociationDetail associationDetail = prepareAgreementAssociationDetail(vo.getModuleCode(), vo.getModuleItemKey());
				AgreementAssociationLink moduleLink = new AgreementAssociationLink();
				if(vo.getAgreementRequestId() != null) {
					moduleLink.setAgreementRequestId(vo.getAgreementRequestId());
					AgreementHeader agreement = agreementDao.getAgreementById(vo.getAgreementRequestId());
					if (Boolean.FALSE.equals(isAlreadyLinked)) {
						agreement.setTitle(agreement.getTitle() == null ? associationDetail.getTitle() : agreement.getTitle());
						agreement.setStartDate(agreement.getStartDate() == null ? associationDetail.getStartDate() : agreement.getStartDate());
						agreement.setEndDate(agreement.getEndDate() == null ? associationDetail.getEndDate() : agreement.getEndDate());
						agreement.setUnitNumber(agreement.getUnitNumber() == null ? associationDetail.getLeadUnitNumber() : agreement.getUnitNumber());
						agreement.setUnitName(agreement.getUnitName() == null ? associationDetail.getLeadUnitName() : agreement.getUnitName());
						agreement.setUnit(agreement.getUnit() == null ? commonDao.getLeadUnitByUnitNumber(associationDetail.getLeadUnitNumber()) : agreement.getUnit());
						agreement = agreementDao.saveOrUpdateAgreement(agreement);
						if ((associationDetail.getPersonId() != null || associationDetail.getRolodexId() != null) && Constants.IMPORT_PI_DETAILS) {
							prepareAndSaveAgreementPIDetails(agreement.getAgreementRequestId(), associationDetail.getPersonId(), associationDetail.getRolodexId(), vo.getUpdateUser());
						}
						if (associationDetail.getSponsorCode() != null && Constants.IMPORT_SPONSOR_DETAILS) {
							prepareAndSaveAgreementSponsor(agreement.getAgreementRequestId(), associationDetail.getSponsorCode(), vo.getUpdateUser());
						}
						if (associationDetail.getContractAdminPersonId() != null && Constants.IMPORT_CA_DETAILS) {
							prepareAndSaveContractAdminDetails(agreement.getAgreementRequestId(), associationDetail.getContractAdminPersonId(), vo.getUpdateUser());
						}
						if (associationDetail.getPrimeSponsorCode() != null  && !associationDetail.getPrimeSponsorCode().equals("") && Constants.IMPORT_PRIME_SPONSOR_DETAILS) {
							prepareAndSavePrimeSponsorDetails(agreement.getAgreementRequestId(), associationDetail.getPrimeSponsorCode(), vo.getUpdateUser());
						}
					}
					vo.setAgreementPeoples(preparePeopleDetails(agreementDao.getAllAgreementPeople(agreement.getAgreementRequestId())));
					vo.setAgreementSponsors(getAgreementSponsors(agreement.getAgreementRequestId()));
					agreement.setNegotiationId(getNegotiationIdBasedOnAgreementId(vo.getAgreementRequestId()));
					vo.setAgreementHeader(agreement);
				} else {
					Integer agreementHeaderId = createAgreementDetail(vo, associationDetail);
					moduleLink.setAgreementRequestId(agreementHeaderId);
				}
				moduleLink.setModuleCode(vo.getModuleCode());
				moduleLink.setModuleItemKey(vo.getModuleItemKey());
				moduleLink.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				moduleLink.setUpdateUser(vo.getUpdateUser());
				agreementDao.saveOrUpdateAgreementAssociationLink(moduleLink);
			}
		}
		List<AgreementAssociationLink> linkDetails = agreementDao.getExternalModuleLinkDetails(vo.getAgreementRequestId());
		vo.setAgreementAssociationDetails(linkDetails != null ? getExternalModuleDetails(linkDetails) : new ArrayList<>());
		return commonDao.convertObjectToJSON(vo);
	}

	public Integer createAgreementDetail(AgreementLinkModuleVO vo, AgreementAssociationDetail associationDetail) {
		AgreementHeader agreementHeader = new AgreementHeader();
		agreementHeader.setPiFullName(associationDetail.getPiName());
		agreementHeader.setTitle(associationDetail.getTitle());
		agreementHeader.setAgreementStatusCode(Constants.AGREEMENT_STATUS_INPROGRESS);
		agreementHeader.setAgreementStatus(agreementDao.fetchAgreementStatusByStatusCode(Constants.AGREEMENT_STATUS_INPROGRESS));
		agreementHeader.setStartDate(associationDetail.getStartDate());
		agreementHeader.setEndDate(associationDetail.getEndDate());
		agreementHeader.setUnitName(associationDetail.getLeadUnitName());
		agreementHeader.setUnitNumber(associationDetail.getLeadUnitNumber());
		agreementHeader.setUnit(associationDetail.getLeadUnitNumber() != null ? commonDao.getLeadUnitByUnitNumber(associationDetail.getLeadUnitNumber()) : null);
		agreementHeader = agreementDao.saveOrUpdateAgreement(agreementHeader);
		if ((associationDetail.getPersonId() != null || associationDetail.getRolodexId() != null)) {
			prepareAndSaveAgreementPIDetails(agreementHeader.getAgreementRequestId(), associationDetail.getPersonId(), associationDetail.getRolodexId(), vo.getUpdateUser());
			vo.setAgreementPeoples(preparePeopleDetails(agreementDao.getAllAgreementPeople(agreementHeader.getAgreementRequestId())));
		}
		if (associationDetail.getSponsorCode() != null) {
			prepareAndSaveAgreementSponsor(agreementHeader.getAgreementRequestId(), associationDetail.getSponsorCode(), agreementHeader.getUpdateUser());
			vo.setAgreementSponsors(getAgreementSponsors(agreementHeader.getAgreementRequestId()));
		}
		vo.setAgreementHeader(agreementHeader);
		return agreementHeader.getAgreementRequestId();
	}

	public List<AgreementAssociationDetail> getExternalModuleDetails(List<AgreementAssociationLink> linkDetails) {
		List<AgreementAssociationDetail> associationDetails = new ArrayList<>();
		for (AgreementAssociationLink linkDetail : linkDetails) {	
			AgreementAssociationDetail associationDetail = prepareAgreementAssociationDetail(linkDetail.getModuleCode(), linkDetail.getModuleItemKey());		
			associationDetails.add(associationDetail);
		}
		return associationDetails;
	}

	private AgreementAssociationDetail prepareAgreementAssociationDetail(Integer moduleCode, String moduleItemKey) {
		return prepareModuleDetails(new AgreementAssociationDetail(),moduleCode, Integer.parseInt(moduleItemKey));	
	}

	private AgreementAssociationDetail prepareModuleDetails(AgreementAssociationDetail associationDetail, Integer moduleCode, Integer moduleItemKey) {
		try {
			Object[] entity = commonDao.fetchModuleDetailsBasedOnId(moduleCode, moduleItemKey);
			if (entity != null) {
				associationDetail.setAccountNumber((entity[0] != null && !entity[0].toString().equals("")) ? entity[0].toString() : null);
				associationDetail.setLeadUnitName(entity[1] != null ? entity[1].toString() : null);
				associationDetail.setLeadUnitNumber(entity[2] != null ? entity[2].toString(): null);
				associationDetail.setTitle(entity[3] != null ? entity[3].toString() : null);
				associationDetail.setStatus(entity[4] != null ? entity[4].toString() : null);
				associationDetail.setPiName(entity[5] != null ?entity[5].toString() : null);
				associationDetail.setSponsorName(entity[6] != null ? entity[6].toString() : null);
				associationDetail.setModuleItemKey(entity[7] != null ? entity[7].toString() : null);
				associationDetail.setModuleCode(entity[8] != null ? Integer.parseInt(entity[8].toString()) : null);
				associationDetail.setModuleItemId(entity[9] != null ? Integer.parseInt(entity[9].toString()) : null);
				associationDetail.setPersonId(entity[10] != null ? entity[10].toString() : null);
				associationDetail.setSponsorCode(entity[11] != null ? entity[11].toString() : null);
				associationDetail.setStartDate(entity[12] != null ? Timestamp.valueOf(entity[12].toString()) : null);
				associationDetail.setEndDate(entity[13] != null ? Timestamp.valueOf(entity[13].toString()) : null);
				associationDetail.setPrimeSponsorCode((entity[14] != null && entity[0].toString().equals("")) ? entity[14].toString() : null);
				associationDetail.setRolodexId(entity[15] != null ? entity[15].toString() : null);
			}
			return associationDetail;
		} catch (Exception e) {
			e.printStackTrace();
			return associationDetail;
		}
	}

	private void prepareAndSaveAgreementPIDetails(Integer agreementRequestId, String personId, String rolodexId, String updateUser) {
		AgreementPeople agreementPI = agreementDao.getAgreementPI(agreementRequestId);
		if (agreementPI == null && (personId != null || rolodexId != null)) {
			AgreementPeople agreementPeople = new AgreementPeople();
			if (personId != null) {
				Person person = personDao.getPersonDetailById(personId);
				agreementPeople.setFullName(person.getFullName());
				agreementPeople.setPersonId(person.getPersonId());
			} else {
				Rolodex rolodex = rolodexDao.getRolodexDetailById(Integer.parseInt(rolodexId));
				agreementPeople.setFullName(rolodex.getFullName());
				agreementPeople.setRolodexId(rolodex.getRolodexId());
			}
			agreementPeople.setPeopleTypeId(Constants.PI_ROLE_CODE);
			agreementPeople.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementPeople.setUpdateUser(updateUser);
			agreementPeople.setAgreementRequestId(agreementRequestId);
			agreementPeople.setAgreementPeopleType(agreementDao.getAgreementPeopleTypeById(3));
			agreementDao.saveOrUpdateAgreementPeople(agreementPeople);
		}
	}

	private void prepareAndSaveAgreementSponsor(Integer agreementRequestId, String sponsorCode, String updateUser) {
		List<AgreementSponsor> agreementSponsors = new ArrayList<>();
		agreementSponsors = agreementDao.getAgreementSponsors(agreementRequestId);
		if (agreementSponsors == null || agreementSponsors.isEmpty()) {
			AgreementSponsor agreementSponsor = new AgreementSponsor();
			Sponsor sponsor = commonDao.getSponsorById(sponsorCode);
			agreementSponsor.setSponsorCode(sponsorCode);
			agreementSponsor.setSponsorName(sponsor.getSponsorName());
			agreementSponsor.setSponsor(sponsor);
			agreementSponsor.setAgreementRequestId(agreementRequestId);
			agreementSponsor.setSponsorRoleTypeCode("1");
			agreementSponsor.setSponsorRole(commonDao.getSponsorRoleByRoleTypeCode("1"));
			agreementSponsor.setAgreementSponsorTypeCode("1");
			agreementSponsor.setAgreementSponsorType(agreementDao.getAgreementSponsorTypeByTypeCode("1"));
			agreementSponsor.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementSponsor.setUpdateUser(updateUser);
			agreementDao.saveOrUpdateAgreementSponsor(agreementSponsor);
		}
	}

	@Override
	public String deleteClausesGroup(Integer clausesGroupCode) {
		agreementDao.deleteAgreementTypeClausesMappingByClauseGroupCode(clausesGroupCode);
		agreementDao.deleteAgreementClausesByClauseGroupCode(clausesGroupCode);
		agreementDao.deleteClausesGroup(clausesGroupCode);
		return commonDao.convertObjectToJSON("Clauses Group deleted successfully");
	}

	@Override
	public String getPersonGroup(String personId) {
		List<Integer> adminGroupIds = commonDao.getAdminGroupIdsBasedOnPersonId(personId);
		if (adminGroupIds != null && !adminGroupIds.isEmpty()) {
			if (adminGroupIds.size() == 1) {
				return commonDao.convertObjectToJSON(commonDao.getAdminGroupByGroupId(adminGroupIds.get(0)));
			} else {
				return commonDao.convertObjectToJSON("The selected admin is assigned to multiple Admin Groups");
			}
		}
		return commonDao.convertObjectToJSON("The selected admin is not assigned to any Admin Group");
	}

	private void prepareAndSaveContractAdminDetails(Integer agreementRequestId, String personId, String updateUser) {
		List<AgreementPeople> contractAdmins = agreementDao.getAgreementPeopleByTypeId(agreementRequestId, Constants.CONTRACT_ADMIN_TYPE_ID);
		Boolean contractAdminExist = false;
		if (contractAdmins != null && !contractAdmins.isEmpty()) {
			for (AgreementPeople contractAdmin : contractAdmins) {
				if (contractAdmin.getPersonId().equals(personId)) {
					contractAdminExist = true;
				}
			}
		}
		if (contractAdmins == null || contractAdmins.isEmpty() || Boolean.FALSE.equals(contractAdminExist)) {
			AgreementPeople agreementPeople = new AgreementPeople();
			Person person = personDao.getPersonDetailById(personId);
			agreementPeople.setPeopleTypeId(Constants.CONTRACT_ADMIN_TYPE_ID);
			agreementPeople.setFullName(person.getFullName());
			agreementPeople.setDepartment(person.getHomeUnit());
			agreementPeople.setPersonId(person.getPersonId());
			agreementPeople.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			agreementPeople.setUpdateUser(updateUser);
			agreementPeople.setAgreementRequestId(agreementRequestId);
			agreementPeople.setAgreementPeopleType(agreementDao.getAgreementPeopleTypeById(Constants.CONTRACT_ADMIN_TYPE_ID));
			agreementDao.saveOrUpdateAgreementPeople(agreementPeople);
		}
	}

	private void prepareAndSavePrimeSponsorDetails(Integer agreementRequestId, String sponsorCode, String updateUser) {
		AgreementSponsor agreementSponsor = new AgreementSponsor();
		Sponsor sponsor = commonDao.getSponsorById(sponsorCode);
		agreementSponsor.setSponsorCode(sponsorCode);
		agreementSponsor.setSponsorName(sponsor.getSponsorName());
		agreementSponsor.setSponsor(sponsor);
		agreementSponsor.setAgreementRequestId(agreementRequestId);
		agreementSponsor.setSponsorRoleTypeCode(Constants.PRIME_SPONSOR_ROLE);
		agreementSponsor.setSponsorRole(commonDao.getSponsorRoleByRoleTypeCode(Constants.PRIME_SPONSOR_ROLE));
		agreementSponsor.setAgreementSponsorTypeCode(Constants.SPONSOR_TYPE_OTHER);
		agreementSponsor.setAgreementSponsorType(agreementDao.getAgreementSponsorTypeByTypeCode(Constants.SPONSOR_TYPE_OTHER));
		agreementSponsor.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementSponsor.setUpdateUser(updateUser);
		agreementDao.saveOrUpdateAgreementSponsor(agreementSponsor);
	}
	
	@Override
	public String updateAttachmentDetails(AgreementVO vo) {
	    agreementDao.updateAttachmentDetails(vo.getAgreementAttachment().getDescription(), vo.getAgreementAttachment().getAgreementAttachmentId());
	    AgreementAttachment agreementAttachment = agreementDao.fetchAttachmentById(vo.getAgreementAttachment().getAgreementAttachmentId());
	    agreementAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(AuthenticatedUser.getLoginUserName()));
		return commonDao.convertObjectToJSON(agreementAttachment);
	}
	
	@Override
	public String deleteAgreement(Integer agreementRequestId) {
		AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementRequestId);
		String leadUnitNumber = agreementHeader.getUnitNumber();
		Boolean deleteAgreement = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), "DELETE_AGREEMENT", leadUnitNumber);
		if ((deleteAgreement != null && Boolean.TRUE.equals(deleteAgreement)) || agreementHeader.getRequestorPersonId().equals(AuthenticatedUser.getLoginPersonId())) {
			return commonDao.convertObjectToJSON(agreementDao.deleteAgreement(agreementRequestId));
		} else {
			return commonDao.convertObjectToJSON("You dont have the right to delete this agreement");
		}
	}
	
	@Override
	public void canAgreementTakeRoutingAction(AgreementVO agreementvo) {
		Workflow workflow = agreementvo.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(agreementvo.getAgreementRequestId().toString(),
					Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY,
					Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		}
		if (workflow != null) {
			agreementvo.getAgreementHeader()
					.setSubmitUserFullName(personDao.getPersonFullNameByPersonId(workflow.getWorkflowStartPerson()));
			agreementvo.getAgreementHeader().setUpdateUserFullName(
					personDao.getUserFullNameByUserName(agreementvo.getAgreementHeader().getCreateUser()));
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(),
					maxApprovalStopNumber);
			for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
				if (finalWorkflowDetail.getApproverPersonId().equals(AuthenticatedUser.getLoginPersonId())
						|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
					agreementvo.setFinalApprover(true);
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				if (agreementvo.getAgreementHeader().getAgreementStatusCode().equals(Constants.AGREEMENT_STATUS_ROUTING)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (workflowDetail.getApproverPersonId().equals(AuthenticatedUser.getLoginPersonId())) {
							if (workflowDetail.getApprovalStatusCode()
									.equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
								agreementvo.setIsApproved(true);
							} else {
								agreementvo.setIsApproved(false);
							}
							agreementvo.setIsApprover(true);
						}
					}
				}
			}
		}
	}



}
