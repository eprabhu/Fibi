package com.polus.fibicomp.agreements.vo;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.agreements.dto.AgreementClausesGroup;
import com.polus.fibicomp.agreements.dto.AgreementComment;
import com.polus.fibicomp.agreements.dto.AgreementHistory;
import com.polus.fibicomp.agreements.dto.AgreementMode;
import com.polus.fibicomp.agreements.dto.QuestionnaireAttachment;
import com.polus.fibicomp.agreements.dto.Requestor;
import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.agreements.pojo.AgreementActionLog;
import com.polus.fibicomp.agreements.pojo.AgreementActionType;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationDetail;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationLink;
import com.polus.fibicomp.agreements.pojo.AgreementAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentType;
import com.polus.fibicomp.agreements.pojo.AgreementCategory;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementNote;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementPeopleType;
import com.polus.fibicomp.agreements.pojo.AgreementReviewType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContact;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContactType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorType;
import com.polus.fibicomp.agreements.pojo.AgreementStatus;
import com.polus.fibicomp.agreements.pojo.AgreementType;
import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.agreements.pojo.SponsorRole;
import com.polus.fibicomp.negotiation.pojo.NegotiationLocationStatus;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivityType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsComment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnelType;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.AgreementLinkModule;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.prereview.pojo.PreReviewSectionType;
import com.polus.fibicomp.vo.BaseVO;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

public class AgreementVO  extends BaseVO {

	private Integer agreementRequestId;

	private AgreementHeader agreementHeader;

	private AgreementSponsor agreementSponsor;

	private AgreementNote agreementNote;

	private AgreementAttachment agreementAttachment;

	private AgreementMode agreementMode;

	private Integer moduleCode;

	private String moduleItemKey;

	private Person person;

	private List<AgreementSponsor> agreementSponsors;

	private List<AgreementActionLog> agreementActionLogList;

	private List<AgreementAttachment> agreementAttachments;

	private List<NegotiationsAttachment> negotiationsAttachments;

	private List<AgreementType> agreementTypes;

	private List<AgreementActionType> agreementActionTypeList;

	private List<AgreementStatus> agreementStatuses;

	private List<AgreementSponsorContactType> agreementSponsorContactTypes;

	private List<AgreementAttachmentType> agreementAttachmentTypes;

	private List<NegotiationsLocation> negotiationsLocations;

	private List<NegotiationsLocationType> negotiationsLocationTypes;

	private List<NegotiationLocationStatus> negotiationLocationStatuses;

	private List<NegotiationsActivity> negotiationsActivities;

	private List<NegotiationsActivityType> negotiationsActivityTypes;

	private List<NegotiationsPersonnelType> negotiationsPersonnelTypes;

	private List<AgreementTypeTemplate> agreementTypeTemplates;

	private Requestor requestor;

	private String loginUserName;

	private String message;

	private List<AgreementAttachment> newAttachments;

	private String updateUser;

	private List<AgreementCategory> agreementCategories;

	private List<Currency> currencies;

	private Integer agreementAttachmentId;

	private Integer documentId;

	private Integer agreementSponsorId;

	private Integer negotiationId;

	private List<AgreementTypeTemplate> newAgreementTypeTemplate;

	private String agreementTypeCode;

	private String piPersonId;

	private String negotiatorPersonId;

	private String piPersonnelTypeCode;

	private AgreementClauses agreementClause;

	private List<AgreementClauses> agreementClauses;

	private Map<String, List<AgreementHistory>> agreementHistoryDetails;

	private Integer agreementClauseId;

	private String personId;

	private String agreementType;

	private Boolean canGenerateAgreement = false;

	private List<String> availableRights;

	private List<AgreementClausesGroup> agreementClausesGroup;

	private Boolean isAgreementCreatedOnce = false;

	private NegotiationsComment negotiationsComment;

	private Boolean isGeneratedAgreement = false;

	private Integer clausesGroupCode;

	private Boolean isUserHaveReview = false;

	private Boolean isFinal = false;

	private String comment;

	private Integer negotiationLocationId;

	private List<NegotiationsComment> negotiationsComments;

	private Integer agreementSponsorContactId;

	private AgreementSponsorContact agreementSponsorContact;

	private String assigneePersonId;

	private List<AgreementComment> agreementComments;

	private Integer commentId;

	private Boolean isLocationComment = false;

	private Integer attachmentId;

	private Boolean isAttachmentDelete = false;

	private List<AgreementPeopleType> agreementPeopleType;

	private AgreementPeople agreementPeople;

	private List<AgreementPeople> agreementPeoples;

	private Integer agreementPeopleId;

	private String workFlowPersonId;

	private String actionType;

	private String approveComment;

	private String isFinalApprover;

	private List<Workflow> workflowList;

	private String canApproveRouting;

	private Workflow workflow;

	private Integer workflowDetailId;

	private WorkflowDetail workflowDetail;

	private Boolean finalApprover;

	private Boolean isApproved;

	private Boolean isApprover;

	private Integer approverStopNumber;

	private Integer mapId;

	private Integer mapNumber;

	private Integer approverNumber;

	private boolean isPrimaryApprover = false;

	private String approverFlag;

	private Integer workFlowId;

	private Integer approvalStopNumber;

	private String approverPersonId;

	private String approvalStatus;

	private Integer subModuleCode;

	private Integer subModuleItemKey;

	private Integer versionNumber;

	private Map<String, List<String>> sectionCodes;

	private List<AgreementNote> agreementNotes;

	private Integer actionLogId;

	private List<QuestionnaireAttachment> questionnnaireAttachment;

	private Boolean isTypeChanged = false;

	private Set<String> agreementTypeCodes;

	private List<PreReviewSectionType> preReviewClarifications;

	private List<SponsorRole> sponsorRoles;

	private List<AgreementSponsorType> agreementSponsorTypes;

	private List<SponsorType> sponsorTypes;

	private Integer triageHeaderId;

	private String agreementReviewTypeCode;

	private List<AgreementReviewType> agreementReviewTypes;

	private Set<Person> persons;

	private String locationTypeCode;

	private String loginUserId;

	private List<AgreementLinkModule> moduleList;

	private List<AgreementAssociationDetail> agreementAssociationDetails;

	private List<AgreementAssociationLink> moduleLinkDetails;

	private List<AdminGroup> agreementAdminGroups;

	private Integer adminGroupId;
	
	List<AgreementHistory> agreementHistories;

	public AgreementVO() {
		agreementHeader = new AgreementHeader();
		agreementSponsors = new ArrayList<>();
		negotiationsActivities = new ArrayList<>();
		agreementClauses = new ArrayList<>();
		availableRights = new ArrayList<>();
		agreementClausesGroup = new ArrayList<>();
		negotiationsLocations = new ArrayList<>();
		negotiationsComments = new ArrayList<>();
		agreementComments = new ArrayList<>();
		agreementPeopleType = new ArrayList<>();
		agreementPeoples = new ArrayList<>();
		workflowList = new ArrayList<>();
		agreementAttachments = new ArrayList<>();
		agreementNotes = new ArrayList<>();
		agreementTypeCodes = new HashSet<>();
		preReviewClarifications = new ArrayList<>();
		sponsorRoles = new ArrayList<>();
		agreementSponsorTypes = new ArrayList<>();
		sponsorTypes = new ArrayList<>();
		agreementReviewTypes = new ArrayList<>();
		persons = new HashSet<>();
	}

	public List<SponsorType> getSponsorTypes() {
		return sponsorTypes;
	}

	public void setSponsorTypes(List<SponsorType> sponsorTypes) {
		this.sponsorTypes = sponsorTypes;
	}

	public List<AgreementNote> getAgreementNotes() {
		return agreementNotes;
	}

	public void setAgreementNotes(List<AgreementNote> agreementNotes) {
		this.agreementNotes = agreementNotes;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public AgreementHeader getAgreementHeader() {
		return agreementHeader;
	}

	public void setAgreementHeader(AgreementHeader agreementHeader) {
		this.agreementHeader = agreementHeader;
	}

	public AgreementSponsor getAgreementSponsor() {
		return agreementSponsor;
	}

	public void setAgreementSponsor(AgreementSponsor agreementSponsor) {
		this.agreementSponsor = agreementSponsor;
	}

	public AgreementNote getAgreementNote() {
		return agreementNote;
	}

	public void setAgreementNote(AgreementNote agreementNote) {
		this.agreementNote = agreementNote;
	}

	public AgreementAttachment getAgreementAttachment() {
		return agreementAttachment;
	}

	public void setAgreementAttachment(AgreementAttachment agreementAttachment) {
		this.agreementAttachment = agreementAttachment;
	}

	public AgreementMode getAgreementMode() {
		return agreementMode;
	}

	public void setAgreementMode(AgreementMode agreementMode) {
		this.agreementMode = agreementMode;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public List<AgreementSponsor> getAgreementSponsors() {
		return agreementSponsors;
	}

	public void setAgreementSponsors(List<AgreementSponsor> agreementSponsors) {
		this.agreementSponsors = agreementSponsors;
	}

	public List<AgreementActionLog> getAgreementActionLogList() {
		return agreementActionLogList;
	}

	public void setAgreementActionLogList(List<AgreementActionLog> agreementActionLogList) {
		this.agreementActionLogList = agreementActionLogList;
	}

	public List<AgreementAttachment> getAgreementAttachments() {
		return agreementAttachments;
	}

	public void setAgreementAttachments(List<AgreementAttachment> agreementAttachments) {
		this.agreementAttachments = agreementAttachments;
	}

	public List<NegotiationsAttachment> getNegotiationsAttachments() {
		return negotiationsAttachments;
	}

	public void setNegotiationsAttachments(List<NegotiationsAttachment> negotiationsAttachments) {
		this.negotiationsAttachments = negotiationsAttachments;
	}

	public List<AgreementType> getAgreementTypes() {
		return agreementTypes;
	}

	public void setAgreementTypes(List<AgreementType> agreementTypes) {
		this.agreementTypes = agreementTypes;
	}

	public List<AgreementActionType> getAgreementActionTypeList() {
		return agreementActionTypeList;
	}

	public void setAgreementActionTypeList(List<AgreementActionType> agreementActionTypeList) {
		this.agreementActionTypeList = agreementActionTypeList;
	}

	public List<AgreementStatus> getAgreementStatuses() {
		return agreementStatuses;
	}

	public void setAgreementStatuses(List<AgreementStatus> agreementStatuses) {
		this.agreementStatuses = agreementStatuses;
	}

	public List<AgreementSponsorContactType> getAgreementSponsorContactTypes() {
		return agreementSponsorContactTypes;
	}

	public void setAgreementSponsorContactTypes(List<AgreementSponsorContactType> agreementSponsorContactTypes) {
		this.agreementSponsorContactTypes = agreementSponsorContactTypes;
	}

	public List<AgreementAttachmentType> getAgreementAttachmentTypes() {
		return agreementAttachmentTypes;
	}

	public void setAgreementAttachmentTypes(List<AgreementAttachmentType> agreementAttachmentTypes) {
		this.agreementAttachmentTypes = agreementAttachmentTypes;
	}

	public List<NegotiationsLocation> getNegotiationsLocations() {
		return negotiationsLocations;
	}

	public void setNegotiationsLocations(List<NegotiationsLocation> negotiationsLocations) {
		this.negotiationsLocations = negotiationsLocations;
	}

	public List<NegotiationsLocationType> getNegotiationsLocationTypes() {
		return negotiationsLocationTypes;
	}

	public void setNegotiationsLocationTypes(List<NegotiationsLocationType> negotiationsLocationTypes) {
		this.negotiationsLocationTypes = negotiationsLocationTypes;
	}

	public List<NegotiationLocationStatus> getNegotiationLocationStatuses() {
		return negotiationLocationStatuses;
	}

	public void setNegotiationLocationStatuses(List<NegotiationLocationStatus> negotiationLocationStatuses) {
		this.negotiationLocationStatuses = negotiationLocationStatuses;
	}

	public List<NegotiationsActivity> getNegotiationsActivities() {
		return negotiationsActivities;
	}

	public void setNegotiationsActivities(List<NegotiationsActivity> negotiationsActivities) {
		this.negotiationsActivities = negotiationsActivities;
	}

	public List<NegotiationsActivityType> getNegotiationsActivityTypes() {
		return negotiationsActivityTypes;
	}

	public void setNegotiationsActivityTypes(List<NegotiationsActivityType> negotiationsActivityTypes) {
		this.negotiationsActivityTypes = negotiationsActivityTypes;
	}

	public List<NegotiationsPersonnelType> getNegotiationsPersonnelTypes() {
		return negotiationsPersonnelTypes;
	}

	public void setNegotiationsPersonnelTypes(List<NegotiationsPersonnelType> negotiationsPersonnelTypes) {
		this.negotiationsPersonnelTypes = negotiationsPersonnelTypes;
	}

	public List<AgreementTypeTemplate> getAgreementTypeTemplates() {
		return agreementTypeTemplates;
	}

	public void setAgreementTypeTemplates(List<AgreementTypeTemplate> agreementTypeTemplates) {
		this.agreementTypeTemplates = agreementTypeTemplates;
	}

	public Requestor getRequestor() {
		return requestor;
	}

	public void setRequestor(Requestor requestor) {
		this.requestor = requestor;
	}

	public String getLoginUserName() {
		return loginUserName;
	}

	public void setLoginUserName(String loginUserName) {
		this.loginUserName = loginUserName;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<AgreementAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<AgreementAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<AgreementCategory> getAgreementCategories() {
		return agreementCategories;
	}

	public void setAgreementCategories(List<AgreementCategory> agreementCategories) {
		this.agreementCategories = agreementCategories;
	}

	public List<Currency> getCurrencies() {
		return currencies;
	}

	public void setCurrencies(List<Currency> currencies) {
		this.currencies = currencies;
	}

	public Integer getAgreementAttachmentId() {
		return agreementAttachmentId;
	}

	public void setAgreementAttachmentId(Integer agreementAttachmentId) {
		this.agreementAttachmentId = agreementAttachmentId;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getAgreementSponsorId() {
		return agreementSponsorId;
	}

	public void setAgreementSponsorId(Integer agreementSponsorId) {
		this.agreementSponsorId = agreementSponsorId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public List<AgreementTypeTemplate> getNewAgreementTypeTemplate() {
		return newAgreementTypeTemplate;
	}

	public void setNewAgreementTypeTemplate(List<AgreementTypeTemplate> newAgreementTypeTemplate) {
		this.newAgreementTypeTemplate = newAgreementTypeTemplate;
	}

	public String getAgreementTypeCode() {
		return agreementTypeCode;
	}

	public void setAgreementTypeCode(String agreementTypeCode) {
		this.agreementTypeCode = agreementTypeCode;
	}

	public String getPiPersonId() {
		return piPersonId;
	}

	public void setPiPersonId(String piPersonId) {
		this.piPersonId = piPersonId;
	}

	public String getNegotiatorPersonId() {
		return negotiatorPersonId;
	}

	public void setNegotiatorPersonId(String negotiatorPersonId) {
		this.negotiatorPersonId = negotiatorPersonId;
	}

	public String getPiPersonnelTypeCode() {
		return piPersonnelTypeCode;
	}

	public void setPiPersonnelTypeCode(String piPersonnelTypeCode) {
		this.piPersonnelTypeCode = piPersonnelTypeCode;
	}

	public AgreementClauses getAgreementClause() {
		return agreementClause;
	}

	public void setAgreementClause(AgreementClauses agreementClause) {
		this.agreementClause = agreementClause;
	}

	public List<AgreementClauses> getAgreementClauses() {
		return agreementClauses;
	}

	public void setAgreementClauses(List<AgreementClauses> agreementClauses) {
		this.agreementClauses = agreementClauses;
	}

	public Map<String, List<AgreementHistory>> getAgreementHistoryDetails() {
		return agreementHistoryDetails;
	}

	public void setAgreementHistoryDetails(Map<String, List<AgreementHistory>> agreementHistoryDetails) {
		this.agreementHistoryDetails = agreementHistoryDetails;
	}

	public String getAgreementType() {
		return agreementType;
	}

	public void setAgreementType(String agreementType) {
		this.agreementType = agreementType;
	}

	public Integer getAgreementClauseId() {
		return agreementClauseId;
	}

	public void setAgreementClauseId(Integer agreementClauseId) {
		this.agreementClauseId = agreementClauseId;
	}

	public Boolean getCanGenerateAgreement() {
		return canGenerateAgreement;
	}

	public void setCanGenerateAgreement(Boolean canGenerateAgreement) {
		this.canGenerateAgreement = canGenerateAgreement;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public List<AgreementClausesGroup> getAgreementClausesGroup() {
		return agreementClausesGroup;
	}

	public void setAgreementClausesGroup(List<AgreementClausesGroup> agreementClausesGroup) {
		this.agreementClausesGroup = agreementClausesGroup;
	}

	public Boolean getIsAgreementCreatedOnce() {
		return isAgreementCreatedOnce;
	}

	public void setIsAgreementCreatedOnce(Boolean isAgreementCreatedOnce) {
		this.isAgreementCreatedOnce = isAgreementCreatedOnce;
	}

	public NegotiationsComment getNegotiationsComment() {
		return negotiationsComment;
	}

	public void setNegotiationsComment(NegotiationsComment negotiationsComment) {
		this.negotiationsComment = negotiationsComment;
	}

	public Integer getClausesGroupCode() {
		return clausesGroupCode;
	}

	public void setClausesGroupCode(Integer clausesGroupCode) {
		this.clausesGroupCode = clausesGroupCode;
	}

	public Boolean getIsUserHaveReview() {
		return isUserHaveReview;
	}

	public void setIsUserHaveReview(Boolean isUserHaveReview) {
		this.isUserHaveReview = isUserHaveReview;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public Integer getNegotiationLocationId() {
		return negotiationLocationId;
	}

	public void setNegotiationLocationId(Integer negotiationLocationId) {
		this.negotiationLocationId = negotiationLocationId;
	}

	public List<NegotiationsComment> getNegotiationsComments() {
		return negotiationsComments;
	}

	public void setNegotiationsComments(List<NegotiationsComment> negotiationsComments) {
		this.negotiationsComments = negotiationsComments;
	}

	public Boolean getIsGeneratedAgreement() {
		return isGeneratedAgreement;
	}

	public void setIsGeneratedAgreement(Boolean isGeneratedAgreement) {
		this.isGeneratedAgreement = isGeneratedAgreement;
	}

	public Integer getAgreementSponsorContactId() {
		return agreementSponsorContactId;
	}

	public void setAgreementSponsorContactId(Integer agreementSponsorContactId) {
		this.agreementSponsorContactId = agreementSponsorContactId;
	}

	public Boolean getIsFinal() {
		return isFinal;
	}

	public void setIsFinal(Boolean isFinal) {
		this.isFinal = isFinal;
	}

	public AgreementSponsorContact getAgreementSponsorContact() {
		return agreementSponsorContact;
	}

	public void setAgreementSponsorContact(AgreementSponsorContact agreementSponsorContact) {
		this.agreementSponsorContact = agreementSponsorContact;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
	}

	public List<AgreementComment> getAgreementComments() {
		return agreementComments;
	}

	public void setAgreementComments(List<AgreementComment> agreementComments) {
		this.agreementComments = agreementComments;
	}

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public Boolean getIsLocationComment() {
		return isLocationComment;
	}

	public void setIsLocationComment(Boolean isLocationComment) {
		this.isLocationComment = isLocationComment;
	}

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public Boolean getIsAttachmentDelete() {
		return isAttachmentDelete;
	}

	public void setIsAttachmentDelete(Boolean isAttachmentDelete) {
		this.isAttachmentDelete = isAttachmentDelete;
	}

	public List<AgreementPeopleType> getAgreementPeopleType() {
		return agreementPeopleType;
	}

	public void setAgreementPeopleType(List<AgreementPeopleType> agreementPeopleType) {
		this.agreementPeopleType = agreementPeopleType;
	}

	public AgreementPeople getAgreementPeople() {
		return agreementPeople;
	}

	public void setAgreementPeople(AgreementPeople agreementPeople) {
		this.agreementPeople = agreementPeople;
	}

	public List<AgreementPeople> getAgreementPeoples() {
		return agreementPeoples;
	}

	public void setAgreementPeoples(List<AgreementPeople> agreementPeoples) {
		this.agreementPeoples = agreementPeoples;
	}

	public Integer getAgreementPeopleId() {
		return agreementPeopleId;
	}

	public void setAgreementPeopleId(Integer agreementPeopleId) {
		this.agreementPeopleId = agreementPeopleId;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public String getApproveComment() {
		return approveComment;
	}

	public void setApproveComment(String approveComment) {
		this.approveComment = approveComment;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public Integer getWorkflowDetailId() {
		return workflowDetailId;
	}

	public void setWorkflowDetailId(Integer workflowDetailId) {
		this.workflowDetailId = workflowDetailId;
	}

	public WorkflowDetail getWorkflowDetail() {
		return workflowDetail;
	}

	public void setWorkflowDetail(WorkflowDetail workflowDetail) {
		this.workflowDetail = workflowDetail;
	}

	public Boolean getFinalApprover() {
		return finalApprover;
	}

	public void setFinalApprover(Boolean finalApprover) {
		this.finalApprover = finalApprover;
	}

	public Boolean getIsApproved() {
		return isApproved;
	}

	public void setIsApproved(Boolean isApproved) {
		this.isApproved = isApproved;
	}

	public Boolean getIsApprover() {
		return isApprover;
	}

	public void setIsApprover(Boolean isApprover) {
		this.isApprover = isApprover;
	}

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public Integer getMapNumber() {
		return mapNumber;
	}

	public void setMapNumber(Integer mapNumber) {
		this.mapNumber = mapNumber;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public boolean isPrimaryApprover() {
		return isPrimaryApprover;
	}

	public void setPrimaryApprover(boolean isPrimaryApprover) {
		this.isPrimaryApprover = isPrimaryApprover;
	}

	public String getApproverFlag() {
		return approverFlag;
	}

	public void setApproverFlag(String approverFlag) {
		this.approverFlag = approverFlag;
	}

	public Integer getWorkFlowId() {
		return workFlowId;
	}

	public void setWorkFlowId(Integer workFlowId) {
		this.workFlowId = workFlowId;
	}

	public Integer getApprovalStopNumber() {
		return approvalStopNumber;
	}

	public void setApprovalStopNumber(Integer approvalStopNumber) {
		this.approvalStopNumber = approvalStopNumber;
	}

	public String getApproverPersonId() {
		return approverPersonId;
	}

	public void setApproverPersonId(String approverPersonId) {
		this.approverPersonId = approverPersonId;
	}

	public String getApprovalStatus() {
		return approvalStatus;
	}

	public void setApprovalStatus(String approvalStatus) {
		this.approvalStatus = approvalStatus;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public Integer getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(Integer subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Map<String, List<String>> getSectionCodes() {
		return sectionCodes;
	}

	public void setSectionCodes(Map<String, List<String>> sectionCodes) {
		this.sectionCodes = sectionCodes;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public List<QuestionnaireAttachment> getQuestionnnaireAttachment() {
		return questionnnaireAttachment;
	}

	public void setQuestionnnaireAttachment(List<QuestionnaireAttachment> questionnnaireAttachment) {
		this.questionnnaireAttachment = questionnnaireAttachment;
	}

	public Boolean getIsTypeChanged() {
		return isTypeChanged;
	}

	public void setIsTypeChanged(Boolean isTypeChanged) {
		this.isTypeChanged = isTypeChanged;
	}

	public Set<String> getAgreementTypeCodes() {
		return agreementTypeCodes;
	}

	public void setAgreementTypeCodes(Set<String> agreementTypeCodes) {
		this.agreementTypeCodes = agreementTypeCodes;
	}

	public List<PreReviewSectionType> getPreReviewClarifications() {
		return preReviewClarifications;
	}

	public void setPreReviewClarifications(List<PreReviewSectionType> preReviewClarifications) {
		this.preReviewClarifications = preReviewClarifications;
	}

	public List<SponsorRole> getSponsorRoles() {
		return sponsorRoles;
	}

	public void setSponsorRoles(List<SponsorRole> sponsorRoles) {
		this.sponsorRoles = sponsorRoles;
	}

	public List<AgreementSponsorType> getAgreementSponsorTypes() {
		return agreementSponsorTypes;
	}

	public void setAgreementSponsorTypes(List<AgreementSponsorType> agreementSponsorTypes) {
		this.agreementSponsorTypes = agreementSponsorTypes;
	}

	public Integer getTriageHeaderId() {
		return triageHeaderId;
	}

	public void setTriageHeaderId(Integer triageHeaderId) {
		this.triageHeaderId = triageHeaderId;
	}

	public String getAgreementReviewTypeCode() {
		return agreementReviewTypeCode;
	}

	public void setAgreementReviewTypeCode(String agreementReviewTypeCode) {
		this.agreementReviewTypeCode = agreementReviewTypeCode;
	}

	public List<AgreementReviewType> getAgreementReviewTypes() {
		return agreementReviewTypes;
	}

	public void setAgreementReviewTypes(List<AgreementReviewType> agreementReviewTypes) {
		this.agreementReviewTypes = agreementReviewTypes;
	}

	public Set<Person> getPersons() {
		return persons;
	}

	public void setPersons(Set<Person> persons) {
		this.persons = persons;
	}

	public String getLocationTypeCode() {
		return locationTypeCode;
	}

	public void setLocationTypeCode(String locationTypeCode) {
		this.locationTypeCode = locationTypeCode;
	}

	public String getLoginUserId() {
		return loginUserId;
	}

	public void setLoginUserId(String loginUserId) {
		this.loginUserId = loginUserId;
	}

	public List<AgreementLinkModule> getModuleList() {
		return moduleList;
	}

	public void setModuleList(List<AgreementLinkModule> moduleList) {
		this.moduleList = moduleList;
	}

	public List<AgreementAssociationLink> getModuleLinkDetails() {
		return moduleLinkDetails;
	}

	public void setModuleLinkDetails(List<AgreementAssociationLink> moduleLinkDetails) {
		this.moduleLinkDetails = moduleLinkDetails;
	}

	public List<AgreementAssociationDetail> getAgreementAssociationDetails() {
		return agreementAssociationDetails;
	}

	public void setAgreementAssociationDetails(List<AgreementAssociationDetail> agreementAssociationDetails) {
		this.agreementAssociationDetails = agreementAssociationDetails;
	}

	public List<AdminGroup> getAgreementAdminGroups() {
		return agreementAdminGroups;
	}

	public void setAgreementAdminGroups(List<AdminGroup> agreementAdminGroups) {
		this.agreementAdminGroups = agreementAdminGroups;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public List<AgreementHistory> getAgreementHistories() {
		return agreementHistories;
	}

	public void setAgreementHistories(List<AgreementHistory> agreementHistories) {
		this.agreementHistories = agreementHistories;
	}

}
