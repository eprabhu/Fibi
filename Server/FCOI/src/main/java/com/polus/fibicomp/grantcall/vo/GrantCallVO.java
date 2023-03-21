package com.polus.fibicomp.grantcall.vo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.grantcall.comparator.GrantCallAttachmentComparator;
import com.polus.fibicomp.grantcall.pojo.FundingSchemeAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachType;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallContact;
import com.polus.fibicomp.grantcall.pojo.GrantCallCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibilityType;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibleDepartment;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIQuestionnaire;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallResearchArea;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallStatus;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.grantcall.pojo.GrantEligibiltyTargetType;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.FundingSourceType;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.RelevantField;
import com.polus.fibicomp.pojo.ResearchType;
import com.polus.fibicomp.pojo.ResearchTypeArea;
import com.polus.fibicomp.pojo.ResearchTypeSubArea;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;
import com.polus.fibicomp.vo.BaseVO;

public class GrantCallVO extends BaseVO {

	private Integer grantCallId;

	private Integer grantKeywordId;

	private Integer grantCallPersonId;

	private Integer grantEligibilityDepartmentId;

	private List<GrantCallType> grantCallTypes;

	private List<GrantCallStatus> grantCallStatus;

	private List<ScienceKeyword> scienceKeywords;

	private List<SponsorType> sponsorTypes;

	private List<Sponsor> sponsors;

	private List<GrantCallCriteria> grantCallCriterias;

	private List<GrantCallEligibilityType> grantCallEligibilityTypes;

	private List<GrantCallAttachType> grantCallAttachTypes;

	private GrantCall grantCall;

	private String sponsorTypeCode;

	private Boolean status;

	private String message;

	private String updateType;

	private Integer grantContactId;

	private Integer grantResearchAreaId;

	private Integer grantEligibilityId;

	private Integer attachmentId;

	private Integer grantStatusCode;

	private GrantCallStatus grantStatus;

	private GrantCallAttachment newAttachment;

	private List<Unit> homeUnits;

	private String userFullName;

	private List<GrantCallAttachment> newAttachments;

	private List<ProposalPersonRole> grantCallPersonRoles;

	private Integer documentId;

	private String researchTypeCode;

	private Integer challengeAreaCode;

	private List<Currency> currencyDetail;

	private List<FundingSourceType> fundingSourceType;

	private String sponsorCode;

	private String homeUnitNumber;

	private Boolean isStatusChanged = false;

	private List<FundingSchemeAttachment> fundingSchemeAttachment;
	
	private String scienceKeyword;
	
	private List<RelevantField> relevantFields;
	
	private Integer grantCallRelevantId;

	private String updateUser;

	private List<GrantEligibiltyTargetType> grantEligibiltyTargetTypes;

	private Integer grantCallIOIId;

	private GrantCallIOIQuestionnaire grantQuestionnaire;

	private Integer grantIOIQuestionnaireId;

	private String loginPersonId;

	private List<String> availableRights;

	private List<ProposalEvaluationScore> proposalEvaluationScores;

	private List<GrantCallKPI> grantCallKPIs;

	private List<GrantCallScoringCriteria> grantCallScoringCriterias;

	private List<GrantCallEvaluationPanel> grantCallEvaluationPanels;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String contentType;

	private String fileTimestamp;

	private GrantCallResearchArea grantCallResearchArea;

	private List<GrantCallAttachment> grantCallAttachments;

	private List<GrantCallContact> grantCallContacts;

	private List<GrantCallResearchArea> grantCallResearchAreas;

	private List<GrantCallEligibility> grantCallEligibilities;

	private List<GrantCallIOIHeader> grantCallIOIHeaders;

	private List<GrantCallEligibleDepartment> grantEligibleDepartments; 

	private GrantCallContact grantCallContact;

	private GrantCallEligibility grantCallEligibility;

	private GrantCallAttachment grantCallAttachment;

	private Boolean canCreateIOI = false;

	private List<SponsorFundingScheme> sponsorFundingSchemes;

	private String fileName;

	private String timeZone;

	private List<GrantCallActionLog> grantCallActionLogs;

	private List<RateType> rateTypes;

	private Boolean isReplaceAttachmentEnabled = Boolean.FALSE;

	private List<ResearchTypeSubArea> researchTypeSubArea;

	private List<ResearchType> researchTypes;

	private List<ResearchTypeArea> researchTypeArea;

	private String grantCallPocDefaultMail;

	private Boolean enableUserDefinedFundingAgency = Boolean.FALSE;

	public GrantCallVO() {
		grantCall = new GrantCall();
		grantCallAttachments = new ArrayList<>();
		grantCallContacts = new ArrayList<>();
		grantCallResearchAreas = new ArrayList<>();
		grantCallEligibilities = new ArrayList<>();
		grantEligibleDepartments = new ArrayList<>();
		proposalEvaluationScores = new ArrayList<>();
		
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public Integer getGrantCallPersonId() {
		return grantCallPersonId;
	}

	public void setGrantCallPersonId(Integer grantCallPersonId) {
		this.grantCallPersonId = grantCallPersonId;
	}

	public Integer getGrantKeywordId() {
		return grantKeywordId;
	}

	public void setGrantKeywordId(Integer grantKeywordId) {
		this.grantKeywordId = grantKeywordId;
	}

	public List<GrantCallType> getGrantCallTypes() {
		return grantCallTypes;
	}

	public void setGrantCallTypes(List<GrantCallType> grantCallTypes) {
		this.grantCallTypes = grantCallTypes;
	}

	public List<GrantCallStatus> getGrantCallStatus() {
		return grantCallStatus;
	}

	public void setGrantCallStatus(List<GrantCallStatus> grantCallStatus) {
		this.grantCallStatus = grantCallStatus;
	}

	public List<ScienceKeyword> getScienceKeywords() {
		return scienceKeywords;
	}

	public void setScienceKeywords(List<ScienceKeyword> scienceKeywords) {
		this.scienceKeywords = scienceKeywords;
	}

	public List<SponsorType> getSponsorTypes() {
		return sponsorTypes;
	}

	public void setSponsorTypes(List<SponsorType> sponsorTypes) {
		this.sponsorTypes = sponsorTypes;
	}

	public List<Sponsor> getSponsors() {
		return sponsors;
	}

	public void setSponsors(List<Sponsor> sponsors) {
		this.sponsors = sponsors;
	}

	public List<GrantCallCriteria> getGrantCallCriterias() {
		return grantCallCriterias;
	}

	public void setGrantCallCriterias(List<GrantCallCriteria> grantCallCriterias) {
		this.grantCallCriterias = grantCallCriterias;
	}

	public List<GrantCallEligibilityType> getGrantCallEligibilityTypes() {
		return grantCallEligibilityTypes;
	}

	public void setGrantCallEligibilityTypes(List<GrantCallEligibilityType> grantCallEligibilityTypes) {
		this.grantCallEligibilityTypes = grantCallEligibilityTypes;
	}

	public List<GrantCallAttachType> getGrantCallAttachTypes() {
		return grantCallAttachTypes;
	}

	public void setGrantCallAttachTypes(List<GrantCallAttachType> grantCallAttachTypes) {
		this.grantCallAttachTypes = grantCallAttachTypes;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateType() {
		return updateType;
	}

	public void setUpdateType(String updateType) {
		this.updateType = updateType;
	}

	public Integer getGrantContactId() {
		return grantContactId;
	}

	public void setGrantContactId(Integer grantContactId) {
		this.grantContactId = grantContactId;
	}

	public Integer getGrantResearchAreaId() {
		return grantResearchAreaId;
	}

	public void setGrantResearchAreaId(Integer grantResearchAreaId) {
		this.grantResearchAreaId = grantResearchAreaId;
	}

	public Integer getGrantEligibilityId() {
		return grantEligibilityId;
	}

	public void setGrantEligibilityId(Integer grantEligibilityId) {
		this.grantEligibilityId = grantEligibilityId;
	}

	public Integer getAttachmentId() {
		return attachmentId;
	}

	public void setAttachmentId(Integer attachmentId) {
		this.attachmentId = attachmentId;
	}

	public Integer getGrantStatusCode() {
		return grantStatusCode;
	}

	public void setGrantStatusCode(Integer grantStatusCode) {
		this.grantStatusCode = grantStatusCode;
	}

	public GrantCallStatus getGrantStatus() {
		return grantStatus;
	}

	public void setGrantStatus(GrantCallStatus grantStatus) {
		this.grantStatus = grantStatus;
	}

	public GrantCallAttachment getNewAttachment() {
		return newAttachment;
	}

	public void setNewAttachment(GrantCallAttachment newAttachment) {
		this.newAttachment = newAttachment;
	}

	public List<Unit> getHomeUnits() {
		return homeUnits;
	}

	public void setHomeUnits(List<Unit> homeUnits) {
		this.homeUnits = homeUnits;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}

	public List<GrantCallAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<GrantCallAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getGrantEligibilityDepartmentId() {
		return grantEligibilityDepartmentId;
	}

	public void setGrantEligibilityDepartmentId(Integer grantEligibilityDepartmentId) {
		this.grantEligibilityDepartmentId = grantEligibilityDepartmentId;
	}

	public List<ProposalPersonRole> getGrantCallPersonRoles() {
		return grantCallPersonRoles;
	}

	public void setGrantCallPersonRoles(List<ProposalPersonRole> grantCallPersonRoles) {
		this.grantCallPersonRoles = grantCallPersonRoles;
	}

	public List<Currency> getCurrencyDetail() {
		return currencyDetail;
	}

	public void setCurrencyDetail(List<Currency> currencyDetail) {
		this.currencyDetail = currencyDetail;
	}

	public String getResearchTypeCode() {
		return researchTypeCode;
	}

	public void setResearchTypeCode(String researchTypeCode) {
		this.researchTypeCode = researchTypeCode;
	}

	public Integer getChallengeAreaCode() {
		return challengeAreaCode;
	}

	public void setChallengeAreaCode(Integer challengeAreaCode) {
		this.challengeAreaCode = challengeAreaCode;
	}

	public List<FundingSourceType> getFundingSourceType() {
		return fundingSourceType;
	}

	public void setFundingSourceType(List<FundingSourceType> fundingSourceType) {
		this.fundingSourceType = fundingSourceType;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getHomeUnitNumber() {
		return homeUnitNumber;
	}

	public void setHomeUnitNumber(String homeUnitNumber) {
		this.homeUnitNumber = homeUnitNumber;
	}

	public Boolean getIsStatusChanged() {
		return isStatusChanged;
	}

	public void setIsStatusChanged(Boolean isStatusChanged) {
		this.isStatusChanged = isStatusChanged;
	}

	public List<FundingSchemeAttachment> getFundingSchemeAttachment() {
		return fundingSchemeAttachment;
	}

	public void setFundingSchemeAttachment(List<FundingSchemeAttachment> fundingSchemeAttachment) {
		this.fundingSchemeAttachment = fundingSchemeAttachment;
	}

	public String getScienceKeyword() {
		return scienceKeyword;
	}

	public void setScienceKeyword(String scienceKeyword) {
		this.scienceKeyword = scienceKeyword;
	}

	public List<RelevantField> getRelevantFields() {
		return relevantFields;
	}

	public void setRelevantFields(List<RelevantField> relevantFields) {
		this.relevantFields = relevantFields;
	}

	public Integer getGrantCallRelevantId() {
		return grantCallRelevantId;
	}

	public void setGrantCallRelevantId(Integer grantCallRelevantId) {
		this.grantCallRelevantId = grantCallRelevantId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<GrantEligibiltyTargetType> getGrantEligibiltyTargetTypes() {
		return grantEligibiltyTargetTypes;
	}

	public void setGrantEligibiltyTargetTypes(List<GrantEligibiltyTargetType> grantEligibiltyTargetTypes) {
		this.grantEligibiltyTargetTypes = grantEligibiltyTargetTypes;
	}

	public Integer getGrantCallIOIId() {
		return grantCallIOIId;
	}

	public void setGrantCallIOIId(Integer grantCallIOIId) {
		this.grantCallIOIId = grantCallIOIId;
	}

	public GrantCallIOIQuestionnaire getGrantQuestionnaire() {
		return grantQuestionnaire;
	}

	public void setGrantQuestionnaire(GrantCallIOIQuestionnaire grantQuestionnaire) {
		this.grantQuestionnaire = grantQuestionnaire;
	}

	public Integer getGrantIOIQuestionnaireId() {
		return grantIOIQuestionnaireId;
	}

	public void setGrantIOIQuestionnaireId(Integer grantIOIQuestionnaireId) {
		this.grantIOIQuestionnaireId = grantIOIQuestionnaireId;
	}

	public String getLoginPersonId() {
		return loginPersonId;
	}

	public void setLoginPersonId(String loginPersonId) {
		this.loginPersonId = loginPersonId;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public List<ProposalEvaluationScore> getProposalEvaluationScores() {
		return proposalEvaluationScores;
	}

	public void setProposalEvaluationScores(List<ProposalEvaluationScore> proposalEvaluationScores) {
		this.proposalEvaluationScores = proposalEvaluationScores;
	}

	public List<GrantCallKPI> getGrantCallKPIs() {
		return grantCallKPIs;
	}

	public void setGrantCallKPIs(List<GrantCallKPI> grantCallKPIs) {
		this.grantCallKPIs = grantCallKPIs;
	}

	public List<GrantCallScoringCriteria> getGrantCallScoringCriterias() {
		return grantCallScoringCriterias;
	}

	public void setGrantCallScoringCriterias(List<GrantCallScoringCriteria> grantCallScoringCriterias) {
		this.grantCallScoringCriterias = grantCallScoringCriterias;
	}

	public List<GrantCallEvaluationPanel> getGrantCallEvaluationPanels() {
		return grantCallEvaluationPanels;
	}

	public void setGrantCallEvaluationPanels(List<GrantCallEvaluationPanel> grantCallEvaluationPanels) {
		this.grantCallEvaluationPanels = grantCallEvaluationPanels;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public List<GrantCallAttachment> getGrantCallAttachments() {
		if (grantCallAttachments != null && !grantCallAttachments.isEmpty()) {
			Collections.sort(grantCallAttachments, new GrantCallAttachmentComparator());
		}
		return grantCallAttachments;
	}

	public void setGrantCallAttachments(List<GrantCallAttachment> grantCallAttachments) {
		this.grantCallAttachments = grantCallAttachments;
	}

	public List<GrantCallContact> getGrantCallContacts() {
		return grantCallContacts;
	}

	public void setGrantCallContacts(List<GrantCallContact> grantCallContacts) {
		this.grantCallContacts = grantCallContacts;
	}

	public List<GrantCallResearchArea> getGrantCallResearchAreas() {
		return grantCallResearchAreas;
	}

	public void setGrantCallResearchAreas(List<GrantCallResearchArea> grantCallResearchAreas) {
		this.grantCallResearchAreas = grantCallResearchAreas;
	}

	public List<GrantCallEligibility> getGrantCallEligibilities() {
		return grantCallEligibilities;
	}

	public void setGrantCallEligibilities(List<GrantCallEligibility> grantCallEligibilities) {
		this.grantCallEligibilities = grantCallEligibilities;
	}

	public List<GrantCallIOIHeader> getGrantCallIOIHeaders() {
		return grantCallIOIHeaders;
	}

	public void setGrantCallIOIHeaders(List<GrantCallIOIHeader> grantCallIOIHeaders) {
		this.grantCallIOIHeaders = grantCallIOIHeaders;
	}

	public List<GrantCallEligibleDepartment> getGrantEligibleDepartments() {
		return grantEligibleDepartments;
	}

	public void setGrantEligibleDepartments(List<GrantCallEligibleDepartment> grantEligibleDepartments) {
		this.grantEligibleDepartments = grantEligibleDepartments;
	}

	public GrantCallResearchArea getGrantCallResearchArea() {
		return grantCallResearchArea;
	}

	public void setGrantCallResearchArea(GrantCallResearchArea grantCallResearchArea) {
		this.grantCallResearchArea = grantCallResearchArea;
	}

	public GrantCallContact getGrantCallContact() {
		return grantCallContact;
	}

	public void setGrantCallContact(GrantCallContact grantCallContact) {
		this.grantCallContact = grantCallContact;
	}

	public GrantCallEligibility getGrantCallEligibility() {
		return grantCallEligibility;
	}

	public void setGrantCallEligibility(GrantCallEligibility grantCallEligibility) {
		this.grantCallEligibility = grantCallEligibility;
	}

	public GrantCallAttachment getGrantCallAttachment() {
		return grantCallAttachment;
	}

	public void setGrantCallAttachment(GrantCallAttachment grantCallAttachment) {
		this.grantCallAttachment = grantCallAttachment;
	}

	public Boolean getCanCreateIOI() {
		return canCreateIOI;
	}

	public void setCanCreateIOI(Boolean canCreateIOI) {
		this.canCreateIOI = canCreateIOI;
	}

	public List<SponsorFundingScheme> getSponsorFundingSchemes() {
		return sponsorFundingSchemes;
	}

	public void setSponsorFundingSchemes(List<SponsorFundingScheme> sponsorFundingSchemes) {
		this.sponsorFundingSchemes = sponsorFundingSchemes;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getTimeZone() {
		return timeZone;
	}

	public void setTimeZone(String timeZone) {
		this.timeZone = timeZone;
	}

	public List<RateType> getRateTypes() {
		return rateTypes;
	}

	public void setRateTypes(List<RateType> rateTypes) {
		this.rateTypes = rateTypes;
	}

	public List<GrantCallActionLog> getGrantCallActionLogs() {
		return grantCallActionLogs;
	}

	public void setGrantCallActionLogs(List<GrantCallActionLog> grantCallActionLogs) {
		this.grantCallActionLogs = grantCallActionLogs;
	}

	public Boolean getIsReplaceAttachmentEnabled() {
		return isReplaceAttachmentEnabled;
	}

	public void setIsReplaceAttachmentEnabled(Boolean isReplaceAttachmentEnabled) {
		this.isReplaceAttachmentEnabled = isReplaceAttachmentEnabled;
	}

	public List<ResearchTypeSubArea> getResearchTypeSubArea() {
		return researchTypeSubArea;
	}

	public void setResearchTypeSubArea(List<ResearchTypeSubArea> researchTypeSubArea) {
		this.researchTypeSubArea = researchTypeSubArea;
	}

	public List<ResearchType> getResearchTypes() {
		return researchTypes;
	}

	public void setResearchTypes(List<ResearchType> researchTypes) {
		this.researchTypes = researchTypes;
	}

	public List<ResearchTypeArea> getResearchTypeArea() {
		return researchTypeArea;
	}

	public void setResearchTypeArea(List<ResearchTypeArea> researchTypeArea) {
		this.researchTypeArea = researchTypeArea;
	}

	public String getGrantCallPocDefaultMail() {
		return grantCallPocDefaultMail;
	}

	public void setGrantCallPocDefaultMail(String grantCallPocDefaultMail) {
		this.grantCallPocDefaultMail = grantCallPocDefaultMail;
	}

	public Boolean getEnableUserDefinedFundingAgency() {
		return enableUserDefinedFundingAgency;
	}

	public void setEnableUserDefinedFundingAgency(Boolean enableUserDefinedFundingAgency) {
		this.enableUserDefinedFundingAgency = enableUserDefinedFundingAgency;
	}

}
