package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.pojo.EvaluationRecommendation;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.util.JpaCharBooleanConversion;
import com.polus.fibicomp.workflow.pojo.Workflow;

@Entity
@Table(name = "EPS_PROPOSAL")
public class Proposal implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROPOSAL_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROPOSAL_ID_GENERATOR", sequenceName = "EPS_PROPOSAL_ID_GENERATOR", allocationSize=1)
	private Integer proposalId;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK2"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ProposalStatus proposalStatus;

	@Column(name = "TYPE_CODE")
	private Integer typeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK1"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ProposalType proposalType;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "SUBMISSION_DATE")
	private Timestamp submissionDate;

	@Column(name = "INTERNAL_DEADLINE_DATE")
	private Timestamp internalDeadLineDate;

	@Column(name = "ABSTRACT_DESC")
	private String abstractDescription;

	@Column(name = "FUNDING_STRATEGY")
	private String fundingStrategy;

	@Column(name = "DETAILS")
	private String details;

	@Column(name = "DELIVERABLES")
	private String deliverables;

	@Column(name = "RESEARCH_AREA_DESC")
	private String researchDescription;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IP_NUMBER")
	private String ipNumber;

	@Column(name = "SPONSOR_DEADLINE_DATE")
	private Timestamp sponsorDeadlineDate;

	@Column(name = "IS_ENDORSED_ONCE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean isEndorsedOnce = false;

	@Column(name = "PROPOSAL_RANK")
	private Integer proposalRank;

	@Column(name = "APPLICATION_ID")
	private String applicationId;

	@Column(name = "MULTI_DISCIPLINARY_DESC")
	private String multiDisciplinaryDescription;

	@Column(name = "DURATION")
	private String duration;

	@JsonManagedReference
	@OneToMany(mappedBy = "proposal", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ProposalKeyword> proposalKeywords;

	@Column(name = "GRANT_TYPE_CODE")
	private Integer grantTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK6"), name = "GRANT_TYPE_CODE", referencedColumnName = "GRANT_TYPE_CODE", insertable = false, updatable = false)
	private GrantCallType grantCallType;

	@Column(name = "HOME_UNIT_NUMBER")
	private String homeUnitNumber;

	@Column(name = "HOME_UNIT_NAME")
	private String homeUnitName;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK12"), name = "HOME_UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK8"), name = "ACTIVITY_TYPE_CODE", referencedColumnName = "ACTIVITY_TYPE_CODE", insertable = false, updatable = false)
	private ActivityType activityType;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK15"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "SUBMIT_USER")
	private String submitUser;

	@Column(name = "SPONSOR_PROPOSAL_NUMBER")
	private String sponsorProposalNumber;

	@Column(name = "AWARD_TYPE_CODE")
	private String awardTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK14"), name = "AWARD_TYPE_CODE", referencedColumnName = "AWARD_TYPE_CODE", insertable = false, updatable = false)
	private AwardType awardType;

	@Column(name = "PRIME_SPONSOR_CODE")
	private String primeSponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK16"), name = "PRIME_SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor primeSponsor;

	@Column(name = "BASE_PROPOSAL_NUMBER")
	private String baseProposalNumber;

	@Column(name = "PROGRAM_ANNOUNCEMENT_NUMBER")
	private String programAnnouncementNumber;

	@Column(name = "CFDA_NUMBER")
	private String cfdaNumber;

	@Column(name = "EXTERNAL_FUNDING_AGENCY_ID")
	private String externalFundingAgencyId;

	@Column(name = "CLUSTER_CODE")
	private Integer clusterCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK10"), name = "CLUSTER_CODE", referencedColumnName = "CLUSTER_CODE", insertable = false, updatable = false)
	private DisciplineCluster disciplineCluster;

	// default value null
	@Column(name = "IS_ELIGIBLE_CRITERIA_MET")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isEligibilityCriteriaMet;

	@Column(name = "EVALUATION_RECOMMENDATION_CODE")
	private Integer recommendationCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK11"), name = "EVALUATION_RECOMMENDATION_CODE", referencedColumnName = "EVALUATION_RECOMMENDATION_CODE", insertable = false, updatable = false)
	private EvaluationRecommendation evaluationRecommendation;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private String documentStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_FK13"), name = "DOCUMENT_STATUS_CODE", referencedColumnName = "DOCUMENT_STATUS_CODE", insertable = false, updatable = false)
	private ProposalDocumentStatus documentStatus;

	@Column(name = "SOURCE_PROPOSAL_ID")
	private Integer sourceProposalId;

	@Transient
	private String applicationActivityType;

	@Transient
	private String applicationType;

	@Transient
	private String applicationStatus;

	@Transient
	private Workflow workflow;

	@Transient
	private List<PreReview> proposalPreReviews;

	@Transient
	private PreReview reviewerReview;

	@Transient
	private Boolean preReviewExist = false;

	@Transient
	private Boolean isPreReviewer = false;

	@Transient
	private List<Workflow> workflowList;

	@Transient
	private String principalInvestigatorForMobile;

	@Transient
	private Set<String> grantORTTManagers;

	@Transient
	private Boolean isAssigned = false;

	@Transient
	private Boolean isRcbfProposal = false;

	@Transient
	private Boolean isModularBudgetEnabled = false;

	@Transient
	private Boolean isSimpleBudgetEnabled = false;

	@Transient
	private Boolean isDetailedBudgetEnabled = false;

	@Transient
	private Boolean isBudgetCategoryTotalEnabled = false;

	@Transient
	private String createUserFullName;

	@Transient
	private String lastUpdateUserFullName;

	@Transient
	private String submitUserFullName;

	@Transient
	private Boolean hasRecommendation = false;

	@Transient
	private Boolean hasRank = false;

	@Transient
	private ProposalPerson investigator;

	@Transient
	private String grantCallName;

	@Transient
	private Timestamp grantCallClosingDate;

	@Transient
	private List<ProposalPerson> proposalPersons;

	@Transient
	private Boolean isReviewExist;
	
	@Transient
	private BigDecimal score = BigDecimal.ZERO;

	@Transient
	private Integer categoryCode;

	@Transient
	private ProposalEvaluationScore proposalEvaluationScore;

	@Transient
	private BigDecimal totalCost;

	@Transient
	private BigDecimal total;

	/*
	 * This Transient variable is used for NTU Where we need the Map Id for
	 * filtering the scoring panel in front end.
	 */
	@Transient
	private Integer scoringMapId;

	/*
	 * This Transient variable is used for NTU to show the
	 * proposal dashboard with the grant call abbreviation.
	 */
	@Transient
	private String abbreviation;

	private transient String awardTitle;

	private transient Integer awardId;

	@Transient
	private String proposalTypeDescription;

	@Transient
	private String proposalCategory;

	@Transient
	private String sourceProposalTitle;

	@Transient
	private String baseProposalTitle;

	private transient String sponsorName;

	private transient String primeSponsorName;

	public Proposal() {
		proposalKeywords = new ArrayList<>();
		grantORTTManagers = new HashSet<>();
		proposalPersons = new ArrayList<>();
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public ProposalStatus getProposalStatus() {
		return proposalStatus;
	}

	public void setProposalStatus(ProposalStatus proposalStatus) {
		this.proposalStatus = proposalStatus;
	}

	public Integer getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(Integer typeCode) {
		this.typeCode = typeCode;
	}

	public ProposalType getProposalType() {
		return proposalType;
	}

	public void setProposalType(ProposalType proposalType) {
		this.proposalType = proposalType;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getSubmissionDate() {
		return submissionDate;
	}

	public void setSubmissionDate(Timestamp submissionDate) {
		this.submissionDate = submissionDate;
	}

	public String getAbstractDescription() {
		return abstractDescription;
	}

	public void setAbstractDescription(String abstractDescription) {
		this.abstractDescription = abstractDescription;
	}

	public String getFundingStrategy() {
		return fundingStrategy;
	}

	public void setFundingStrategy(String fundingStrategy) {
		this.fundingStrategy = fundingStrategy;
	}

	public String getDetails() {
		return details;
	}

	public void setDetails(String details) {
		this.details = details;
	}

	public String getDeliverables() {
		return deliverables;
	}

	public void setDeliverables(String deliverables) {
		this.deliverables = deliverables;
	}

	public String getResearchDescription() {
		return researchDescription;
	}

	public void setResearchDescription(String researchDescription) {
		this.researchDescription = researchDescription;
	}

	public Timestamp getCreateTimeStamp() {
		return createTimeStamp;
	}

	public void setCreateTimeStamp(Timestamp createTimeStamp) {
		this.createTimeStamp = createTimeStamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public GrantCallType getGrantCallType() {
		return grantCallType;
	}

	public void setGrantCallType(GrantCallType grantCallType) {
		this.grantCallType = grantCallType;
	}

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public String getHomeUnitNumber() {
		return homeUnitNumber;
	}

	public void setHomeUnitNumber(String homeUnitNumber) {
		this.homeUnitNumber = homeUnitNumber;
	}

	public String getHomeUnitName() {
		return homeUnitName;
	}

	public void setHomeUnitName(String homeUnitName) {
		this.homeUnitName = homeUnitName;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public String getIpNumber() {
		return ipNumber;
	}

	public void setIpNumber(String ipNumber) {
		this.ipNumber = ipNumber;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public ActivityType getActivityType() {
		return activityType;
	}

	public void setActivityType(ActivityType activityType) {
		this.activityType = activityType;
	}

	public String getApplicationActivityType() {
		return applicationActivityType;
	}

	public void setApplicationActivityType(String applicationActivityType) {
		this.applicationActivityType = applicationActivityType;
	}

	public String getApplicationType() {
		return applicationType;
	}

	public void setApplicationType(String applicationType) {
		this.applicationType = applicationType;
	}

	public String getApplicationStatus() {
		return applicationStatus;
	}

	public void setApplicationStatus(String applicationStatus) {
		this.applicationStatus = applicationStatus;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public String getSubmitUser() {
		return submitUser;
	}

	public void setSubmitUser(String submitUser) {
		this.submitUser = submitUser;
	}

	public String getSponsorProposalNumber() {
		return sponsorProposalNumber;
	}

	public void setSponsorProposalNumber(String sponsorProposalNumber) {
		this.sponsorProposalNumber = sponsorProposalNumber;
	}

	public Timestamp getInternalDeadLineDate() {
		return internalDeadLineDate;
	}

	public void setInternalDeadLineDate(Timestamp internalDeadLineDate) {
		this.internalDeadLineDate = internalDeadLineDate;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public Boolean getPreReviewExist() {
		return preReviewExist;
	}

	public void setPreReviewExist(Boolean preReviewExist) {
		this.preReviewExist = preReviewExist;
	}

	public Boolean getIsPreReviewer() {
		return isPreReviewer;
	}

	public void setIsPreReviewer(Boolean isPreReviewer) {
		this.isPreReviewer = isPreReviewer;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getPrincipalInvestigatorForMobile() {
		return principalInvestigatorForMobile;
	}

	public void setPrincipalInvestigatorForMobile(String principalInvestigatorForMobile) {
		this.principalInvestigatorForMobile = principalInvestigatorForMobile;
	}

	public List<PreReview> getProposalPreReviews() {
		return proposalPreReviews;
	}

	public void setProposalPreReviews(List<PreReview> proposalPreReviews) {
		this.proposalPreReviews = proposalPreReviews;
	}

	public PreReview getReviewerReview() {
		return reviewerReview;
	}

	public void setReviewerReview(PreReview reviewerReview) {
		this.reviewerReview = reviewerReview;
	}

	public String getAwardTypeCode() {
		return awardTypeCode;
	}

	public void setAwardTypeCode(String awardTypeCode) {
		this.awardTypeCode = awardTypeCode;
	}

	public AwardType getAwardType() {
		return awardType;
	}

	public void setAwardType(AwardType awardType) {
		this.awardType = awardType;
	}

	public String getPrimeSponsorCode() {
		return primeSponsorCode;
	}

	public void setPrimeSponsorCode(String primeSponsorCode) {
		this.primeSponsorCode = primeSponsorCode;
	}

	public Sponsor getPrimeSponsor() {
		return primeSponsor;
	}

	public void setPrimeSponsor(Sponsor primeSponsor) {
		this.primeSponsor = primeSponsor;
	}

	public String getBaseProposalNumber() {
		return baseProposalNumber;
	}

	public void setBaseProposalNumber(String baseProposalNumber) {
		this.baseProposalNumber = baseProposalNumber;
	}

	public String getProgramAnnouncementNumber() {
		return programAnnouncementNumber;
	}

	public void setProgramAnnouncementNumber(String programAnnouncementNumber) {
		this.programAnnouncementNumber = programAnnouncementNumber;
	}

	public String getCfdaNumber() {
		return cfdaNumber;
	}

	public void setCfdaNumber(String cfdaNumber) {
		this.cfdaNumber = cfdaNumber;
	}

	public Timestamp getSponsorDeadlineDate() {
		return sponsorDeadlineDate;
	}

	public void setSponsorDeadlineDate(Timestamp sponsorDeadlineDate) {
		this.sponsorDeadlineDate = sponsorDeadlineDate;
	}

	public Integer getProposalRank() {
		return proposalRank;
	}

	public void setProposalRank(Integer proposalRank) {
		this.proposalRank = proposalRank;
	}

	public String getApplicationId() {
		return applicationId;
	}

	public void setApplicationId(String applicationId) {
		this.applicationId = applicationId;
	}

	public Set<String> getGrantORTTManagers() {
		return grantORTTManagers;
	}

	public void setGrantORTTManagers(Set<String> grantORTTManagers) {
		this.grantORTTManagers = grantORTTManagers;
	}

	public Boolean getIsAssigned() {
		return isAssigned;
	}

	public void setIsAssigned(Boolean isAssigned) {
		this.isAssigned = isAssigned;
	}

	public String getMultiDisciplinaryDescription() {
		return multiDisciplinaryDescription;
	}

	public void setMultiDisciplinaryDescription(String multiDisciplinaryDescription) {
		this.multiDisciplinaryDescription = multiDisciplinaryDescription;
	}

	public Integer getClusterCode() {
		return clusterCode;
	}

	public void setClusterCode(Integer clusterCode) {
		this.clusterCode = clusterCode;
	}

	public DisciplineCluster getDisciplineCluster() {
		return disciplineCluster;
	}

	public void setDisciplineCluster(DisciplineCluster disciplineCluster) {
		this.disciplineCluster = disciplineCluster;
	}

	public String getExternalFundingAgencyId() {
		return externalFundingAgencyId;
	}

	public void setExternalFundingAgencyId(String externalFundingAgencyId) {
		this.externalFundingAgencyId = externalFundingAgencyId;
	}

	public Boolean getIsEligibilityCriteriaMet() {
		return isEligibilityCriteriaMet;
	}

	public void setIsEligibilityCriteriaMet(Boolean isEligibilityCriteriaMet) {
		this.isEligibilityCriteriaMet = isEligibilityCriteriaMet;
	}

	public Boolean getIsRcbfProposal() {
		return isRcbfProposal;
	}

	public void setIsRcbfProposal(Boolean isRcbfProposal) {
		this.isRcbfProposal = isRcbfProposal;
	}

	public Boolean getIsModularBudgetEnabled() {
		return isModularBudgetEnabled;
	}

	public void setIsModularBudgetEnabled(Boolean isModularBudgetEnabled) {
		this.isModularBudgetEnabled = isModularBudgetEnabled;
	}

	public Boolean getIsSimpleBudgetEnabled() {
		return isSimpleBudgetEnabled;
	}

	public void setIsSimpleBudgetEnabled(Boolean isSimpleBudgetEnabled) {
		this.isSimpleBudgetEnabled = isSimpleBudgetEnabled;
	}

	public Boolean getIsDetailedBudgetEnabled() {
		return isDetailedBudgetEnabled;
	}

	public void setIsDetailedBudgetEnabled(Boolean isDetailedBudgetEnabled) {
		this.isDetailedBudgetEnabled = isDetailedBudgetEnabled;
	}

	public Boolean getIsBudgetCategoryTotalEnabled() {
		return isBudgetCategoryTotalEnabled;
	}

	public void setIsBudgetCategoryTotalEnabled(Boolean isBudgetCategoryTotalEnabled) {
		this.isBudgetCategoryTotalEnabled = isBudgetCategoryTotalEnabled;
	}

	public List<ProposalKeyword> getProposalKeywords() {
		return proposalKeywords;
	}

	public void setProposalKeywords(List<ProposalKeyword> proposalKeywords) {
		this.proposalKeywords = proposalKeywords;
	}

	public List<ProposalPerson> getProposalPersons() {
		return proposalPersons;
	}

	public void setProposalPersons(List<ProposalPerson> proposalPersons) {
		this.proposalPersons = proposalPersons;
	}

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public Timestamp getGrantCallClosingDate() {
		return grantCallClosingDate;
	}

	public void setGrantCallClosingDate(Timestamp grantCallClosingDate) {
		this.grantCallClosingDate = grantCallClosingDate;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public EvaluationRecommendation getEvaluationRecommendation() {
		return evaluationRecommendation;
	}

	public void setEvaluationRecommendation(EvaluationRecommendation evaluationRecommendation) {
		this.evaluationRecommendation = evaluationRecommendation;
	}

	public Boolean getHasRecommendation() {
		return hasRecommendation;
	}

	public void setHasRecommendation(Boolean hasRecommendation) {
		this.hasRecommendation = hasRecommendation;
	}

	public Boolean getHasRank() {
		return hasRank;
	}

	public void setHasRank(Boolean hasRank) {
		this.hasRank = hasRank;
	}

	public Integer getRecommendationCode() {
		return recommendationCode;
	}

	public void setRecommendationCode(Integer recommendationCode) {
		this.recommendationCode = recommendationCode;
	}

	public ProposalPerson getInvestigator() {
		if (proposalPersons != null && !proposalPersons.isEmpty()) {
			for (ProposalPerson proposalPerson : proposalPersons) {
				if (proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE) || proposalPerson.getPersonRoleId().equals(Constants.PI_ROLE_CODE_MULTI)) {
					investigator = proposalPerson;
				}
			}
		}
		return investigator;
	}

	public void setInvestigator(ProposalPerson investigator) {
		this.investigator = investigator;
	}

	public Boolean getIsReviewExist() {
		return isReviewExist;
	}

	public void setIsReviewExist(Boolean isReviewExist) {
		this.isReviewExist = isReviewExist;
	}

	public BigDecimal getScore() {
		return score;
	}

	public void setScore(BigDecimal score) {
		this.score = score;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}
	
	public ProposalEvaluationScore getProposalEvaluationScore() {
		return proposalEvaluationScore;
	}

	public void setProposalEvaluationScore(ProposalEvaluationScore proposalEvaluationScore) {
		this.proposalEvaluationScore = proposalEvaluationScore;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
	}
	
	public BigDecimal getTotal() {
		return total;
	}

	public void setTotal(BigDecimal total) {
		this.total = total;
	}

	public Integer getScoringMapId() {
		return scoringMapId;
	}

	public void setScoringMapId(Integer scoringMapId) {
		this.scoringMapId = scoringMapId;
	}

	public String getAbbreviation() {
		return abbreviation;
	}

	public void setAbbreviation(String abbreviation) {
		this.abbreviation = abbreviation;
	}

	public boolean getIsEndorsedOnce() {
		return isEndorsedOnce;
	}

	public void setIsEndorsedOnce(boolean isEndorsedOnce) {
		this.isEndorsedOnce = isEndorsedOnce;
	}

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getAwardTitle() {
		return awardTitle;
	}

	public void setAwardTitle(String awardTitle) {
		this.awardTitle = awardTitle;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getProposalTypeDescription() {
		return proposalTypeDescription;
	}

	public void setProposalTypeDescription(String proposalTypeDescription) {
		this.proposalTypeDescription = proposalTypeDescription;
	}

	public String getProposalCategory() {
		return proposalCategory;
	}

	public void setProposalCategory(String proposalCategory) {
		this.proposalCategory = proposalCategory;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public ProposalDocumentStatus getDocumentStatus() {
		return documentStatus;
	}

	public void setDocumentStatus(ProposalDocumentStatus documentStatus) {
		this.documentStatus = documentStatus;
	}

	public Integer getSourceProposalId() {
		return sourceProposalId;
	}

	public void setSourceProposalId(Integer sourceProposalId) {
		this.sourceProposalId = sourceProposalId;
	}

	public String getSourceProposalTitle() {
		return sourceProposalTitle;
	}

	public void setSourceProposalTitle(String sourceProposalTitle) {
		this.sourceProposalTitle = sourceProposalTitle;
	}

	public String getBaseProposalTitle() {
		return baseProposalTitle;
	}

	public void setBaseProposalTitle(String baseProposalTitle) {
		this.baseProposalTitle = baseProposalTitle;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getPrimeSponsorName() {
		return primeSponsorName;
	}

	public void setPrimeSponsorName(String primeSponsorName) {
		this.primeSponsorName = primeSponsorName;
	}

}
