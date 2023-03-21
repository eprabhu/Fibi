package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.OneToMany;
import javax.persistence.FetchType;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.proposal.pojo.DisciplineCluster;
import com.polus.fibicomp.util.JpaCharBooleanConversion;
import com.polus.fibicomp.workflow.pojo.Workflow;

@Entity
@Table(name = "PROPOSAL")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposal implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_ID_GNTR")
	@SequenceGenerator(name="SEQ_IP_ID_GNTR", sequenceName = "SEQ_IP_ID_GNTR", allocationSize=1)
	private Integer proposalId;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK2"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private InstituteProposalStatus instProposalStatus;

	@Column(name = "TYPE_CODE")
	private Integer typeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK1"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private InstituteProposalType instProposalType;

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

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IP_NUMBER")
	private String ipNumber;

	@Column(name = "PROPOSAL_NUMBER")
	private String proposalNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "HOME_UNIT_NUMBER")
	private String homeUnitNumber;

	@Column(name = "HOME_UNIT_NAME")
	private String homeUnitName;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK11"), name = "HOME_UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK8"), name = "ACTIVITY_TYPE_CODE", referencedColumnName = "ACTIVITY_TYPE_CODE", insertable = false, updatable = false)
	private ActivityType activityType;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK12"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "SUBMIT_USER")
	private String submitUser;

	@Column(name = "SPONSOR_PROPOSAL_NUMBER")
	private String sponsorProposalNumber;

	@Column(name = "CFDA_NUMBER")
	private String cfdaNumber;

	@Column(name = "BASE_PROPOSAL_NUMBER")
	private String baseProposalNumber;

	@Column(name = "PROGRAM_ANNOUNCEMENT_NUMBER")
	private String programAnnouncementNumber;

	@Column(name = "PRIME_SPONSOR_CODE")
	private String primeSponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK13"), name = "PRIME_SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor primeSponsor;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK4"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID", insertable = false, updatable = false)
	private GrantCall grantCall;

	@Column(name = "IS_SUBCONTRACT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSubcontract = false;

	@Column(name = "MULTI_DISCIPLINARY_DESC")
	private String multiDisciplinaryDescription;

	@Column(name = "PROPOSAL_SEQUENCE_STATUS")
	private String proposalSequenceStatus;

	@Column(name = "SPONSOR_DEADLINE_DATE")
	private Timestamp sponsorDeadlineDate;

	@Column(name = "CLUSTER_CODE")
	private Integer clusterCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK14"), name = "CLUSTER_CODE", referencedColumnName = "CLUSTER_CODE", insertable = false, updatable = false)
	private DisciplineCluster disciplineCluster;

	@Column(name = "EXTERNAL_FUNDING_AGENCY_ID")
	private String externalFundingAgencyId;

	@JsonManagedReference
	@OneToMany(mappedBy = "instProposal", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<InstituteProposalKeywords> instProposalKeywords;

	@Transient
	private String principalInvestigator;

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
	private String createUserFullName;

	@Transient
	private String updateUserFullName;

	@Column(name = "GRANT_TYPE_CODE")
	private Integer grantTypeCode;

	@ManyToOne(optional = true, cascade = { CascadeType.ALL })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK10"), name = "GRANT_TYPE_CODE", referencedColumnName = "GRANT_TYPE_CODE", insertable = false, updatable = false)
	private GrantCallType grantCallType;

	@Column(name = "AWARD_TYPE_CODE")
	private String awardTypecode;

	@ManyToOne(optional = true, cascade = { CascadeType.ALL })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_FK9"), name = "AWARD_TYPE_CODE", referencedColumnName = "AWARD_TYPE_CODE", insertable = false, updatable = false)
	private AwardType awardType;

	@Column(name = "DURATION")
	private String duration;

	@Transient
	private List<InstituteProposalPerson> instProposalPersons;

	@Transient
	private Boolean isRcbfProposal = false;
	
	@Transient
	private String baseProposalTitle;

	private transient String sponsorName;

	private transient String primeSponsorName;

	public String getBaseProposalTitle() {
		return baseProposalTitle;
	}

	public void setBaseProposalTitle(String baseProposalTitle) {
		this.baseProposalTitle = baseProposalTitle;
	}

	public InstituteProposal() {
		this.instProposalPersons = new ArrayList<>();
		this.instProposalKeywords = new ArrayList<>();
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public Integer getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(Integer typeCode) {
		this.typeCode = typeCode;
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

	public Timestamp getInternalDeadLineDate() {
		return internalDeadLineDate;
	}

	public void setInternalDeadLineDate(Timestamp internalDeadLineDate) {
		this.internalDeadLineDate = internalDeadLineDate;
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

	public String getIpNumber() {
		return ipNumber;
	}

	public void setIpNumber(String ipNumber) {
		this.ipNumber = ipNumber;
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

	public Boolean getIsSubcontract() {
		return isSubcontract;
	}

	public void setIsSubcontract(Boolean isSubcontract) {
		this.isSubcontract = isSubcontract;
	}

	public String getPrincipalInvestigator() {
		InstituteProposalPerson pi = null;
		if (instProposalPersons != null && instProposalPersons.isEmpty()) {
			return principalInvestigator;
		}
		for (InstituteProposalPerson person : instProposalPersons) {
			if (StringUtils.equals(person.getProposalPersonRole().getCode(), Constants.PRINCIPAL_INVESTIGATOR)) {
				pi = person;
				break;
			}
		}
		principalInvestigator = pi != null ? pi.getFullName() : null;
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
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

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
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
		return principalInvestigator;
	}

	public void setPrincipalInvestigatorForMobile(String principalInvestigatorForMobile) {
		this.principalInvestigatorForMobile = principalInvestigatorForMobile;
	}

	public String getProposalNumber() {
		return proposalNumber;
	}

	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public List<InstituteProposalPerson> getInstProposalPersons() {
		return instProposalPersons;
	}

	public void setInstProposalPersons(List<InstituteProposalPerson> instProposalPersons) {
		this.instProposalPersons = instProposalPersons;
	}

	public InstituteProposalStatus getInstProposalStatus() {
		return instProposalStatus;
	}

	public void setInstProposalStatus(InstituteProposalStatus instProposalStatus) {
		this.instProposalStatus = instProposalStatus;
	}

	public InstituteProposalType getInstProposalType() {
		return instProposalType;
	}

	public void setInstProposalType(InstituteProposalType instProposalType) {
		this.instProposalType = instProposalType;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getCfdaNumber() {
		return cfdaNumber;
	}

	public void setCfdaNumber(String cfdaNumber) {
		this.cfdaNumber = cfdaNumber;
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

	public String getMultiDisciplinaryDescription() {
		return multiDisciplinaryDescription;
	}

	public void setMultiDisciplinaryDescription(String multiDisciplinaryDescription) {
		this.multiDisciplinaryDescription = multiDisciplinaryDescription;
	}

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public String getAwardTypecode() {
		return awardTypecode;
	}

	public void setAwardTypecode(String awardTypecode) {
		this.awardTypecode = awardTypecode;
	}

	public AwardType getAwardType() {
		return awardType;
	}

	public void setAwardType(AwardType awardType) {
		this.awardType = awardType;
	}

	public GrantCallType getGrantCallType() {
		return grantCallType;
	}

	public void setGrantCallType(GrantCallType grantCallType) {
		this.grantCallType = grantCallType;
	}

	public String getProposalSequenceStatus() {
		return proposalSequenceStatus;
	}

	public void setProposalSequenceStatus(String proposalSequenceStatus) {
		this.proposalSequenceStatus = proposalSequenceStatus;
	}

	public Boolean getIsRcbfProposal() {
		return isRcbfProposal;
	}

	public void setIsRcbfProposal(Boolean isRcbfProposal) {
		this.isRcbfProposal = isRcbfProposal;
	}

	public Timestamp getSponsorDeadlineDate() {
		return sponsorDeadlineDate;
	}

	public void setSponsorDeadlineDate(Timestamp sponsorDeadlineDate) {
		this.sponsorDeadlineDate = sponsorDeadlineDate;
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

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}


	public List<InstituteProposalKeywords> getInstProposalKeywords() {
		return instProposalKeywords;
	}

	public void setInstProposalKeywords(List<InstituteProposalKeywords> instProposalKeywords) {
		this.instProposalKeywords = instProposalKeywords;
	}

}
