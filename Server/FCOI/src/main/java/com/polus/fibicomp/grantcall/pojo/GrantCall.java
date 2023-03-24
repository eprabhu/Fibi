package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

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
import javax.persistence.JoinColumns;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "GRANT_CALL_HEADER")
public class GrantCall implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_HEADER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_HEADER_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_HEADER_ID_GENERATOR", sequenceName = "GRANT_HEADER_ID_GENERATOR", allocationSize=1)
	private Integer grantCallId;

	@Column(name = "OPENING_DATE")
	private Timestamp openingDate;

	@Column(name = "CLOSING_DATE")
	private Timestamp closingDate;

	/* Not Needed in master KU Specific - Nikhil 25/04/2019 */
//	@Column(name = "INTERNAL_DEADLINE_DATE")
//	private Timestamp internalDeadLineDate;

	@Column(name = "INTERNAL_SUBMISSION_DEADLINE_DATE")
	private Timestamp internalSubmissionDeadLineDate;

	@Column(name = "NAME")
	private String grantCallName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "MAX_BUDGET", precision = 10, scale = 2)
	private BigDecimal maximumBudget;

	@Column(name = "QUANTUM", precision = 10, scale = 2)
	private BigDecimal quantum;

	@Column(name = "GRANT_THEME")
	private String grantTheme;

	@Column(name = "GRANT_TYPE_CODE")
	private Integer grantTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK1"), name = "GRANT_TYPE_CODE", referencedColumnName = "GRANT_TYPE_CODE", insertable = false, updatable = false)
	private GrantCallType grantCallType;

	@Column(name = "GRANT_STATUS_CODE")
	private Integer grantStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK2"), name = "GRANT_STATUS_CODE", referencedColumnName = "GRANT_STATUS_CODE", insertable = false, updatable = false)
	private GrantCallStatus grantCallStatus;

	@Column(name = "FUNDING_SCHEME_ID")
	private Integer fundingSchemeId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK3"), name = "FUNDING_SCHEME_ID", referencedColumnName = "FUNDING_SCHEME_ID", insertable = false, updatable = false)
	private SponsorFundingScheme sponsorFundingScheme;

	@Column(name = "SPONSOR_TYPE_CODE")
	private String sponsorTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK4"), name = "SPONSOR_TYPE_CODE", referencedColumnName = "SPONSOR_TYPE_CODE", insertable = false, updatable = false)
	private SponsorType sponsorType;

	@Column(name = "CURRENCY_CODE")
	private String currencyCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK9"), name = "CURRENCY_CODE", referencedColumnName = "CURRENCY_CODE", insertable = false, updatable = false)
	private Currency currency;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK6"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "APPLICATION_PROCEDURE")
	private String applicationProcedure;

	@Column(name = "OTHER_INFORMATION")
	private String otherInformation;

	@Column(name = "EXTERNAL_URL")
	private String externalUrl;
    
	@Column(name = "ABBREVIATION")
	private String abbrevation;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PUBLISH_TIMESTAMP")
	private Timestamp publishTimeStamp;

	@Column(name = "IS_PUBLISHED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPublished = false;
	
	@Column(name = "HOME_UNIT_NUMBER")
	private String homeUnitNumber;

	@Column(name = "HOME_UNIT_NAME")
	private String homeUnitName;

	@Column(name = "PRIME_SPONSOR_CODE")
	private String primeSponsorCode;

	@Column(name = "OVERHEAD_COMMENT")
    private String overHeadComment;

	@Column(name = "RATE_TYPE_CODE")
	private String rateTypeCode;

	@Column(name = "RATE_CLASS_CODE")
	private String rateClassCode;

	@ManyToOne(optional = true)
	@JoinColumns(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK12"), value = {
			@JoinColumn(name = "RATE_CLASS_CODE", referencedColumnName = "RATE_CLASS_CODE", insertable = false, updatable = false),
			@JoinColumn(name = "RATE_TYPE_CODE", referencedColumnName = "RATE_TYPE_CODE", insertable = false, updatable = false) })
	private RateType rateType;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_HEADER_FK11"), name = "PRIME_SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor primeSponsor;

	@JsonManagedReference
	@OneToMany(mappedBy = "grantCall", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<GrantCallKeyword> grantCallKeywords;

	@JsonManagedReference
	@OneToMany(mappedBy = "grantCall", orphanRemoval = true, cascade = { CascadeType.ALL, CascadeType.REMOVE }, fetch = FetchType.LAZY)
	private List<GrantCallRelevant> grantCallRelevants;

	@JsonManagedReference
	@OneToOne(mappedBy = "grantCall", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private GrantCallIOIQuestionnaire grantCallIOIQuestionnaire;

	@JsonIgnore
	@OneToMany
	@JoinColumn(name = "GRANT_HEADER_ID", insertable = false, updatable = false)
	private List<GrantCallEligibility> grantCallEligibility;

	@Transient
	private String grantCallTypeDesc;

	@Transient
	private String grantCallStatusDesc;

	@Transient
	private String sponsorName;

	@Transient
	private List<Proposal> proposals;

	@Transient
	private String createUserFullName;

	@Transient
	private String lastUpdateUserFullName;

	@Transient
	private String closingTime;

	private transient String primeSponsorName;
	
	@Transient
	private List<String> relevantFields;

	public GrantCall() {
		grantCallKeywords = new ArrayList<>();
		grantCallRelevants = new ArrayList<>();
	}

	public GrantCall(Integer grantCallId, String grantCallName, SponsorFundingScheme sponsorFundingScheme, Timestamp  closingDate, String sponsorName, GrantCallType grantCallType, GrantCallStatus grantCallStatus, Timestamp internalSubmissionDeadLineDate) {
		super();
		this.grantCallId = grantCallId;
		this.grantCallName = grantCallName;
		this.sponsorFundingScheme = sponsorFundingScheme;
		this.closingDate = closingDate;
		this.sponsorName = sponsorName;
		this.grantCallType = grantCallType;
		this.grantCallStatus = grantCallStatus;
		this.internalSubmissionDeadLineDate = internalSubmissionDeadLineDate;
	}

	public GrantCall(String grantCallName, Timestamp closingDate) {
		super();
		this.closingDate = closingDate;
		this.grantCallName = grantCallName;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public String getGrantTheme() {
		return grantTheme;
	}

	public void setGrantTheme(String grantTheme) {
		this.grantTheme = grantTheme;
	}

	public GrantCallType getGrantCallType() {
		return grantCallType;
	}

	public void setGrantCallType(GrantCallType grantCallType) {
		this.grantCallType = grantCallType;
	}

	public Integer getGrantStatusCode() {
		return grantStatusCode;
	}

	public void setGrantStatusCode(Integer grantStatusCode) {
		this.grantStatusCode = grantStatusCode;
	}

	public GrantCallStatus getGrantCallStatus() {
		return grantCallStatus;
	}

	public void setGrantCallStatus(GrantCallStatus grantCallStatus) {
		this.grantCallStatus = grantCallStatus;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public SponsorType getSponsorType() {
		return sponsorType;
	}

	public void setSponsorType(SponsorType sponsorType) {
		this.sponsorType = sponsorType;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public Currency getCurrency() {
		return currency;
	}

	public void setCurrency(Currency currency) {
		this.currency = currency;
	}

	public String getApplicationProcedure() {
		return applicationProcedure;
	}

	public void setApplicationProcedure(String applicationProcedure) {
		this.applicationProcedure = applicationProcedure;
	}

	public String getOtherInformation() {
		return otherInformation;
	}

	public void setOtherInformation(String otherInformation) {
		this.otherInformation = otherInformation;
	}

	public String getExternalUrl() {
		return externalUrl;
	}

	public void setExternalUrl(String externalUrl) {
		this.externalUrl = externalUrl;
	}

	public List<GrantCallKeyword> getGrantCallKeywords() {
		return grantCallKeywords;
	}

	public void setGrantCallKeywords(List<GrantCallKeyword> grantCallKeywords) {
		this.grantCallKeywords = grantCallKeywords;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public String getGrantCallTypeDesc() {
		return grantCallTypeDesc;
	}

	public void setGrantCallTypeDesc(String grantCallTypeDesc) {
		this.grantCallTypeDesc = grantCallTypeDesc;
	}

	public String getGrantCallStatusDesc() {
		return grantCallStatusDesc;
	}

	public void setGrantCallStatusDesc(String grantCallStatusDesc) {
		this.grantCallStatusDesc = grantCallStatusDesc;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public Timestamp getOpeningDate() {
		return openingDate;
	}

	public void setOpeningDate(Timestamp openingDate) {
		this.openingDate = openingDate;
	}

	public Timestamp getClosingDate() {
		return closingDate;
	}

	public void setClosingDate(Timestamp closingDate) {
		this.closingDate = closingDate;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public SponsorFundingScheme getSponsorFundingScheme() {
		return sponsorFundingScheme;
	}

	public void setSponsorFundingScheme(SponsorFundingScheme sponsorFundingScheme) {
		this.sponsorFundingScheme = sponsorFundingScheme;
	}

	public List<Proposal> getProposals() {
		return proposals;
	}

	public void setProposals(List<Proposal> proposals) {
		this.proposals = proposals;
	}

	public Timestamp getInternalSubmissionDeadLineDate() {
		return internalSubmissionDeadLineDate;
	}

	public void setInternalSubmissionDeadLineDate(Timestamp internalSubmissionDeadLineDate) {
		this.internalSubmissionDeadLineDate = internalSubmissionDeadLineDate;
	}

	public BigDecimal getMaximumBudget() {
		return maximumBudget;
	}

	public void setMaximumBudget(BigDecimal maximumBudget) {
		this.maximumBudget = maximumBudget;
	}

	public Timestamp getPublishTimeStamp() {
		return publishTimeStamp;
	}

	public void setPublishTimeStamp(Timestamp publishTimeStamp) {
		this.publishTimeStamp = publishTimeStamp;
	}

	public Boolean getIsPublished() {
		return isPublished;
	}

	public void setIsPublished(Boolean isPublished) {
		this.isPublished = isPublished;
	}

	public List<GrantCallRelevant> getGrantCallRelevants() {
		return grantCallRelevants;
	}

	public void setGrantCallRelevants(List<GrantCallRelevant> grantCallRelevants) {
		this.grantCallRelevants = grantCallRelevants;
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

	public BigDecimal getQuantum() {
		return quantum;
	}

	public void setQuantum(BigDecimal quantum) {
		this.quantum = quantum;
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

	public GrantCallIOIQuestionnaire getGrantCallIOIQuestionnaire() {
		return grantCallIOIQuestionnaire;
	}

	public void setGrantCallIOIQuestionnaire(GrantCallIOIQuestionnaire grantCallIOIQuestionnaire) {
		this.grantCallIOIQuestionnaire = grantCallIOIQuestionnaire;
	}

	public String getAbbrevation() {
		return abbrevation;
	}

	public void setAbbrevation(String abbrevation) {
		this.abbrevation = abbrevation;
	}

	public Integer getFundingSchemeId() {
		return fundingSchemeId;
	}

	public void setFundingSchemeId(Integer fundingSchemeId) {
		this.fundingSchemeId = fundingSchemeId;
	}

	public String getPrimeSponsorCode() {
		return primeSponsorCode;
	}

	public void setPrimeSponsorCode(String primeSponsorCode) {
		this.primeSponsorCode = primeSponsorCode;
	}

	public String getOverHeadComment() {
		return overHeadComment;
	}

	public void setOverHeadComment(String overHeadComment) {
		this.overHeadComment = overHeadComment;
	}

	public String getRateTypeCode() {
		return rateTypeCode;
	}

	public void setRateTypeCode(String rateTypeCode) {
		this.rateTypeCode = rateTypeCode;
	}

	public String getRateClassCode() {
		return rateClassCode;
	}

	public void setRateClassCode(String rateClassCode) {
		this.rateClassCode = rateClassCode;
	}

	public RateType getRateType() {
		return rateType;
	}

	public void setRateType(RateType rateType) {
		this.rateType = rateType;
	}

	public Sponsor getPrimeSponsor() {
		return primeSponsor;
	}

	public void setPrimeSponsor(Sponsor primeSponsor) {
		this.primeSponsor = primeSponsor;
	}

	public String getClosingTime() {
		return closingTime;
	}

	public void setClosingTime(String closingTime) {
		this.closingTime = closingTime;
	}

	public List<GrantCallEligibility> getGrantCallEligibility() {
		return grantCallEligibility;
	}

	public void setGrantCallEligibility(List<GrantCallEligibility> grantCallEligibility) {
		this.grantCallEligibility = grantCallEligibility;
	}

	public String getPrimeSponsorName() {
		return primeSponsorName;
	}

	public void setPrimeSponsorName(String primeSponsorName) {
		this.primeSponsorName = primeSponsorName;
	}

	public List<String> getRelevantFields() {
		return relevantFields;
	}

	public void setRelevantFields(List<String> relevantFields) {
		this.relevantFields = relevantFields;
	}
}
