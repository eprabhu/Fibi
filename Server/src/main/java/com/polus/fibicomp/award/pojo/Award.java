package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
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
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.budget.pojo.AwardBudgetStatus;
import com.polus.fibicomp.budget.pojo.AwardWorkflowStatus;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD")
public class Award implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_ID_GENERATOR")
	@SequenceGenerator(name = "AWARD_ID_GENERATOR", sequenceName = "AWARD_ID_GENERATOR", allocationSize = 1)
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "LEAD_UNIT_NUMBER")
	private String leadUnitNumber;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK3"), name = "LEAD_UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit leadUnit;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "AWARD_TYPE_CODE")
	private String awardTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK11"), name = "AWARD_TYPE_CODE", referencedColumnName = "AWARD_TYPE_CODE", insertable = false, updatable = false)
	private AwardType awardType;

	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK9"), name = "ACTIVITY_TYPE_CODE", referencedColumnName = "ACTIVITY_TYPE_CODE", insertable = false, updatable = false)
	private ActivityType activityType;

	@Column(name = "ACCOUNT_TYPE_CODE")
	private String accountTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK10"), name = "ACCOUNT_TYPE_CODE", referencedColumnName = "ACCOUNT_TYPE_CODE", insertable = false, updatable = false)
	private AccountType accountType;

	@Column(name = "STATUS_CODE")
	private String statusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK5"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private AwardStatus awardStatus;

	@Column(name = "SPONSOR_TEMPLATE_CODE")
	private Integer sponsorTemplateCode;

	@Column(name = "SPONSOR_AWARD_NUMBER")
	private String sponsorAwardNumber;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK1"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "AWARD_EXECUTION_DATE")
	private Timestamp awardExecutionDate;

	@Column(name = "AWARD_EFFECTIVE_DATE")
	private Timestamp awardEffectiveDate;

	@Column(name = "FINAL_EXPIRATION_DATE")
	private Timestamp finalExpirationDate;

	@Column(name = "BEGIN_DATE")
	private Timestamp beginDate;

	@Column(name = "PRE_AWARD_AUTHORIZED_AMOUNT")
	private Double preAwardAutherizedAmount;

	@Column(name = "SPECIAL_EB_RATE_OFF_CAMPUS")
	private Double specialEBRateOffCampus;

	@Column(name = "SPECIAL_EB_RATE_ON_CAMPUS")
	private Double specialEBRateOnCampus;

	@Column(name = "PRE_AWARD_EFFECTIVE_DATE")
	private Timestamp preAwardEffectiveDate;

	@Column(name = "PRIME_SPONSOR_CODE")
	private String primeSponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK2"), name = "PRIME_SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor primeSponsor;

	@Column(name = "AWARD_SEQUENCE_STATUS")
	private String awardSequenceStatus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_LATEST")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isLatest = false;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "WORKFLOW_AWARD_STATUS_CODE")
	private String workflowAwardStatusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK6"), name = "WORKFLOW_AWARD_STATUS_CODE", referencedColumnName = "WORKFLOW_AWARD_STATUS_CODE", insertable = false, updatable = false)
	private AwardWorkflowStatus awardWorkflowStatus;

	@Column(name = "AWARD_DOCUMENT_TYPE_CODE")
	private String awardDocumentTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK7"), name = "AWARD_DOCUMENT_TYPE_CODE", referencedColumnName = "AWARD_DOCUMENT_TYPE_CODE", insertable = false, updatable = false)
	private AwardDocumentType awardDocumentType;

	@Column(name = "AWARD_VARIATION_TYPE_CODE")
	private String awardVariationTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK8"), name = "AWARD_VARIATION_TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestType serviceRequestType;

	@JsonManagedReference
	@OneToMany(mappedBy = "award", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<AwardKeyword> awardKeywords;

	@OneToMany
	@JoinColumn(name = "AWARD_ID", insertable = false, updatable = false)
	private List<AwardPerson> awardPersons;

	@Column(name = "SUBMISSION_DATE")
	private Timestamp submissionDate;

	@Column(name = "SUBMIT_USER")
	private String submitUser;

	@Column(name = "FUND_CENTER")
	private String fundCenter;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK12"), name = "BASIS_OF_PAYMENT_CODE", referencedColumnName = "BASIS_OF_PAYMENT_CODE", insertable = false, updatable = false)
	private AwardBasisOfPayment awardBasisOfPayment;

	@Column(name = "BASIS_OF_PAYMENT_CODE")
	private String basisOfPaymentCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK13"), name = "METHOD_OF_PAYMENT_CODE", referencedColumnName = "METHOD_OF_PAYMENT_CODE", insertable = false, updatable = false)
	private AwardMethodOfPayment awardMethodOfPayment;

	@Column(name = "METHOD_OF_PAYMENT_CODE")
	private String methodOfPaymentCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FK14"), name = "PAYMENT_INVOICE_FREQ_CODE", referencedColumnName = "FREQUENCY_CODE", insertable = false, updatable = false)
	private Frequency frequency;

	@Column(name = "PAYMENT_INVOICE_FREQ_CODE")
	private String paymentInvoiceFrequencyCode;

	@Column(name = "INVOICE_NUMBER_OF_COPIES")
	private Integer invoiceNoOfCopies;

	@Column(name = "FINAL_INVOICE_DUE")
	private Integer finalInvoiceDue;

	@Column(name = "DFAFS_NUMBER")
	private String dfafsNumber;

	@Column(name = "INVOICE_INSTRUCTIONS")
	private String invoiceInstructions;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantHeaderId;

	@Column(name = "CFDA_NUMBER")
	private String cfdaNumber;

	@Column(name = "RESEARCH_AREA_DESC")
	private String researchDescription;

	@Column(name = "MULTI_DISCIPLINARY_DESC")
	private String multiDisciplinaryDescription;
	
	@Column(name = "DURATION")
	private String duration;

	@Column(name = "DOCUMENT_UPDATE_TIMESTAMP")
	private Timestamp documentUpdateTimeStamp;

	@Column(name = "DOCUMENT_UPDATE_USER")
	private String documentUpdateUser;

	@Column(name = "FUNDER_APPROVAL_DATE")
	private Timestamp funderApprovalDate;

	@Transient
	private String principalInvestigator;

	@Transient
	private String piPersonId;

	@Transient
	private Boolean canCreateVariationRequest = false;

	@Transient
	private String submitUserFullName;

	@Transient
	private String updateUserFullName;

	@Transient
	private String createUserFullName;

	@Transient
	private ServiceRequest serviceRequest;

	@Transient
	private String workFlowStatusName;

	@Transient
	private String serviceRequestSubject;

	@Transient
	private String principalInvestigatorId;

	@Transient
	private String grantCallName;

	@Transient
	private Integer awardPersonId;

	@Transient
	private Boolean viewTimesheetRightExist = false;

	@Transient
	private String personRoleName;

  	@Transient
	private List<ModuleVariableSection> moduleVariableSections;

	@Transient
	private SapFeedStatus sapFeedStatus;

	@Transient
	private AwardBudgetStatus budgetStatus;

	@Transient
	private String durationInMonths;

	private transient String sponsorName;

	private transient String primeSponsorName;

	public Award() {
		awardKeywords = new ArrayList<>();
		awardPersons = new ArrayList<>();
		moduleVariableSections = new ArrayList<>();
	}

	public Award(Integer sequenceNumber, String awardSequenceStatus) {
		this.sequenceNumber = sequenceNumber;
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public Award(Integer awardId, String awardNumber, Integer sequenceNumber, String title,
			String awardVariationTypeCode,  String createUser, String awardSequenceStatus) {
		super();
		this.awardId = awardId;
		this.awardNumber = awardNumber;
		this.sequenceNumber = sequenceNumber;
		this.title = title;
		this.awardVariationTypeCode = awardVariationTypeCode;
		this.createUser = createUser;
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public Award(Integer awardId, String awardNumber, Integer sequenceNumber, String leadUnitNumber, Unit leadUnit,
			String title, String statusCode, AwardStatus awardStatus, String awardSequenceStatus,
			ServiceRequestType serviceRequestType, Object submissionDate, String submitUserFullName,
			String createUserFullName, String updateUserFullName, String pIName, Object updateTimeStamp, 
			Object createTimestamp) {
		super();
		this.awardId = awardId;
		this.awardNumber = awardNumber;
		this.sequenceNumber = sequenceNumber;
		this.leadUnitNumber = leadUnitNumber;
		this.leadUnit = leadUnit;
		this.title = title;
		this.statusCode = statusCode;
		this.awardStatus = awardStatus;
		this.awardSequenceStatus = awardSequenceStatus;
		this.serviceRequestType = serviceRequestType;
		this.submissionDate = (Timestamp) submissionDate;
		this.submitUserFullName = submitUserFullName;
		this.createUserFullName = createUserFullName;
		this.updateUserFullName = updateUserFullName;
		this.updateTimeStamp = (Timestamp) updateTimeStamp;
		this.createTimestamp = (Timestamp) createTimestamp;
		if (pIName != null) {
			AwardPerson pI = new AwardPerson();
			pI.setFullName(pIName);
			pI.setIsPi(true);
			List<AwardPerson> awardPIPersons = new ArrayList<AwardPerson>();
			awardPIPersons.add(pI);
			this.awardPersons = awardPIPersons;
		}
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public String getAwardTypeCode() {
		return awardTypeCode;
	}

	public void setAwardTypeCode(String awardTypeCode) {
		this.awardTypeCode = awardTypeCode;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public String getAccountTypeCode() {
		return accountTypeCode;
	}

	public void setAccountTypeCode(String accountTypeCode) {
		this.accountTypeCode = accountTypeCode;
	}

	public String getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(String statusCode) {
		this.statusCode = statusCode;
	}

	public Integer getSponsorTemplateCode() {
		return sponsorTemplateCode;
	}

	public void setSponsorTemplateCode(Integer sponsorTemplateCode) {
		this.sponsorTemplateCode = sponsorTemplateCode;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Timestamp getAwardExecutionDate() {
		return awardExecutionDate;
	}

	public void setAwardExecutionDate(Timestamp awardExecutionDate) {
		this.awardExecutionDate = awardExecutionDate;
	}

	public Timestamp getAwardEffectiveDate() {
		return awardEffectiveDate;
	}

	public void setAwardEffectiveDate(Timestamp awardEffectiveDate) {
		this.awardEffectiveDate = awardEffectiveDate;
	}

	public Timestamp getFinalExpirationDate() {
		return finalExpirationDate;
	}

	public void setFinalExpirationDate(Timestamp finalExpirationDate) {
		this.finalExpirationDate = finalExpirationDate;
	}

	public Timestamp getBeginDate() {
		return beginDate;
	}

	public void setBeginDate(Timestamp beginDate) {
		this.beginDate = beginDate;
	}

	public Double getPreAwardAutherizedAmount() {
		return preAwardAutherizedAmount;
	}

	public void setPreAwardAutherizedAmount(Double preAwardAutherizedAmount) {
		this.preAwardAutherizedAmount = preAwardAutherizedAmount;
	}

	public Double getSpecialEBRateOffCampus() {
		return specialEBRateOffCampus;
	}

	public void setSpecialEBRateOffCampus(Double specialEBRateOffCampus) {
		this.specialEBRateOffCampus = specialEBRateOffCampus;
	}

	public Double getSpecialEBRateOnCampus() {
		return specialEBRateOnCampus;
	}

	public void setSpecialEBRateOnCampus(Double specialEBRateOnCampus) {
		this.specialEBRateOnCampus = specialEBRateOnCampus;
	}

	public Timestamp getPreAwardEffectiveDate() {
		return preAwardEffectiveDate;
	}

	public void setPreAwardEffectiveDate(Timestamp preAwardEffectiveDate) {
		this.preAwardEffectiveDate = preAwardEffectiveDate;
	}

	public String getPrimeSponsorCode() {
		return primeSponsorCode;
	}

	public void setPrimeSponsorCode(String primeSponsorCode) {
		this.primeSponsorCode = primeSponsorCode;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
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

	public Unit getLeadUnit() {
		return leadUnit;
	}

	public void setLeadUnit(Unit leadUnit) {
		this.leadUnit = leadUnit;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public Sponsor getPrimeSponsor() {
		return primeSponsor;
	}

	public void setPrimeSponsor(Sponsor primeSponsor) {
		this.primeSponsor = primeSponsor;
	}

	public Boolean getIsLatest() {
		return isLatest;
	}

	public void setIsLatest(Boolean isLatest) {
		this.isLatest = isLatest;
	}

	public AwardStatus getAwardStatus() {
		return awardStatus;
	}

	public void setAwardStatus(AwardStatus awardStatus) {
		this.awardStatus = awardStatus;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public List<AwardKeyword> getAwardKeywords() {
		return awardKeywords;
	}

	public void setAwardKeywords(List<AwardKeyword> awardKeywords) {
		this.awardKeywords = awardKeywords;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public List<AwardPerson> getAwardPersons() {
		return awardPersons;
	}

	public void setAwardPersons(List<AwardPerson> awardPersons) {
		this.awardPersons = awardPersons;
	}

	public String getPrincipalInvestigator() {
		AwardPerson pi = null;
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson person : awardPersons) {
				if (person != null) {
					if (person.getIsPi()) {
						pi = person;
						break;
					}
				}
			}
		}
		principalInvestigator = pi != null ? pi.getFullName() : null;
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
	}

	public String getWorkflowAwardStatusCode() {
		return workflowAwardStatusCode;
	}

	public void setWorkflowAwardStatusCode(String workflowAwardStatusCode) {
		this.workflowAwardStatusCode = workflowAwardStatusCode;
	}

	public AwardWorkflowStatus getAwardWorkflowStatus() {
		return awardWorkflowStatus;
	}

	public void setAwardWorkflowStatus(AwardWorkflowStatus awardWorkflowStatus) {
		this.awardWorkflowStatus = awardWorkflowStatus;
	}

	public String getAwardDocumentTypeCode() {
		return awardDocumentTypeCode;
	}

	public void setAwardDocumentTypeCode(String awardDocumentTypeCode) {
		this.awardDocumentTypeCode = awardDocumentTypeCode;
	}

	public AwardDocumentType getAwardDocumentType() {
		return awardDocumentType;
	}

	public void setAwardDocumentType(AwardDocumentType awardDocumentType) {
		this.awardDocumentType = awardDocumentType;
	}

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public Boolean getCanCreateVariationRequest() {
		return canCreateVariationRequest;
	}

	public void setCanCreateVariationRequest(Boolean canCreateVariationRequest) {
		this.canCreateVariationRequest = canCreateVariationRequest;
	}

	public String getSubmitUser() {
		return submitUser;
	}

	public void setSubmitUser(String submitUser) {
		this.submitUser = submitUser;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public ActivityType getActivityType() {
		return activityType;
	}

	public void setActivityType(ActivityType activityType) {
		this.activityType = activityType;
	}

	public AwardType getAwardType() {
		return awardType;
	}

	public void setAwardType(AwardType awardType) {
		this.awardType = awardType;
	}

	public AccountType getAccountType() {
		return accountType;
	}

	public void setAccountType(AccountType accountType) {
		this.accountType = accountType;
	}

	public int getIndexOfLastAwardAmountInfo(List<AwardAmountInfo> awardAmountInfos) {
		return awardAmountInfos.size() - 1;
	}

	public AwardAmountInfo getLastAwardAmountInfo(List<AwardAmountInfo> awardAmountInfos) {
		return awardAmountInfos.get(getIndexOfLastAwardAmountInfo(awardAmountInfos));
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public ServiceRequest getServiceRequest() {
		return serviceRequest;
	}

	public void setServiceRequest(ServiceRequest serviceRequest) {
		this.serviceRequest = serviceRequest;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getPiPersonId() {
		AwardPerson pi = null;
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson person : awardPersons) {
				if (person != null) {
					if (person.getProposalPersonRole() != null && (StringUtils.equals(person.getProposalPersonRole().getCode(),
							Constants.PRINCIPAL_INVESTIGATOR))) {
						pi = person;
						break;
					}
				}
			}
		}
		piPersonId = pi != null ? pi.getPersonId() : null;
		return piPersonId;
	}

	public void setPiPersonId(String piPersonId) {
		this.piPersonId = piPersonId;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public String getWorkFlowStatusName() {
		return workFlowStatusName;
	}

	public void setWorkFlowStatusName(String workFlowStatusName) {
		this.workFlowStatusName = workFlowStatusName;
	}

	public String getServiceRequestSubject() {
		return serviceRequestSubject;
	}

	public void setServiceRequestSubject(String serviceRequestSubject) {
		this.serviceRequestSubject = serviceRequestSubject;
	}

	public Timestamp getSubmissionDate() {
		return submissionDate;
	}

	public void setSubmissionDate(Timestamp submissionDate) {
		this.submissionDate = submissionDate;
	}

	public AwardBasisOfPayment getAwardBasisOfPayment() {
		return awardBasisOfPayment;
	}

	public void setAwardBasisOfPayment(AwardBasisOfPayment awardBasisOfPayment) {
		this.awardBasisOfPayment = awardBasisOfPayment;
	}

	public String getBasisOfPaymentCode() {
		return basisOfPaymentCode;
	}

	public void setBasisOfPaymentCode(String basisOfPaymentCode) {
		this.basisOfPaymentCode = basisOfPaymentCode;
	}

	public AwardMethodOfPayment getAwardMethodOfPayment() {
		return awardMethodOfPayment;
	}

	public void setAwardMethodOfPayment(AwardMethodOfPayment awardMethodOfPayment) {
		this.awardMethodOfPayment = awardMethodOfPayment;
	}

	public String getMethodOfPaymentCode() {
		return methodOfPaymentCode;
	}

	public void setMethodOfPaymentCode(String methodOfPaymentCode) {
		this.methodOfPaymentCode = methodOfPaymentCode;
	}

	public Frequency getFrequency() {
		return frequency;
	}

	public void setFrequency(Frequency frequency) {
		this.frequency = frequency;
	}

	public Integer getInvoiceNoOfCopies() {
		return invoiceNoOfCopies;
	}

	public void setInvoiceNoOfCopies(Integer invoiceNoOfCopies) {
		this.invoiceNoOfCopies = invoiceNoOfCopies;
	}

	public Integer getFinalInvoiceDue() {
		return finalInvoiceDue;
	}

	public void setFinalInvoiceDue(Integer finalInvoiceDue) {
		this.finalInvoiceDue = finalInvoiceDue;
	}

	public String getDfafsNumber() {
		return dfafsNumber;
	}

	public void setDfafsNumber(String dfafsNumber) {
		this.dfafsNumber = dfafsNumber;
	}

	public String getInvoiceInstructions() {
		return invoiceInstructions;
	}

	public void setInvoiceInstructions(String invoiceInstructions) {
		this.invoiceInstructions = invoiceInstructions;
	}

	public String getPaymentInvoiceFrequencyCode() {
		return paymentInvoiceFrequencyCode;
	}

	public void setPaymentInvoiceFrequencyCode(String paymentInvoiceFrequencyCode) {
		this.paymentInvoiceFrequencyCode = paymentInvoiceFrequencyCode;
	}

	public String getPrincipalInvestigatorId() {
		return principalInvestigatorId;
	}

	public void setPrincipalInvestigatorId(String principalInvestigatorId) {
		this.principalInvestigatorId = principalInvestigatorId;
	}

	public Integer getGrantHeaderId() {
		return grantHeaderId;
	}

	public void setGrantHeaderId(Integer grantHeaderId) {
		this.grantHeaderId = grantHeaderId;
	}

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public String getCfdaNumber() {
		return cfdaNumber;
	}

	public void setCfdaNumber(String cfdaNumber) {
		this.cfdaNumber = cfdaNumber;
	}

	public String getResearchDescription() {
		return researchDescription;
	}

	public void setResearchDescription(String researchDescription) {
		this.researchDescription = researchDescription;
	}

	public String getMultiDisciplinaryDescription() {
		return multiDisciplinaryDescription;
	}

	public void setMultiDisciplinaryDescription(String multiDisciplinaryDescription) {
		this.multiDisciplinaryDescription = multiDisciplinaryDescription;
	}

	public String getAwardVariationTypeCode() {
		return awardVariationTypeCode;
	}

	public void setAwardVariationTypeCode(String awardVariationTypeCode) {
		this.awardVariationTypeCode = awardVariationTypeCode;
	}

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}

	public Timestamp getDocumentUpdateTimeStamp() {
		return documentUpdateTimeStamp;
	}

	public void setDocumentUpdateTimeStamp(Timestamp documentUpdateTimeStamp) {
		this.documentUpdateTimeStamp = documentUpdateTimeStamp;
	}

	public String getDocumentUpdateUser() {
		return documentUpdateUser;
	}

	public void setDocumentUpdateUser(String documentUpdateUser) {
		this.documentUpdateUser = documentUpdateUser;
	}

	public Timestamp getFunderApprovalDate() {
		return funderApprovalDate;
	}

	public void setFunderApprovalDate(Timestamp funderApprovalDate) {
		this.funderApprovalDate = funderApprovalDate;
	}
	
	public List<ModuleVariableSection> getModuleVariableSections() {
		return moduleVariableSections;
	}

	public void setModuleVariableSections(List<ModuleVariableSection> moduleVariableSections) {
		this.moduleVariableSections = moduleVariableSections;
	}

	public SapFeedStatus getSapFeedStatus() {
		return sapFeedStatus;
	}

	public void setSapFeedStatus(SapFeedStatus sapFeedStatus) {
		this.sapFeedStatus = sapFeedStatus;
	}

	public AwardBudgetStatus getBudgetStatus() {
		return budgetStatus;
	}

	public void setBudgetStatus(AwardBudgetStatus budgetStatus) {
		this.budgetStatus = budgetStatus;
	}

	public Integer getAwardPersonId() {
		return awardPersonId;
	}

	public void setAwardPersonId(Integer awardPersonId) {
		this.awardPersonId = awardPersonId;
	}

	public Boolean getViewTimesheetRightExist() {
		return viewTimesheetRightExist;
	}

	public void setViewTimesheetRightExist(Boolean viewTimesheetRightExist) {
		this.viewTimesheetRightExist = viewTimesheetRightExist;
	}

	public String getPersonRoleName() {
		return personRoleName;
	}

	public void setPersonRoleName(String personRoleName) {
		this.personRoleName = personRoleName;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public String getPrimeSponsorName() {
		return primeSponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public void setPrimeSponsorName(String primeSponsorName) {
		this.primeSponsorName = primeSponsorName;
	}

	public String getDurationInMonths() {
		return durationInMonths;
	}

	public void setDurationInMonths(String durationInMonths) {
		this.durationInMonths = durationInMonths;
	}
}
