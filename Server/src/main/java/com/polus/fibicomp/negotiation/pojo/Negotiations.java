package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
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
import com.polus.fibicomp.negotiation.dto.NegotiationProjectDetailsDto;

@Entity
@Table(name = "NEGOTIATION")
public class Negotiations implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_NEGOTIATION")
	@SequenceGenerator(name = "SEQ_NEGOTIATION", sequenceName = "SEQ_NEGOTIATION", allocationSize = 1)
	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@Column(name = "NEGOTIATION_STATUS_CODE")
	private String negotiationStatusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_FK1"), name = "NEGOTIATION_STATUS_CODE", referencedColumnName = "NEGOTIATION_STATUS_CODE", insertable = false, updatable = false)
	private NegotiationsStatus negotiationsStatus;

	@Column(name = "WORKFLOW_STATUS_CODE")
	private String workflowStatusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_FK2"), name = "WORKFLOW_STATUS_CODE", referencedColumnName = "WORKFLOW_STATUS_CODE", insertable = false, updatable = false)
	private NegotiationsWorkflowStatus negotiationsWorkflowStatus;

	@Column(name = "AGREEMENT_TYPE_CODE")
	private String agreementTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_FK3"), name = "AGREEMENT_TYPE_CODE", referencedColumnName = "AGREEMENT_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsAgreementType negotiationsAgreementType;

	@Column(name = "ASSOCIATED_PROJECT_ID")
	private String associatedProjectId;

	@Column(name = "NEGOTIATOR_PERSON_ID")
	private String negotiatorPersonId;

	@Column(name = "NEGOTIATOR_FULL_NAME")
	private String negotiatorFullName;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "FINAL_CONTRACT_DOCUMENT")
	private Blob finalContractDocument;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@Column(name = "SUMMARY_COMMENT")
	private String summaryComment;

	@Column(name = "NEGOTIATOR_COMMENT")
	private String negotiatorComment;

	@Column(name = "LEGAL_COMMENT")
	private String legalComment;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "TOTAL_BUDGET_AMOUNT", precision = 10, scale = 2)
	private BigDecimal totalBudgetAmount = BigDecimal.ZERO;

	@Transient
	private String associationsTypeCode;

	@Transient
	private String acType;

	@Transient
	private String sponsor;

	@Transient
	private String negotiationStatus;

	@Transient
	private List<NegotiationProjectDetailsDto> projectDetails;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NegotiationsPersonnel> negotiationsPersonnels;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NegotiationsAgreementValue> negotiationsAgreementValues;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NegotiationsActivity> negotiationsActivities;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NegotiationsAssociationDetails> negotiationsAssociationDetails;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NegotiationsAssociation> negotiationsAssociations;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.REMOVE,
			CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<NegotiationsAttachment> negotiationsAttachments;
//	@OneToMany(mappedBy = "negotiations", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)

	@Transient
	private String createUserFullName;

	@Transient
	private String updateUserFullName;

	public Negotiations() {
		negotiationsPersonnels = new ArrayList<NegotiationsPersonnel>();
		negotiationsAgreementValues = new ArrayList<NegotiationsAgreementValue>();
		negotiationsActivities = new ArrayList<NegotiationsActivity>();
		negotiationsAssociationDetails = new ArrayList<NegotiationsAssociationDetails>();
		negotiationsAssociations = new ArrayList<NegotiationsAssociation>();
		negotiationsAttachments = new ArrayList<NegotiationsAttachment>();
		projectDetails = new ArrayList<NegotiationProjectDetailsDto>();

	}

	public String getAssociatedProjectId() {
		return associatedProjectId;
	}

	public void setAssociatedProjectId(String associatedProjectId) {
		this.associatedProjectId = associatedProjectId;
	}

	public String getNegotiatorPersonId() {
		return negotiatorPersonId;
	}

	public void setNegotiatorPersonId(String negotiatorPersonId) {
		this.negotiatorPersonId = negotiatorPersonId;
	}

	public String getNegotiatorFullName() {
		return negotiatorFullName;
	}

	public void setNegotiatorFullName(String negotiatorFullName) {
		this.negotiatorFullName = negotiatorFullName;
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

	public Blob getFinalContractDocument() {
		return finalContractDocument;
	}

	public void setFinalContractDocument(Blob finalContractDocument) {
		this.finalContractDocument = finalContractDocument;
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

	public NegotiationsStatus getNegotiationsStatus() {
		return negotiationsStatus;
	}

	public void setNegotiationsStatus(NegotiationsStatus negotiationsStatus) {
		this.negotiationsStatus = negotiationsStatus;
	}

	public NegotiationsAgreementType getNegotiationsAgreementType() {
		return negotiationsAgreementType;
	}

	public void setNegotiationsAgreementType(NegotiationsAgreementType negotiationsAgreementType) {
		this.negotiationsAgreementType = negotiationsAgreementType;
	}

	public String getNegotiationStatusCode() {
		return negotiationStatusCode;
	}

	public void setNegotiationStatusCode(String negotiationStatusCode) {
		this.negotiationStatusCode = negotiationStatusCode;
	}

	public String getWorkflowStatusCode() {
		return workflowStatusCode;
	}

	public void setWorkflowStatusCode(String workflowStatusCode) {
		this.workflowStatusCode = workflowStatusCode;
	}

	public NegotiationsWorkflowStatus getNegotiationsWorkflowStatus() {
		return negotiationsWorkflowStatus;
	}

	public void setNegotiationsWorkflowStatus(NegotiationsWorkflowStatus negotiationsWorkflowStatus) {
		this.negotiationsWorkflowStatus = negotiationsWorkflowStatus;
	}

	public String getAgreementTypeCode() {
		return agreementTypeCode;
	}

	public void setAgreementTypeCode(String agreementTypeCode) {
		this.agreementTypeCode = agreementTypeCode;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public List<NegotiationsPersonnel> getNegotiationsPersonnels() {
		return negotiationsPersonnels;
	}

	public void setNegotiationsPersonnels(List<NegotiationsPersonnel> negotiationsPersonnels) {
		this.negotiationsPersonnels = negotiationsPersonnels;
	}

	public List<NegotiationsActivity> getNegotiationsActivities() {
		return negotiationsActivities;
	}

	public void setNegotiationsActivities(List<NegotiationsActivity> negotiationsActivities) {
		this.negotiationsActivities = negotiationsActivities;
	}

	public List<NegotiationsAssociationDetails> getNegotiationsAssociationDetails() {
		return negotiationsAssociationDetails;
	}

	public void setNegotiationsAssociationDetails(List<NegotiationsAssociationDetails> negotiationsAssociationDetails) {
		this.negotiationsAssociationDetails = negotiationsAssociationDetails;
	}

	public List<NegotiationsAgreementValue> getNegotiationsAgreementValues() {
		return negotiationsAgreementValues;
	}

	public void setNegotiationsAgreementValues(List<NegotiationsAgreementValue> negotiationsAgreementValues) {
		this.negotiationsAgreementValues = negotiationsAgreementValues;
	}

	public List<NegotiationsAssociation> getNegotiationsAssociations() {
		return negotiationsAssociations;
	}

	public void setNegotiationsAssociations(List<NegotiationsAssociation> negotiationsAssociations) {
		this.negotiationsAssociations = negotiationsAssociations;
	}

	public List<NegotiationsAttachment> getNegotiationsAttachments() {
		return negotiationsAttachments;
	}

	public void setNegotiationsAttachments(List<NegotiationsAttachment> negotiationsAttachments) {
		this.negotiationsAttachments = negotiationsAttachments;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public String getSummaryComment() {
		return summaryComment;
	}

	public void setSummaryComment(String summaryComment) {
		this.summaryComment = summaryComment;
	}

	public String getNegotiatorComment() {
		return negotiatorComment;
	}

	public void setNegotiatorComment(String negotiatorComment) {
		this.negotiatorComment = negotiatorComment;
	}

	public String getLegalComment() {
		return legalComment;
	}

	public void setLegalComment(String legalComment) {
		this.legalComment = legalComment;
	}

	public String getAssociationsTypeCode() {
		return associationsTypeCode;
	}

	public void setAssociationsTypeCode(String associationsTypeCode) {
		this.associationsTypeCode = associationsTypeCode;
	}

	public List<NegotiationProjectDetailsDto> getProjectDetails() {
		return projectDetails;
	}

	public void setProjectDetails(List<NegotiationProjectDetailsDto> projectDetails) {
		this.projectDetails = projectDetails;
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

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getSponsor() {
		return sponsor;
	}

	public void setSponsor(String sponsor) {
		this.sponsor = sponsor;
	}

	public String getNegotiationStatus() {
		return negotiationStatus;
	}

	public void setNegotiationStatus(String negotiationStatus) {
		this.negotiationStatus = negotiationStatus;
	}

	public BigDecimal getTotalBudgetAmount() {
		return totalBudgetAmount;
	}

	public void setTotalBudgetAmount(BigDecimal totalBudgetAmount) {
		this.totalBudgetAmount = totalBudgetAmount;
	}

}
