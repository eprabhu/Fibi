package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.Unit;

@Entity
@Table(name = "AGREEMENT_HEADER")
public class AgreementHeader implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_HEADER")
	@SequenceGenerator(name="SEQ_AGREEMENT_HEADER", sequenceName = "SEQ_AGREEMENT_HEADER", allocationSize=1)
	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@Column(name = "AGREEMENT_STATUS_CODE")
	private String agreementStatusCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK1"), name = "AGREEMENT_STATUS_CODE", referencedColumnName = "AGREEMENT_STATUS_CODE", insertable = false, updatable = false)
	private AgreementStatus agreementStatus;

	@Column(name = "AGREEMENT_TYPE_CODE")
	private String agreementTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK2"), name = "AGREEMENT_TYPE_CODE", referencedColumnName = "AGREEMENT_TYPE_CODE", insertable = false, updatable = false)
	private AgreementType agreementType;

	@Column(name = "CATEGORY_CODE")
	private String categoryCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK3"), name = "CATEGORY_CODE", referencedColumnName = "CATEGORY_CODE", insertable = false, updatable = false)
	private AgreementCategory agreementCategory;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "CONTRACT_VALUE")
	private BigDecimal contractValue = BigDecimal.ZERO;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "REMARKS")
	private String remarks;

	@Column(name = "REQUESTOR_PERSON_ID")
	private String requestorPersonId;

	@Column(name = "REQUESTOR_NAME")
	private String requestorName;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK5"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "CURRENCY_CODE")
	private String currencyCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK4"), name = "CURRENCY_CODE", referencedColumnName = "CURRENCY_CODE", insertable = false, updatable = false)
	private Currency currency;

	@Column(name = "AMOUNT_IN_WORDS")
	private String amountInWords;

	@Column(name = "WORKFLOW_STATUS_CODE")
	private String workflowStatusCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK7"), name = "WORKFLOW_STATUS_CODE", referencedColumnName = "WORKFLOW_STATUS_CODE", insertable = false, updatable = false)
	private AgreementWorkflowStatus agreementWorkflowStatus;

	@Column(name = "SUBMIT_USER")
	private String submitUser;

	@Column(name = "AGREEMENT_SEQUENCE_STATUS")
	private String agreementSequenceStatus = "ACTIVE";

	@Column(name = "ADMIN_PERSON_ID")
	private String adminPersonId;

	@Column(name = "ADMIN_PERSON_NAME")
	private String adminName;

	@Transient
	private String createUserFullName;

	@Transient
	private String submitUserFullName;

	@Transient
	private String updateUserFullName;

	@Transient
	private Integer negotiationId;

	@Transient
	private String requestorEmail;

	@Transient
	private String requestorPhoneNumber;

	@Transient
	private String requestorDepartment;

	@Transient
	private String organization;

	@Transient
	private String negotiatorFullName;

	@Transient
	private String piFullName;

	@Transient
	private String catFullName;

	@Transient
	private String stoFullName;

	@Transient
	private String caFullName;

	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_HEADER_FK8"), name = "ADMIN_GROUP_ID", referencedColumnName = "ADMIN_GROUP_ID", insertable = false, updatable = false)
	private AdminGroup agreementAdminGroup;

	@Transient
	private String statusDescription;

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public String getAgreementStatusCode() {
		return agreementStatusCode;
	}

	public void setAgreementStatusCode(String agreementStatusCode) {
		this.agreementStatusCode = agreementStatusCode;
	}

	public AgreementStatus getAgreementStatus() {
		return agreementStatus;
	}

	public void setAgreementStatus(AgreementStatus agreementStatus) {
		this.agreementStatus = agreementStatus;
	}

	public String getAgreementTypeCode() {
		return agreementTypeCode;
	}

	public void setAgreementTypeCode(String agreementTypeCode) {
		this.agreementTypeCode = agreementTypeCode;
	}

	public AgreementType getAgreementType() {
		return agreementType;
	}

	public void setAgreementType(AgreementType agreementType) {
		this.agreementType = agreementType;
	}

	public String getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(String categoryCode) {
		this.categoryCode = categoryCode;
	}

	public AgreementCategory getAgreementCategory() {
		return agreementCategory;
	}

	public void setAgreementCategory(AgreementCategory agreementCategory) {
		this.agreementCategory = agreementCategory;
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

	public BigDecimal getContractValue() {
		return contractValue;
	}

	public void setContractValue(BigDecimal contractValue) {
		this.contractValue = contractValue;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public String getRequestorPersonId() {
		return requestorPersonId;
	}

	public void setRequestorPersonId(String requestorPersonId) {
		this.requestorPersonId = requestorPersonId;
	}

	public String getRequestorName() {
		return requestorName;
	}

	public void setRequestorName(String requestorName) {
		this.requestorName = requestorName;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
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

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
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

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public String getAmountInWords() {
		return amountInWords;
	}

	public void setAmountInWords(String amountInWords) {
		this.amountInWords = amountInWords;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public String getRequestorEmail() {
		return requestorEmail;
	}

	public void setRequestorEmail(String requestorEmail) {
		this.requestorEmail = requestorEmail;
	}

	public String getRequestorPhoneNumber() {
		return requestorPhoneNumber;
	}

	public void setRequestorPhoneNumber(String requestorPhoneNumber) {
		this.requestorPhoneNumber = requestorPhoneNumber;
	}

	public String getRequestorDepartment() {
		return requestorDepartment;
	}

	public void setRequestorDepartment(String requestorDepartment) {
		this.requestorDepartment = requestorDepartment;
	}

	public String getWorkflowStatusCode() {
		return workflowStatusCode;
	}

	public void setWorkflowStatusCode(String workflowStatusCode) {
		this.workflowStatusCode = workflowStatusCode;
	}

	public AgreementWorkflowStatus getAgreementWorkflowStatus() {
		return agreementWorkflowStatus;
	}

	public void setAgreementWorkflowStatus(AgreementWorkflowStatus agreementWorkflowStatus) {
		this.agreementWorkflowStatus = agreementWorkflowStatus;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getOrganization() {
		return organization;
	}

	public void setOrganization(String organization) {
		this.organization = organization;
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

	public String getNegotiatorFullName() {
		return negotiatorFullName;
	}

	public void setNegotiatorFullName(String negotiatorFullName) {
		this.negotiatorFullName = negotiatorFullName;
	}

	public String getPiFullName() {
		return piFullName;
	}

	public void setPiFullName(String piFullName) {
		this.piFullName = piFullName;
	}

	public String getCatFullName() {
		return catFullName;
	}

	public void setCatFullName(String catFullName) {
		this.catFullName = catFullName;
	}

	public String getStoFullName() {
		return stoFullName;
	}

	public void setStoFullName(String stoFullName) {
		this.stoFullName = stoFullName;
	}

	public String getCaFullName() {
		return caFullName;
	}

	public void setCaFullName(String caFullName) {
		this.caFullName = caFullName;
	}

	public String getAgreementSequenceStatus() {
		return agreementSequenceStatus;
	}

	public void setAgreementSequenceStatus(String agreementSequenceStatus) {
		this.agreementSequenceStatus = agreementSequenceStatus;
	}

	public String getAdminPersonId() {
		return adminPersonId;
	}

	public void setAdminPersonId(String adminPersonId) {
		this.adminPersonId = adminPersonId;
	}

	public String getAdminName() {
		return adminName;
	}

	public void setAdminName(String adminName) {
		this.adminName = adminName;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public AdminGroup getAgreementAdminGroup() {
		return agreementAdminGroup;
	}

	public void setAgreementAdminGroup(AdminGroup agreementAdminGroup) {
		this.agreementAdminGroup = agreementAdminGroup;
	}

	public String getStatusDescription() {
		return statusDescription;
	}

	public void setStatusDescription(String statusDescription) {
		this.statusDescription = statusDescription;
	}

}
