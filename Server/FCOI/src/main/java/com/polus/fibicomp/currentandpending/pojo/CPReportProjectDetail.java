package com.polus.fibicomp.currentandpending.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CP_REPORT_PROJECT_DETAILS")
public class CPReportProjectDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CP_REPORT_PROJECT_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "CP_REPORT_PROJECT_DETAIL_ID_GENERATOR")
	@SequenceGenerator(name = "CP_REPORT_PROJECT_DETAIL_ID_GENERATOR", sequenceName = "CP_REPORT_PROJECT_DETAIL_ID_GENERATOR", allocationSize = 1)
	private Integer cpReportProjectDetailId;

	@JsonBackReference
	@OneToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "CP_REPORT_PROJECT_DETAILS_FK"), name = "CP_REPORT_HEADER_ID", referencedColumnName = "CP_REPORT_HEADER_ID")
	private CPReportHeader cpReportHeader;

	@Column(name = "LINKED_MODULE_CODE")
	private Integer linkedModuleCode;

	@Column(name = "LINKED_MODULE_ITEM_ID")
	private String linkedModuleItemId;

	@Column(name = "IS_EXCLUDED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isExcluded;

	@Column(name = "IS_MANUALLY_ADDED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isManuallyAdded;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "PROJECT_TITLE")
	private String title;

	@Column(name = "GRANT_CALL_NAME")
	private String grantCallName;

	@Column(name = "PROP_PERSON_ROLE_ID")
	private Integer personRoleId;

	@Column(name = "AMOUNT", precision = 12, scale = 2)
	private BigDecimal totalAwardAmount = BigDecimal.ZERO;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "SPONSOR")
	private String sponsorName;

	@Column(name = "SPONSOR_TYPE_CODE")
	private String sponsorTypeCode;

	@Column(name = "FUNDING_STATUS_CODE")
	private Integer fundingStatusCode;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@Column(name = "CURRENCY_CODE")
	private String currencyCode;

	@Column(name = "PERCENTAGE_OF_EFFORT")
	private BigDecimal percentageEffort;

	@Transient
	private SponsorType sponsorType;

	@Transient
	private String moduleStatus;

	@Transient
	private Sponsor sponsor;

	@Transient
	private ProposalPersonRole proposalPersonRole;

	@Transient
	private Currency currency;

	public Integer getCpReportProjectDetailId() {
		return cpReportProjectDetailId;
	}

	public void setCpReportProjectDetailId(Integer cpReportProjectDetailId) {
		this.cpReportProjectDetailId = cpReportProjectDetailId;
	}

	public CPReportHeader getCpReportHeader() {
		return cpReportHeader;
	}

	public void setCpReportHeader(CPReportHeader cpReportHeader) {
		this.cpReportHeader = cpReportHeader;
	}

	public Integer getLinkedModuleCode() {
		return linkedModuleCode;
	}

	public void setLinkedModuleCode(Integer linkedModuleCode) {
		this.linkedModuleCode = linkedModuleCode;
	}

	public String getLinkedModuleItemId() {
		return linkedModuleItemId;
	}

	public void setLinkedModuleItemId(String linkedModuleItemId) {
		this.linkedModuleItemId = linkedModuleItemId;
	}

	public Boolean getIsExcluded() {
		return isExcluded;
	}

	public void setIsExcluded(Boolean isExcluded) {
		this.isExcluded = isExcluded;
	}

	public Boolean getIsManuallyAdded() {
		return isManuallyAdded;
	}

	public void setIsManuallyAdded(Boolean isManuallyAdded) {
		this.isManuallyAdded = isManuallyAdded;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public BigDecimal getTotalAwardAmount() {
		return totalAwardAmount;
	}

	public void setTotalAwardAmount(BigDecimal totalAwardAmount) {
		this.totalAwardAmount = totalAwardAmount;
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

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public Integer getFundingStatusCode() {
		return fundingStatusCode;
	}

	public void setFundingStatusCode(Integer fundingStatusCode) {
		this.fundingStatusCode = fundingStatusCode;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public BigDecimal getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(BigDecimal percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public SponsorType getSponsorType() {
		return sponsorType;
	}

	public void setSponsorType(SponsorType sponsorType) {
		this.sponsorType = sponsorType;
	}

	public String getModuleStatus() {
		return moduleStatus;
	}

	public void setModuleStatus(String moduleStatus) {
		this.moduleStatus = moduleStatus;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public Currency getCurrency() {
		return currency;
	}

	public void setCurrency(Currency currency) {
		this.currency = currency;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

}
