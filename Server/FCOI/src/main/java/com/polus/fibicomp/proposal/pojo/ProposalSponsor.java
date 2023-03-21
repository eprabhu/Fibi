package com.polus.fibicomp.proposal.pojo;

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

import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorType;

@Entity
@Table(name = "EPS_PROPOSAL_SPONSORS")
public class ProposalSponsor implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SPONSOR_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_SPONSOR_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_SPONSOR_ID_GENERATOR", sequenceName = "EPS_PROP_SPONSOR_ID_GENERATOR", allocationSize=1)
	private Integer sponsorId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_SPONSORS_FK2"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "AMOUNT", precision = 12, scale = 2)
	private BigDecimal amount = BigDecimal.ZERO;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "FUNDING_STATUS_CODE")
	private Integer fundingStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_SPONSORS_FK3"), name = "FUNDING_STATUS_CODE", referencedColumnName = "FUNDING_STATUS_CODE", insertable = false, updatable = false)
	private ProposalFundingStatus proposalFundingStatus;

	@Column(name = "SPONSOR_TYPE_CODE")
	private String sponsorTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_SPONSORS_FK4"), name = "SPONSOR_TYPE_CODE", referencedColumnName = "SPONSOR_TYPE_CODE", insertable = false, updatable = false)
	private SponsorType sponsorType;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "SPONSOR_NAME")
	private String sponsorName;

	@Column(name = "PROP_PERSON_ROLE_ID")
	private Integer personRoleId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_SPONSORS_FK5"), name = "PROP_PERSON_ROLE_ID", referencedColumnName = "PROP_PERSON_ROLE_ID", insertable = false, updatable = false)
	private ProposalPersonRole proposalPersonRole;

	@Column(name = "PROJECT_TITLE")
	private String projectTitle;

	@Column(name = "PERCENTAGE_OF_EFFORT")
	private BigDecimal percentageOfEffort;

	@Column(name = "GRANT_CALL_NAME")
	private String grantCallName;

	@Column(name = "CURRENCY_CODE")
	private String currencyCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_SPONSORS_FK6"), name = "CURRENCY_CODE", referencedColumnName = "CURRENCY_CODE", insertable = false, updatable = false)
	private Currency currency;

	public Integer getSponsorId() {
		return sponsorId;
	}

	public void setSponsorId(Integer sponsorId) {
		this.sponsorId = sponsorId;
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

	public Integer getFundingStatusCode() {
		return fundingStatusCode;
	}

	public void setFundingStatusCode(Integer fundingStatusCode) {
		this.fundingStatusCode = fundingStatusCode;
	}

	public ProposalFundingStatus getProposalFundingStatus() {
		return proposalFundingStatus;
	}

	public void setProposalFundingStatus(ProposalFundingStatus proposalFundingStatus) {
		this.proposalFundingStatus = proposalFundingStatus;
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

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getProjectTitle() {
		return projectTitle;
	}

	public void setProjectTitle(String projectTitle) {
		this.projectTitle = projectTitle;
	}

	public BigDecimal getPercentageOfEffort() {
		return percentageOfEffort;
	}

	public void setPercentageOfEffort(BigDecimal percentageOfEffort) {
		this.percentageOfEffort = percentageOfEffort;
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

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
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

	public BigDecimal getAmount() {
		return amount;
	}

	public void setAmount(BigDecimal amount) {
		this.amount = amount;
	}

}
