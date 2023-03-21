package com.polus.fibicomp.award.pojo;

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
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_COST_SHARE")
public class AwardCostShare implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_COST_SHARE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardCostShareId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

/*	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_COST_SHARE_FK1"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;*/

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PROJECT_PERIOD")
	private String projectPeriod;

	@Column(name = "COST_SHARE_PERCENTAGE")
	private Double costSharePercentage;

	@Column(name = "COST_SHARE_TYPE_CODE")
	private Integer costShareTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_COST_SHARE_FK2"), name = "COST_SHARE_TYPE_CODE", referencedColumnName = "COST_SHARE_TYPE_CODE", insertable = false, updatable = false)
	private CostShareType costShareType;

	@Column(name = "SOURCE")
	private String source;

	@Column(name = "DESTINATION")
	private String destination;

	@Column(name = "COMMITMENT_AMOUNT")
	private BigDecimal commitmentAmount = BigDecimal.ZERO;

	@Column(name = "VERIFICATION_DATE")
	private Timestamp verificationDate;

	@Column(name = "COST_SHARE_MET")
	private BigDecimal costShareMet = BigDecimal.ZERO;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "COST_SHARE_DISTRIBUTABLE")
	private BigDecimal costShareDistributable = BigDecimal.ZERO;

	public Integer getAwardCostShareId() {
		return awardCostShareId;
	}

	public void setAwardCostShareId(Integer awardCostShareId) {
		this.awardCostShareId = awardCostShareId;
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

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getProjectPeriod() {
		return projectPeriod;
	}

	public void setProjectPeriod(String projectPeriod) {
		this.projectPeriod = projectPeriod;
	}

	public Double getCostSharePercentage() {
		return costSharePercentage;
	}

	public void setCostSharePercentage(Double costSharePercentage) {
		this.costSharePercentage = costSharePercentage;
	}

	public Integer getCostShareTypeCode() {
		return costShareTypeCode;
	}

	public void setCostShareTypeCode(Integer costShareTypeCode) {
		this.costShareTypeCode = costShareTypeCode;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
	}

	public Timestamp getVerificationDate() {
		return verificationDate;
	}

	public void setVerificationDate(Timestamp verificationDate) {
		this.verificationDate = verificationDate;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public BigDecimal getCostShareDistributable() {
		return costShareDistributable;
	}

	public void setCostShareDistributable(BigDecimal costShareDistributable) {
		this.costShareDistributable = costShareDistributable;
	}

	public BigDecimal getCommitmentAmount() {
		return commitmentAmount;
	}

	public void setCommitmentAmount(BigDecimal commitmentAmount) {
		this.commitmentAmount = commitmentAmount;
	}

	public BigDecimal getCostShareMet() {
		return costShareMet;
	}

	public void setCostShareMet(BigDecimal costShareMet) {
		this.costShareMet = costShareMet;
	}

	public CostShareType getCostShareType() {
		return costShareType;
	}

	public void setCostShareType(CostShareType costShareType) {
		this.costShareType = costShareType;
	}

}
