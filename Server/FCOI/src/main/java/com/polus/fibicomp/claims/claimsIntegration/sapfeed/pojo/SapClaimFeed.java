package com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

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

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.fastintegration.pojo.SapFeedType;
import com.polus.fibicomp.fastintegration.pojo.SapFeedUserAction;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SAP_CLAIM_FEED")
@EntityListeners(AuditingEntityListener.class)
public class SapClaimFeed implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FEED_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_CLAIM_FEED_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_CLAIM_FEED_ID_GENERATOR", sequenceName = "SAP_CLAIM_FEED_ID_GENERATOR", allocationSize = 1)
	private Integer feedId;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@JsonIgnore
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_CLAIM_FEED_FK1"), name = "BATCH_ID", referencedColumnName = "BATCH_ID", insertable = false, updatable = false)
	private SapClaimFeedBatch sapClaimFeedBatch;

	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;
	
	@Column(name = "INVOICE_ID")
	private Integer invoiceId;
	
	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "FEED_TYPE_CODE")
	private String feedType;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_CLAIM_FEED_FK2"), name = "FEED_TYPE_CODE", referencedColumnName = "FEED_TYPE_CODE", insertable = false, updatable = false)
	private SapFeedType sapFeedType;

	@Column(name = "FEED_STATUS_CODE")
	private String feedStatus;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_CLAIM_FEED_FK3"), name = "FEED_STATUS_CODE", referencedColumnName = "FEED_STATUS_CODE", insertable = false, updatable = false)
	private SapFeedStatus sapFeedStatus;

	@Column(name = "USER_ACTION_CODE")
	private String userActionCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_CLAIM_FEED_FK4"), name = "USER_ACTION_CODE", referencedColumnName = "USER_ACTION_CODE", insertable = false, updatable = false)
	private SapFeedUserAction sapFeedUserAction;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "USER_COMMENT")
	private String userComment;
	
	@Column(name = "BUSINESS_AREA")
	private String businessArea;
	
	@Column(name = "NO_FEED_REPORT_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean noFeedFlag;

	public Integer getFeedId() {
		return feedId;
	}

	public void setFeedId(Integer feedId) {
		this.feedId = feedId;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public String getFeedType() {
		return feedType;
	}

	public void setFeedType(String feedType) {
		this.feedType = feedType;
	}

	public String getFeedStatus() {
		return feedStatus;
	}

	public void setFeedStatus(String feedStatus) {
		this.feedStatus = feedStatus;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
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

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}

	public SapFeedStatus getSapFeedStatus() {
		return sapFeedStatus;
	}

	public void setSapFeedStatus(SapFeedStatus sapFeedStatus) {
		this.sapFeedStatus = sapFeedStatus;
	}

	public SapFeedType getSapFeedType() {
		return sapFeedType;
	}

	public void setSapFeedType(SapFeedType sapFeedType) {
		this.sapFeedType = sapFeedType;
	}

	public String getUserActionCode() {
		return userActionCode;
	}

	public void setUserActionCode(String userActionCode) {
		this.userActionCode = userActionCode;
	}

	public SapFeedUserAction getSapFeedUserAction() {
		return sapFeedUserAction;
	}

	public void setSapFeedUserAction(SapFeedUserAction sapFeedUserAction) {
		this.sapFeedUserAction = sapFeedUserAction;
	}


	public SapClaimFeedBatch getSapClaimFeedBatch() {
		return sapClaimFeedBatch;
	}

	public void setSapClaimFeedBatch(SapClaimFeedBatch sapClaimFeedBatch) {
		this.sapClaimFeedBatch = sapClaimFeedBatch;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public Integer getInvoiceId() {
		return invoiceId;
	}

	public void setInvoiceId(Integer invoiceId) {
		this.invoiceId = invoiceId;
	}

	public String getBusinessArea() {
		return businessArea;
	}

	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

	public Boolean getNoFeedFlag() {
		return noFeedFlag;
	}

	public void setNoFeedFlag(Boolean noFeedFlag) {
		this.noFeedFlag = noFeedFlag;
	}

}
