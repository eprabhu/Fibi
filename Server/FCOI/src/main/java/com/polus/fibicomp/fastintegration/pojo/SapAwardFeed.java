package com.polus.fibicomp.fastintegration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;

@Entity
@Table(name = "SAP_AWARD_FEED")
@EntityListeners(AuditingEntityListener.class)
public class SapAwardFeed {

	@Id
	@Column(name = "FEED_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer feedId;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@JsonIgnore
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_AWARD_FEED_FK1"), name = "BATCH_ID", referencedColumnName = "BATCH_ID", insertable = false, updatable = false)
	private SapAwardFeedBatch sapAwardFeedBatch;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "FEED_TYPE")
	private String feedType;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_AWARD_FEED_FK2"), name = "FEED_TYPE", referencedColumnName = "FEED_TYPE_CODE", insertable = false, updatable = false)
	private SapFeedType sapFeedType;

	@Column(name = "FEED_STATUS")
	private String feedStatus;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_AWARD_FEED_FK3"), name = "FEED_STATUS", referencedColumnName = "FEED_STATUS_CODE", insertable = false, updatable = false)
	private SapFeedStatus sapFeedStatus;

	@Column(name = "USER_ACTION_CODE")
	private String userActionCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SAP_AWARD_FEED_FK4"), name = "USER_ACTION_CODE", referencedColumnName = "USER_ACTION_CODE", insertable = false, updatable = false)
	private SapFeedUserAction sapFeedUserAction;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "USER_COMMENT")
	private String userComment;

	@Transient
	@Column(name = "GRANT_CODE")
	private String grantCode;

	@Transient
	private String piName;

	@Transient
	private Integer totalAwards;

	@Transient
	private Integer totalErrorAwards;

	@Transient
	private String updateUserFullName;
	
	@Transient
	private ServiceRequestType serviceRequestType; 

	@Transient
	private String awardDocumentType;

	@Transient
	private Timestamp responseTimestamp;

	@Transient
	private String accountNumber;

	@Transient
	private String feedStatusDesc;

	@Transient
	private String feedTypeDesc;

	@Transient
	private String userActionDesc;

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

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getGrantCode() {
		return grantCode;
	}

	public void setGrantCode(String grantCode) {
		this.grantCode = grantCode;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
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

	public Integer getTotalAwards() {
		return totalAwards;
	}

	public void setTotalAwards(Integer totalAwards) {
		this.totalAwards = totalAwards;
	}

	public Integer getTotalErrorAwards() {
		return totalErrorAwards;
	}

	public void setTotalErrorAwards(Integer totalErrorAwards) {
		this.totalErrorAwards = totalErrorAwards;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public SapAwardFeedBatch getSapAwardFeedBatch() {
		return sapAwardFeedBatch;
	}

	public void setSapAwardFeedBatch(SapAwardFeedBatch sapAwardFeedBatch) {
		this.sapAwardFeedBatch = sapAwardFeedBatch;
	}

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public Timestamp getResponseTimestamp() {
		return responseTimestamp;
	}

	public void setResponseTimestamp(Timestamp responseTimestamp) {
		this.responseTimestamp = responseTimestamp;
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

	public String getAwardDocumentType() {
		return awardDocumentType;
	}

	public void setAwardDocumentType(String awardDocumentType) {
		this.awardDocumentType = awardDocumentType;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getFeedStatusDesc() {
		return feedStatusDesc;
	}

	public void setFeedStatusDesc(String feedStatusDesc) {
		this.feedStatusDesc = feedStatusDesc;
	}

	public String getFeedTypeDesc() {
		return feedTypeDesc;
	}

	public void setFeedTypeDesc(String feedTypeDesc) {
		this.feedTypeDesc = feedTypeDesc;
	}

	public String getUserActionDesc() {
		return userActionDesc;
	}

	public void setUserActionDesc(String userActionDesc) {
		this.userActionDesc = userActionDesc;
	}

}
