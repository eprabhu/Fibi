package com.polus.fibicomp.award.pojo;

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
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_SPECIAL_REVIEW")
@EntityListeners(AuditingEntityListener.class)
public class AwardSpecialReview implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_SPECIAL_REVIEW_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardSpecailReviewId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SPECIAL_REVIEW_CODE")
	private String specialReviewCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_SPECIAL_REVIEW_FK2"), name = "SPECIAL_REVIEW_CODE", referencedColumnName = "SPECIAL_REVIEW_CODE", insertable = false, updatable = false)
	private SpecialReviewType specialReview;

	@Column(name = "APPROVAL_TYPE_CODE")
	private String approvalTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_SPECIAL_REVIEW_FK3"), name = "APPROVAL_TYPE_CODE", referencedColumnName = "APPROVAL_TYPE_CODE", insertable = false, updatable = false)
	private SpecialReviewApprovalType specialReviewApprovalType;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@Column(name = "PROTOCOL_NUMBER")
	private String protocolNumber;

	@Column(name = "APPLICATION_DATE")
	private Timestamp applicationDate;

	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;

	@Lob
	@Column(name = "COMMENTS")
	private String comments;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "IS_INTEGRATED_PROTOCOL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isProtocolIntegrated = Boolean.FALSE;

	@Transient
	private IrbProtocol irbProtocol;

	@Transient
	private AcProtocol acProtocol;

	public Integer getAwardSpecailReviewId() {
		return awardSpecailReviewId;
	}

	public void setAwardSpecailReviewId(Integer awardSpecailReviewId) {
		this.awardSpecailReviewId = awardSpecailReviewId;
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

	public String getSpecialReviewCode() {
		return specialReviewCode;
	}

	public void setSpecialReviewCode(String specialReviewCode) {
		this.specialReviewCode = specialReviewCode;
	}

	public SpecialReviewType getSpecialReview() {
		return specialReview;
	}

	public void setSpecialReview(SpecialReviewType specialReview) {
		this.specialReview = specialReview;
	}

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getProtocolNumber() {
		return protocolNumber;
	}

	public void setProtocolNumber(String protocolNumber) {
		this.protocolNumber = protocolNumber;
	}

	public Timestamp getApplicationDate() {
		return applicationDate;
	}

	public void setApplicationDate(Timestamp applicationDate) {
		this.applicationDate = applicationDate;
	}

	public Timestamp getApprovalDate() {
		return approvalDate;
	}

	public void setApprovalDate(Timestamp approvalDate) {
		this.approvalDate = approvalDate;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
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

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getApprovalTypeCode() {
		return approvalTypeCode;
	}

	public void setApprovalTypeCode(String approvalTypeCode) {
		this.approvalTypeCode = approvalTypeCode;
	}

	public SpecialReviewApprovalType getSpecialReviewApprovalType() {
		return specialReviewApprovalType;
	}

	public void setSpecialReviewApprovalType(SpecialReviewApprovalType specialReviewApprovalType) {
		this.specialReviewApprovalType = specialReviewApprovalType;
	}

	public Boolean getIsProtocolIntegrated() {
		return isProtocolIntegrated;
	}

	public void setIsProtocolIntegrated(Boolean isProtocolIntegrated) {
		this.isProtocolIntegrated = isProtocolIntegrated;
	}

	public IrbProtocol getIrbProtocol() {
		return irbProtocol;
	}

	public void setIrbProtocol(IrbProtocol irbProtocol) {
		this.irbProtocol = irbProtocol;
	}

	public AcProtocol getAcProtocol() {
		return acProtocol;
	}

	public void setAcProtocol(AcProtocol acProtocol) {
		this.acProtocol = acProtocol;
	}

}
