package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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
@Table(name = "PROPOSAL_SPECIAL_REVIEW")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalSpecialReview implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_SPECIAL_REVIEW_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_SPECIAL_REVIEW_ID_GNTR")
	@SequenceGenerator(name="SEQ_IP_SPECIAL_REVIEW_ID_GNTR", sequenceName = "SEQ_IP_SPECIAL_REVIEW_ID_GNTR", allocationSize=1)
	private Integer specialReviewId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "PROPOSAL_NUMBER")
	private String proposalNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "APPLICATION_DATE")
	private Timestamp applicationDate;

	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;

	@Column(name = "APPROVAL_TYPE_CODE")
	private String approvalTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROP_SPL_REVIEW_FK3"), name = "APPROVAL_TYPE_CODE", referencedColumnName = "APPROVAL_TYPE_CODE", insertable = false, updatable = false)
	private SpecialReviewApprovalType specialReviewApprovalType;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@Column(name = "PROTOCOL_NUMBER")
	private String protocolNumber;

	@Column(name = "PROTOCOL_STATUS_DESCRIPTION")
	private String statusDescription;

	@Column(name = "SPECIAL_REVIEW_CODE")
	private String specialReviewCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROP_SPL_REVIEW_FK2"), name = "SPECIAL_REVIEW_CODE", referencedColumnName = "SPECIAL_REVIEW_CODE", insertable = false, updatable = false)
	private SpecialReviewType specialReviewType;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_INTEGRATED_PROTOCOL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isProtocolIntegrated = Boolean.FALSE;

	@Transient
	private IrbProtocol irbProtocol;

	@Transient
	private AcProtocol acProtocol;

	public Integer getSpecialReviewId() {
		return specialReviewId;
	}

	public void setSpecialReviewId(Integer specialReviewId) {
		this.specialReviewId = specialReviewId;
	}

	public String getProposalNumber() {
		return proposalNumber;
	}

	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
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

	public String getApprovalTypeCode() {
		return approvalTypeCode;
	}

	public void setApprovalTypeCode(String approvalTypeCode) {
		this.approvalTypeCode = approvalTypeCode;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getProtocolNumber() {
		return protocolNumber;
	}

	public void setProtocolNumber(String protocolNumber) {
		this.protocolNumber = protocolNumber;
	}

	public String getStatusDescription() {
		return statusDescription;
	}

	public void setStatusDescription(String statusDescription) {
		this.statusDescription = statusDescription;
	}

	public String getSpecialReviewCode() {
		return specialReviewCode;
	}

	public void setSpecialReviewCode(String specialReviewCode) {
		this.specialReviewCode = specialReviewCode;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public SpecialReviewApprovalType getSpecialReviewApprovalType() {
		return specialReviewApprovalType;
	}

	public void setSpecialReviewApprovalType(SpecialReviewApprovalType specialReviewApprovalType) {
		this.specialReviewApprovalType = specialReviewApprovalType;
	}

	public SpecialReviewType getSpecialReviewType() {
		return specialReviewType;
	}

	public void setSpecialReviewType(SpecialReviewType specialReviewType) {
		this.specialReviewType = specialReviewType;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
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
