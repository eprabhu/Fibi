package com.polus.fibicomp.compilance.pojo;

import java.io.Serializable;
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
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EPS_PROPOSAL_SPECIAL_REVIEW")
public class ProposalSpecialReview implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROPOSAL_SPECIAL_REVIEW_ID_GENERATOR")
	@SequenceGenerator(name = "EPS_PROPOSAL_SPECIAL_REVIEW_ID_GENERATOR", sequenceName = "EPS_PROPOSAL_SPECIAL_REVIEW_ID_GENERATOR", allocationSize = 1)
	@Column(name = "PROPOSAL_SPECIAL_REVIEW_ID")
	private Integer id;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "SPECIAL_REVIEW_CODE", length = 3)
	private String specialReviewTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_SPL_REVIEW_FK2"), name = "SPECIAL_REVIEW_CODE", referencedColumnName = "SPECIAL_REVIEW_CODE", insertable = false, updatable = false)
	private SpecialReviewType specialReviewType;

	@Column(name = "APPROVAL_TYPE_CODE", length = 3)
	private String approvalTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_SPL_REVIEW_FK3"), name = "APPROVAL_TYPE_CODE", referencedColumnName = "APPROVAL_TYPE_CODE", insertable = false, updatable = false)
	private SpecialReviewApprovalType approvalType;

	@Column(name = "PROTOCOL_STATUS_DESCRIPTION")
	private String protocolStatus;

	@Column(name = "PROTOCOL_NUMBER")
	private String protocolNumber;

	@Column(name = "APPLICATION_DATE")
	private Timestamp applicationDate;

	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@Column(name = "COMMENTS")
	//@Lob
	private String comments;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_INTEGRATED_PROTOCOL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isProtocolIntegrated = Boolean.FALSE;

	@Transient
	private IrbProtocol irbProtocol;

	@Transient
	private AcProtocol acProtocol;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getSpecialReviewTypeCode() {
		return specialReviewTypeCode;
	}

	public void setSpecialReviewTypeCode(String specialReviewTypeCode) {
		this.specialReviewTypeCode = specialReviewTypeCode;
	}

	public String getApprovalTypeCode() {
		return approvalTypeCode;
	}

	public void setApprovalTypeCode(String approvalTypeCode) {
		this.approvalTypeCode = approvalTypeCode;
	}

	public String getProtocolStatus() {
		return protocolStatus;
	}

	public void setProtocolStatus(String protocolStatus) {
		this.protocolStatus = protocolStatus;
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

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public SpecialReviewType getSpecialReviewType() {
		return specialReviewType;
	}

	public void setSpecialReviewType(SpecialReviewType specialReviewType) {
		this.specialReviewType = specialReviewType;
	}

	public SpecialReviewApprovalType getApprovalType() {
		return approvalType;
	}

	public void setApprovalType(SpecialReviewApprovalType approvalType) {
		this.approvalType = approvalType;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
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

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
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
