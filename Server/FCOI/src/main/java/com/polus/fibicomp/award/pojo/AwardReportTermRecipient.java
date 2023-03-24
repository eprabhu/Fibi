package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
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
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "AWARD_REPORT_TERM_RECIPIENT")
public class AwardReportTermRecipient implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_REPORT_TERM_RECIPIENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardReportTermRecipientId;

//	@Column(name = "AWARD_REPORT_TERMS_ID")
//	private Integer awardReportTermsId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_REPORT_TRM_RECIPIENT_FK1"), name = "AWARD_REPORT_TERMS_ID", referencedColumnName = "AWARD_REPORT_TERMS_ID")
	private AwardReportTerms awardReportTerms;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "RECIPIENT_ID")
	private String recipientId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String acType;
	
	@Transient
	private String fullName;

	public Integer getAwardReportTermRecipientId() {
		return awardReportTermRecipientId;
	}

	public void setAwardReportTermRecipientId(Integer awardReportTermRecipientId) {
		this.awardReportTermRecipientId = awardReportTermRecipientId;
	}

//	public Integer getAwardReportTermsId() {
//		return awardReportTermsId;
//	}
//
//	public void setAwardReportTermsId(Integer awardReportTermsId) {
//		this.awardReportTermsId = awardReportTermsId;
//	}

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

	public String getRecipientId() {
		return recipientId;
	}

	public void setRecipientId(String recipientId) {
		this.recipientId = recipientId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public AwardReportTerms getAwardReportTerms() {
		return awardReportTerms;
	}

	public void setAwardReportTerms(AwardReportTerms awardReportTerms) {
		this.awardReportTerms = awardReportTerms;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

}
