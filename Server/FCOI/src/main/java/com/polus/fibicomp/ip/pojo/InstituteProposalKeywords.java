package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.ScienceKeyword;

@Entity
@Table(name = "PROPOSAL_KEYWORDS")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalKeywords implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KEYWORD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_KEYWORD_ID_GNTR")
	@SequenceGenerator(name="SEQ_IP_KEYWORD_ID_GNTR", sequenceName = "SEQ_IP_KEYWORD_ID_GNTR", allocationSize=1)
	private Integer keywordId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROP_KEYWORDS_FK1"), name = "PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID")
	private InstituteProposal instProposal;

	@Column(name = "SCIENCE_KEYWORD_CODE")
	private String scienceKeywordCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROP_KEYWORDS_FK2"), name = "SCIENCE_KEYWORD_CODE", referencedColumnName = "SCIENCE_KEYWORD_CODE", insertable = false, updatable = false)
	private ScienceKeyword scienceKeyword;

	@Column(name = "KEYWORD")
	private String keyword;

	@Column(name = "PROPOSAL_NUMBER")
	private String proposalNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getKeywordId() {
		return keywordId;
	}

	public void setKeywordId(Integer keywordId) {
		this.keywordId = keywordId;
	}

	public String getScienceKeywordCode() {
		return scienceKeywordCode;
	}

	public void setScienceKeywordCode(String scienceKeywordCode) {
		this.scienceKeywordCode = scienceKeywordCode;
	}

	public ScienceKeyword getScienceKeyword() {
		return scienceKeyword;
	}

	public void setScienceKeyword(ScienceKeyword scienceKeyword) {
		this.scienceKeyword = scienceKeyword;
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

	public String getKeyword() {
		return keyword;
	}

	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}

	public InstituteProposal getInstProposal() {
		return instProposal;
	}

	public void setInstProposal(InstituteProposal instProposal) {
		this.instProposal = instProposal;
	}

}
