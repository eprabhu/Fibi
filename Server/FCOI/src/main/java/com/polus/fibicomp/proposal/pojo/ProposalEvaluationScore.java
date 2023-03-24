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

@Entity
@Table(name = "EPS_PROP_EVALUATION_SCORE")
public class ProposalEvaluationScore implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EPS_PROP_EVALUATION_SCORE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_EVALUATION_SCORE_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_EVALUATION_SCORE_ID_GENERATOR", sequenceName = "EPS_PROP_EVALUATION_SCORE_ID_GENERATOR", allocationSize=1)
	private Integer proposalEvalutionScoreId;

	@Column(name = "ADJUSTED_SCORE")
	private BigDecimal adjustedScore;

	@Column(name = "IS_SHORTLISTED")
	private String isShortListed;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "GRANT_HEADER_ID") 
	private Integer grantHeaderId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_EVALUATION_SCORE_FK2"), name = "PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID", insertable = false, updatable = false)
	private Proposal proposal;

	@Column(name = "PROPOSAL_RANK")
	private String proposalRank;

	public Proposal getProposal() {
		return proposal;
	}

	public void setProposal(Proposal proposal) {
		this.proposal = proposal;
	}

	public Integer getProposalEvalutionScoreId() {
		return proposalEvalutionScoreId;
	}

	public void setProposalEvalutionScoreId(Integer proposalEvalutionScoreId) {
		this.proposalEvalutionScoreId = proposalEvalutionScoreId;
	}

	public Integer getGrantHeaderId() {
		return grantHeaderId;
	}

	public void setGrantHeaderId(Integer grantHeaderId) {
		this.grantHeaderId = grantHeaderId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public BigDecimal getAdjustedScore() {
		return adjustedScore;
	}

	public void setAdjustedScore(BigDecimal adjustedScore) {
		this.adjustedScore = adjustedScore;
	}

	public String getIsShortListed() {
		return isShortListed;
	}

	public void setIsShortListed(String isShortListed) {
		this.isShortListed = isShortListed;
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

	public String getProposalRank() {
		return proposalRank;
	}

	public void setProposalRank(String proposalRank) {
		this.proposalRank = proposalRank;
	}

}
