package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_FUNDING_PROPOSALS")
public class AwardFundingProposal implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_FUNDING_PROPOSAL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardFundingProposalId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_FUNDING_PROPOSALS_FK2"), name = "PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID", insertable = false, updatable = false)
	private InstituteProposal proposal;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardFundingProposalId() {
		return awardFundingProposalId;
	}

	public void setAwardFundingProposalId(Integer awardFundingProposalId) {
		this.awardFundingProposalId = awardFundingProposalId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public InstituteProposal getProposal() {
		return proposal;
	}

	public void setProposal(InstituteProposal proposal) {
		this.proposal = proposal;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
