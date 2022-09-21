package com.polus.fibicomp.proposal.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "EPS_PROPOSAL_MILESTONE")
public class ProposalMileStone {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_MILESTONE_ID", length = 10)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROPOSAL_MILESTONE_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROPOSAL_MILESTONE_ID_GENERATOR", sequenceName = "EPS_PROPOSAL_MILESTONE_ID_GENERATOR", allocationSize=1)
	private Integer proposalMileStoneId;

	@Column(name = "PROPOSAL_ID", length = 10)
	private Integer proposalId;

	@Column(name = "MILESTONE", length = 1000)
	private String mileStone;

	@Column(name = "START_MONTH", length = 3)
	private Integer startMonth;

	@Column(name = "DURATION", length = 3)
	private Integer duration;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	public Integer getProposalMileStoneId() {
		return proposalMileStoneId;
	}

	public void setProposalMileStoneId(Integer proposalMileStoneId) {
		this.proposalMileStoneId = proposalMileStoneId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getMileStone() {
		return mileStone;
	}

	public void setMileStone(String mileStone) {
		this.mileStone = mileStone;
	}

	public Integer getStartMonth() {
		return startMonth;
	}

	public void setStartMonth(Integer startMonth) {
		this.startMonth = startMonth;
	}

	public Integer getDuration() {
		return duration;
	}

	public void setDuration(Integer duration) {
		this.duration = duration;
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

}
