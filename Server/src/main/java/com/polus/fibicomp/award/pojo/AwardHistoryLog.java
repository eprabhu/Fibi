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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_HISTORY_LOG")
public class AwardHistoryLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_HISTORY_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_HISTORY_LOG_ID_GENERATOR")
	@SequenceGenerator(name = "AWARD_HISTORY_LOG_ID_GENERATOR", sequenceName = "AWARD_HISTORY_LOG_ID_GENERATOR", allocationSize = 1)
	private Integer awardHistoryLogId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_HISTORY_LOG_FK1"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "LAST_MERGED_AWARD_CREATON")
	private Integer lastMergedawardIdCreation;

	@Column(name = "LAST_MERGED_AWARD_APPRVAL")
	private Integer lastMergedawardIdApproval;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardHistoryLogId() {
		return awardHistoryLogId;
	}

	public void setAwardHistoryLogId(Integer awardHistoryLogId) {
		this.awardHistoryLogId = awardHistoryLogId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
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

	public Integer getLastMergedawardIdCreation() {
		return lastMergedawardIdCreation;
	}

	public void setLastMergedawardIdCreation(Integer lastMergedawardIdCreation) {
		this.lastMergedawardIdCreation = lastMergedawardIdCreation;
	}

	public Integer getLastMergedawardIdApproval() {
		return lastMergedawardIdApproval;
	}

	public void setLastMergedawardIdApproval(Integer lastMergedawardIdApproval) {
		this.lastMergedawardIdApproval = lastMergedawardIdApproval;
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

}
