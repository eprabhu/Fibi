package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

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

import com.polus.fibicomp.award.dto.SponsorTermCodeData;

@Entity
@Table(name = "AWARD_SPONSOR_TERM")
public class AwardSponsorTerm implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_SPONSOR_TERM_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardSponsorTermId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_SPONSOR_TERM_FK1"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "SPONSOR_TERM_TYPE_CODE")
	private String sponsorTermTypeCode;

	@Transient
	private String sponsorTermType;

	@Column(name = "SPONSOR_TERM_CODE")
	private String sponsorTermCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private List<SponsorTermCodeData> sponsorTermCodeList;

	public Integer getAwardSponsorTermId() {
		return awardSponsorTermId;
	}

	public void setAwardSponsorTermId(Integer awardSponsorTermId) {
		this.awardSponsorTermId = awardSponsorTermId;
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

	public String getSponsorTermTypeCode() {
		return sponsorTermTypeCode;
	}

	public void setSponsorTermTypeCode(String sponsorTermTypeCode) {
		this.sponsorTermTypeCode = sponsorTermTypeCode;
	}

	public String getSponsorTermCode() {
		return sponsorTermCode;
	}

	public void setSponsorTermCode(String sponsorTermCode) {
		this.sponsorTermCode = sponsorTermCode;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public List<SponsorTermCodeData> getSponsorTermCodeList() {
		return sponsorTermCodeList;
	}

	public void setSponsorTermCodeList(List<SponsorTermCodeData> sponsorTermCodeList) {
		this.sponsorTermCodeList = sponsorTermCodeList;
	}

	public String getSponsorTermType() {
		return sponsorTermType;
	}

	public void setSponsorTermType(String sponsorTermType) {
		this.sponsorTermType = sponsorTermType;
	}

}
