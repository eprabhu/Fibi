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

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.ScienceKeyword;

@Entity
@Table(name = "AWARD_SCIENCE_KEYWORD")
public class AwardKeyword implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_SCIENCE_KEYWORD_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardKeywordId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_SCIENCE_KEYWORD_FK1"), name = "AWARD_ID", referencedColumnName = "AWARD_ID")
	private Award award;

	@Column(name = "SCIENCE_KEYWORD_CODE")
	private String scienceKeywordCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_SCIENCE_KEYWORD_FK2"), name = "SCIENCE_KEYWORD_CODE", referencedColumnName = "SCIENCE_KEYWORD_CODE", insertable = false, updatable = false)
	private ScienceKeyword scienceKeyword;

	@Column(name = "KEYWORD")
	private String keyword;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardKeywordId() {
		return awardKeywordId;
	}

	public void setAwardKeywordId(Integer awardKeywordId) {
		this.awardKeywordId = awardKeywordId;
	}

	public String getScienceKeywordCode() {
		return scienceKeywordCode;
	}

	public void setScienceKeywordCode(String scienceKeywordCode) {
		this.scienceKeywordCode = scienceKeywordCode;
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

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public ScienceKeyword getScienceKeyword() {
		return scienceKeyword;
	}

	public void setScienceKeyword(ScienceKeyword scienceKeyword) {
		this.scienceKeyword = scienceKeyword;
	}

	public String getKeyword() {
		return keyword;
	}

	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}

}
