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

import com.polus.fibicomp.pojo.ResearchType;
import com.polus.fibicomp.pojo.ResearchTypeArea;
import com.polus.fibicomp.pojo.ResearchTypeSubArea;

@Entity
@Table(name = "AWARD_RESEARCH_AREAS")
public class AwardResearchArea implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "RESRCH_AREA_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer researchAreaId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "RESRCH_TYPE_CODE")
	private String researchTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_RESEARCH_AREAS_FK2"), name = "RESRCH_TYPE_CODE", referencedColumnName = "RESRCH_TYPE_CODE", insertable = false, updatable = false)
	private ResearchType researchType;

	@Column(name = "RESRCH_TYPE_AREA_CODE")
	private String researchTypeAreaCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_RESEARCH_AREAS_FK3"), name = "RESRCH_TYPE_AREA_CODE", referencedColumnName = "RESRCH_TYPE_AREA_CODE", insertable = false, updatable = false)
	private ResearchTypeArea researchTypeArea;

	@Column(name = "RESRCH_TYPE_SUB_AREA_CODE")
	private String researchTypeSubAreaCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_RESEARCH_AREAS_FK4"), name = "RESRCH_TYPE_SUB_AREA_CODE", referencedColumnName = "RESRCH_TYPE_SUB_AREA_CODE", insertable = false, updatable = false)
	private ResearchTypeSubArea researchTypeSubArea;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getResearchAreaId() {
		return researchAreaId;
	}

	public void setResearchAreaId(Integer researchAreaId) {
		this.researchAreaId = researchAreaId;
	}

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

	public String getResearchTypeCode() {
		return researchTypeCode;
	}

	public void setResearchTypeCode(String researchTypeCode) {
		this.researchTypeCode = researchTypeCode;
	}

	public ResearchType getResearchType() {
		return researchType;
	}

	public void setResearchType(ResearchType researchType) {
		this.researchType = researchType;
	}

	public String getResearchTypeAreaCode() {
		return researchTypeAreaCode;
	}

	public void setResearchTypeAreaCode(String researchTypeAreaCode) {
		this.researchTypeAreaCode = researchTypeAreaCode;
	}

	public ResearchTypeArea getResearchTypeArea() {
		return researchTypeArea;
	}

	public void setResearchTypeArea(ResearchTypeArea researchTypeArea) {
		this.researchTypeArea = researchTypeArea;
	}

	public ResearchTypeSubArea getResearchTypeSubArea() {
		return researchTypeSubArea;
	}

	public void setResearchTypeSubArea(ResearchTypeSubArea researchTypeSubArea) {
		this.researchTypeSubArea = researchTypeSubArea;
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

	public String getResearchTypeSubAreaCode() {
		return researchTypeSubAreaCode;
	}

	public void setResearchTypeSubAreaCode(String researchTypeSubAreaCode) {
		this.researchTypeSubAreaCode = researchTypeSubAreaCode;
	}

}
