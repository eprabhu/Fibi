package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "RESEARCH_TYPE_SUB_AREA")
public class ResearchTypeSubArea implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "RESRCH_TYPE_SUB_AREA_CODE")
	private String researchTypeSubAreaCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "RESRCH_TYPE_AREA_CODE")
	private String researchTypeAreaCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "RESEARCH_TYPE_SUB_AREA_FK2"), name = "RESRCH_TYPE_AREA_CODE", referencedColumnName = "RESRCH_TYPE_AREA_CODE", insertable = false, updatable = false)
	private ResearchTypeArea researchTypeArea;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getResearchTypeSubAreaCode() {
		return researchTypeSubAreaCode;
	}

	public void setResearchTypeSubAreaCode(String researchTypeSubAreaCode) {
		this.researchTypeSubAreaCode = researchTypeSubAreaCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

}
