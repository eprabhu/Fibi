package com.polus.fibicomp.grantcall.pojo;

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

import com.polus.fibicomp.pojo.ResearchType;
import com.polus.fibicomp.pojo.ResearchTypeArea;
import com.polus.fibicomp.pojo.ResearchTypeSubArea;

@Entity
@Table(name = "GRANT_CALL_RESEARCH_AREAS")
public class GrantCallResearchArea implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_RESEARCH_AREAS_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_RESEARCH_AREAS_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_RESEARCH_AREAS_ID_GENERATOR", sequenceName = "GRANT_RESEARCH_AREAS_ID_GENERATOR", allocationSize=1)
	private Integer grantResearchAreaId;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@Column(name = "RESRCH_TYPE_CODE")
	private String researchTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_RSRCH_AREA_FK2"), name = "RESRCH_TYPE_CODE", referencedColumnName = "RESRCH_TYPE_CODE", insertable = false, updatable = false)
	private ResearchType researchType;

	@Column(name = "RESRCH_TYPE_AREA_CODE")
	private String researchTypeAreaCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_RSRCH_AREA_FK3"), name = "RESRCH_TYPE_AREA_CODE", referencedColumnName = "RESRCH_TYPE_AREA_CODE", insertable = false, updatable = false)
	private ResearchTypeArea researchTypeArea;

	@Column(name = "RESRCH_TYPE_SUB_AREA_CODE")
	private String researchTypeSubAreaCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_RSRCH_AREA_FK4"), name = "RESRCH_TYPE_SUB_AREA_CODE", referencedColumnName = "RESRCH_TYPE_SUB_AREA_CODE", insertable = false, updatable = false)
	private ResearchTypeSubArea researchTypeSubArea;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getGrantResearchAreaId() {
		return grantResearchAreaId;
	}

	public void setGrantResearchAreaId(Integer grantResearchAreaId) {
		this.grantResearchAreaId = grantResearchAreaId;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
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

	public String getResearchTypeSubAreaCode() {
		return researchTypeSubAreaCode;
	}

	public void setResearchTypeSubAreaCode(String researchTypeSubAreaCode) {
		this.researchTypeSubAreaCode = researchTypeSubAreaCode;
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

}
