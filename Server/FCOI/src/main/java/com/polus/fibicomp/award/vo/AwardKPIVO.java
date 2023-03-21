package com.polus.fibicomp.award.vo;

import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.grantcall.pojo.KPIType;

public class AwardKPIVO {

	private List<AwardKPI> awardKpis;

	private List<AwardKPICriteria> awardKPICriterias;

	private List<KPIType> kpiTypes;

	private Integer awardId;

	private String awardNumber;

	private Integer sequenceNumber;

	private Integer awardKPIId;

	private String message;

	private Integer awardKPICriteriaId;

	private String updateUser;

	private Timestamp updateTimeStamp;

	public List<AwardKPI> getAwardKpis() {
		return awardKpis;
	}

	public void setAwardKpis(List<AwardKPI> awardKpis) {
		this.awardKpis = awardKpis;
	}

	public List<AwardKPICriteria> getAwardKPICriterias() {
		return awardKPICriterias;
	}

	public void setAwardKPICriterias(List<AwardKPICriteria> awardKPICriterias) {
		this.awardKPICriterias = awardKPICriterias;
	}

	public List<KPIType> getKpiTypes() {
		return kpiTypes;
	}

	public void setKpiTypes(List<KPIType> kpiTypes) {
		this.kpiTypes = kpiTypes;
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

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Integer getAwardKPIId() {
		return awardKPIId;
	}

	public void setAwardKPIId(Integer awardKPIId) {
		this.awardKPIId = awardKPIId;
	}

	public Integer getAwardKPICriteriaId() {
		return awardKPICriteriaId;
	}

	public void setAwardKPICriteriaId(Integer awardKPICriteriaId) {
		this.awardKPICriteriaId = awardKPICriteriaId;
	}

}
