package com.polus.fibicomp.adminportal.vo;

import java.util.List;

import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.adminportal.pojo.RateClass;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.pojo.ActivityType;

public class RateVO {

	private String unitNumber;
	private String unitName;
	private List<RateClass> rateClassList;
	private List<RateType> rateTypeList;
	private List<ActivityType> activityTypeList;
	private List<InstituteRate> instituteRatesList;
	private InstituteRate instituteRate;
	private String campusFlag;

	public String getCampusFlag() {
		return campusFlag;
	}
	
	public void setCampusFlag(String campusFlag) {
		this.campusFlag = campusFlag;
	}
	
	public InstituteRate getInstituteRate() {
		return instituteRate;
	}
	
	public void setInstituteRate(InstituteRate instituteRate) {
		this.instituteRate = instituteRate;
	}
	
	public List<RateClass> getRateClassList() {
		return rateClassList;
	}
	
	public void setRateClassList(List<RateClass> rateClassList) {
		this.rateClassList = rateClassList;
	}
	
	public List<RateType> getRateTypeList() {
		return rateTypeList;
	}
	
	public void setRateTypeList(List<RateType> rateTypeList) {
		this.rateTypeList = rateTypeList;
	}
	
	public String getUnitNumber() {
		return unitNumber;
	}
	
	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}
	
	public List<ActivityType> getActivityTypeList() {
		return activityTypeList;
	}
	
	public void setActivityTypeList(List<ActivityType> activityTypeList) {
		this.activityTypeList = activityTypeList;
	}
	
	public String getUnitName() {
		return unitName;
	}
	
	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}
	
	public List<InstituteRate> getInstituteRatesList() {
		return instituteRatesList;
	}
	
	public void setInstituteRatesList(List<InstituteRate> instituteRatesList) {
		this.instituteRatesList = instituteRatesList;
	}
}
