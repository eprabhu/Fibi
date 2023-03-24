package com.polus.fibicomp.adminportal.vo;

import java.util.List;
import com.polus.fibicomp.adminportal.pojo.InstituteLARate;
import com.polus.fibicomp.adminportal.pojo.RateClass;
import com.polus.fibicomp.adminportal.pojo.RateType;

public class RateLaVO {
	private String unitNumber;
	private String unitName;
	private List<InstituteLARate> instituteLARatesList;
	private List<RateClass> rateClassLaList;
	private List<RateType> rateTypeLaList;
	private InstituteLARate instituteLARate;
	private String campusFlag;
	
	public List<InstituteLARate> getInstituteLARatesList() {
		return instituteLARatesList;
	}
	
	public void setInstituteLARatesList(List<InstituteLARate> instituteLARatesList) {
		this.instituteLARatesList = instituteLARatesList;
	}
	
	public List<RateClass> getRateClassLaList() {
		return rateClassLaList;
	}
	
	public void setRateClassLaList(List<RateClass> rateClassLaList) {
		this.rateClassLaList = rateClassLaList;
	}
	
	public List<RateType> getRateTypeLaList() {
		return rateTypeLaList;
	}
	
	public void setRateTypeLaList(List<RateType> rateTypeLaList) {
		this.rateTypeLaList = rateTypeLaList;
	}
	
	public InstituteLARate getInstituteLARate() {
		return instituteLARate;
	}
	
	public void setInstituteLARate(InstituteLARate instituteLARate) {
		this.instituteLARate = instituteLARate;
	}
	
	public String getCampusFlag() {
		return campusFlag;
	}
	
	public void setCampusFlag(String campusFlag) {
		this.campusFlag = campusFlag;
	}
	
	public String getUnitNumber() {
		return unitNumber;
	}
	
	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}
	
	public String getUnitName() {
		return unitName;
	}
	
	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}
}
