package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.List;

public class AwardKPIPrintParameter {

	private String KPIType;
	private List<AwardKPICriteriaType> awardKPICriteriaTypes;

	public AwardKPIPrintParameter() {
		awardKPICriteriaTypes = new ArrayList<>();
	}

	public String getKPIType() {
		return KPIType;
	}

	public void setKPIType(String kPIType) {
		KPIType = kPIType;
	}

	public List<AwardKPICriteriaType> getAwardKPICriteriaTypes() {
		return awardKPICriteriaTypes;
	}

	public void setAwardKPICriteriaTypes(List<AwardKPICriteriaType> awardKPICriteriaTypes) {
		this.awardKPICriteriaTypes = awardKPICriteriaTypes;
	}

}
