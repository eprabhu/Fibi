package com.polus.fibicomp.report.vo;

import java.util.List;

public class ReportJSON {

	private List<Field> reports;

	private String extraCondition;

	private String extraSelection;

	private String innerJoinPlaceHolder;

	private String selectDistinct;

	public List<Field> getReports() {
		return reports;
	}

	public void setReports(List<Field> reports) {
		this.reports = reports;
	}

	public String getExtraCondition() {
		return extraCondition;
	}

	public void setExtraCondition(String extraCondition) {
		this.extraCondition = extraCondition;
	}

	public String getExtraSelection() {
		return extraSelection;
	}

	public void setExtraSelection(String extraSelection) {
		this.extraSelection = extraSelection;
	}

	public String getInnerJoinPlaceHolder() {
		return innerJoinPlaceHolder;
	}

	public void setInnerJoinPlaceHolder(String innerJoinPlaceHolder) {
		this.innerJoinPlaceHolder = innerJoinPlaceHolder;
	}

	public String getSelectDistinct() {
		return selectDistinct;
	}

	public void setSelectDistinct(String selectDistinct) {
		this.selectDistinct = selectDistinct;
	}

}
