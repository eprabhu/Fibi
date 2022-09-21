package com.polus.fibicomp.report.vo;

public class Template {

	private String coloumnName;

	private String displayName;

	private String filterType;

	private String valueField;

	private Integer index;

	private String queryType;

	public String getColoumnName() {
		return coloumnName;
	}

	public void setColoumnName(String coloumnName) {
		this.coloumnName = coloumnName;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getFilterType() {
		return filterType;
	}

	public void setFilterType(String filterType) {
		this.filterType = filterType;
	}

	public String getValueField() {
		return valueField;
	}

	public void setValueField(String valueField) {
		this.valueField = valueField;
	}

	public Integer getIndex() {
		return index;
	}

	public void setIndex(Integer index) {
		this.index = index;
	}

	public String getQueryType() {
		return queryType;
	}

	public void setQueryType(String queryType) {
		this.queryType = queryType;
	}

}
