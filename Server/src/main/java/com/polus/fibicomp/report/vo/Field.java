package com.polus.fibicomp.report.vo;

public class Field {

	private String summable;

	private String displayName;

	private String index;

	private String filterType;

	private String valueField;

	private String columnName;

	private String queryType;

	private String innerJoin;

	private String excludeDisplay;

	private String isCurrency;

	private String isEncrypted;

	private String defaultValue;

	private Boolean isMandatory;

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getColumnName() {
		return columnName;
	}

	public void setColumnName(String columnName) {
		this.columnName = columnName;
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

	public String getIndex() {
		return index;
	}

	public void setIndex(String index) {
		this.index = index;
	}

	public String getQueryType() {
		return queryType;
	}

	public void setQueryType(String queryType) {
		this.queryType = queryType;
	}

	public String getSummable() {
		return summable;
	}

	public void setSummable(String summable) {
		this.summable = summable;
	}

	public String getInnerJoin() {
		return innerJoin;
	}

	public void setInnerJoin(String innerJoin) {
		this.innerJoin = innerJoin;
	}

	public String getExcludeDisplay() {
		return excludeDisplay;
	}

	public void setExcludeDisplay(String excludeDisplay) {
		this.excludeDisplay = excludeDisplay;
	}

	public String getIsCurrency() {
		return isCurrency;
	}

	public void setIsCurrency(String isCurrency) {
		this.isCurrency = isCurrency;
	}

	public String getIsEncrypted() {
		return isEncrypted;
	}

	public void setIsEncrypted(String isEncrypted) {
		this.isEncrypted = isEncrypted;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public Boolean getIsMandatory() {
		return isMandatory;
	}

	public void setIsMandatory(Boolean isMandatory) {
		this.isMandatory = isMandatory;
	}

}
