package com.polus.fibicomp.customdataelement.vo;

import java.util.List;

import com.polus.fibicomp.customdataelement.pojo.CustomData;

public class CustomDataResponse {
		
	private Integer customDataElementId;
	
	private String columnName;
	
	private String defaultValue;
	
	private String dataType;
	
	private String isRequired;
	
	private List<String> options;
	
	private List<CustomData> answers;
	
	private Integer moduleItemCode;
	
	private String moduleItemKey;
	
	private Integer dataLength;
	
	private Integer columnId;
	
	private Integer versionNumber;

	private String lookupWindow;

	private String lookupArgument;

	private String filterType;

	private Integer orderNumber;

	private String isActive;
	
	public Integer getColumnId() {
		return columnId;
	}

	public void setColumnId(Integer columnId) {
		this.columnId = columnId;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Integer getCustomDataElementId() {
		return customDataElementId;
	}

	public void setCustomDataElementId(Integer customDataElementId) {
		this.customDataElementId = customDataElementId;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public String getDataType() {
		return dataType;
	}

	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	public String getIsRequired() {
		return isRequired;
	}

	public void setIsRequired(String isRequired) {
		this.isRequired = isRequired;
	}

	public List<String> getOptions() {
		return options;
	}

	public void setOptions(List<String> options) {
		this.options = options;
	}

	public List<CustomData> getAnswers() {
		return answers;
	}

	public void setAnswers(List<CustomData> answers) {
		this.answers = answers;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getDataLength() {
		return dataLength;
	}

	public void setDataLength(Integer dataLength) {
		this.dataLength = dataLength;
	}

	public String getLookupWindow() {
		return lookupWindow;
	}

	public void setLookupWindow(String lookupWindow) {
		this.lookupWindow = lookupWindow;
	}

	public String getLookupArgument() {
		return lookupArgument;
	}

	public void setLookupArgument(String lookupArgument) {
		this.lookupArgument = lookupArgument;
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

	public Integer getOrderNumber() {
		return orderNumber;
	}

	public void setOrderNumber(Integer orderNumber) {
		this.orderNumber = orderNumber;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}
}
