package com.polus.fibicomp.customdataelement.vo;

public class CustomResponseView {
	
	private Integer customOptionsId;
	
	private Integer customDataElementId;
	
	private String optionName;
	
	private String value;
	
	private Integer ColumnId;
	
	private Integer moduleCode;

	public Integer getCustomOptionsId() {
		return customOptionsId;
	}

	public void setCustomOptionsId(Integer customOptionsId) {
		this.customOptionsId = customOptionsId;
	}

	public Integer getCustomDataElementId() {
		return customDataElementId;
	}

	public void setCustomDataElementId(Integer customDataElementId) {
		this.customDataElementId = customDataElementId;
	}

	public String getOptionName() {
		return optionName;
	}

	public void setOptionName(String optionName) {
		this.optionName = optionName;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public Integer getColumnId() {
		return ColumnId;
	}

	public void setColumnId(Integer columnId) {
		ColumnId = columnId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}
	
}
