package com.polus.fibicomp.codetable.dto;

import java.util.HashMap;
import java.util.List;

public class Fields {

	private String columnName;

	private String displayName;

	private String dataType;

	private Boolean isEditable;

	private Integer length;

	private Boolean canEmpty;

	private Boolean visible;

	private Boolean valueChanged;

	private String index;

	private String filterType;

	private String valueField;

	private String isTransient;

	private List<HashMap<String, Object>> values;

	private String refColumnName;

	private String defaultValue;

	public String getColumnName() {
		return columnName;
	}

	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}

	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	public String getDataType() {
		return dataType;
	}

	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	public Boolean getIsEditable() {
		return isEditable;
	}

	public void setIsEditable(Boolean isEditable) {
		this.isEditable = isEditable;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public Boolean getCanEmpty() {
		return canEmpty;
	}

	public void setCanEmpty(Boolean canEmpty) {
		this.canEmpty = canEmpty;
	}

	public Boolean getVisible() {
		return visible;
	}

	public void setVisible(Boolean visible) {
		this.visible = visible;
	}

	public Boolean getValueChanged() {
		return valueChanged;
	}

	public void setValueChanged(Boolean valueChanged) {
		this.valueChanged = valueChanged;
	}

	public String getIndex() {
		return index;
	}

	public void setIndex(String index) {
		this.index = index;
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

	public String getIsTransient() {
		return isTransient;
	}

	public void setIsTransient(String isTransient) {
		this.isTransient = isTransient;
	}

	public List<HashMap<String, Object>> getValues() {
		return values;
	}

	public void setValues(List<HashMap<String, Object>> values) {
		this.values = values;
	}

	public String getRefColumnName() {
		return refColumnName;
	}

	public void setRefColumnName(String refColumnName) {
		this.refColumnName = refColumnName;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

}
