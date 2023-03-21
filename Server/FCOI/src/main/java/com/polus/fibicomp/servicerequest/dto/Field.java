package com.polus.fibicomp.servicerequest.dto;

public class Field {

	private String field_name;
	private String field_display_type;
	private String data_type;
	private Boolean can_empty;
	private Boolean is_editable;

	public String getField_name() {
		return field_name;
	}

	public void setField_name(String field_name) {
		this.field_name = field_name;
	}

	public String getField_display_type() {
		return field_display_type;
	}

	public void setField_display_type(String field_display_type) {
		this.field_display_type = field_display_type;
	}

	public String getData_type() {
		return data_type;
	}

	public void setData_type(String data_type) {
		this.data_type = data_type;
	}
	
	public Boolean getCan_empty() {
		return can_empty;
	}

	public void setCan_empty(Boolean can_empty) {
		this.can_empty = can_empty;
	}

	public Boolean getIs_editable() {
		return is_editable;
	}

	public void setIs_editable(Boolean is_editable) {
		this.is_editable = is_editable;
	}

}
