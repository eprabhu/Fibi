package com.polus.fibicomp.servicerequest.dto;

import java.util.ArrayList;
import java.util.List;

public class ServiceRequestType {

	private String property;
	private Integer type_code;
	private List<Field> fields;

	public ServiceRequestType() {
		fields = new ArrayList<Field>();
	}

	public String getProperty() {
		return property;
	}
	public void setProperty(String property) {
		this.property = property;
	}
	public Integer getType_code() {
		return type_code;
	}
	public void setType_code(Integer type_code) {
		this.type_code = type_code;
	}
	public List<Field> getFields() {
		return fields;
	}
	public void setFields(List<Field> fields) {
		this.fields = fields;
	}

}
