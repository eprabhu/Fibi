package com.polus.fibicomp.pojo;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "ARG_VALUE_LOOKUP")
public class ArgValueLookup {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ARG_VALUE_LOOKUP_ID")
	private String argValueLooupId;

	@Column(name = "ARGUMENT_NAME")
	private String argumentName;

	@Column(name = "ARGUMENT_VALUE")
	private String argumentvalue;

	@Column(name = "DESCRIPTION")
	private String description;

	public String getArgValueLooupId() {
		return argValueLooupId;
	}

	public void setArgValueLooupId(String argValueLooupId) {
		this.argValueLooupId = argValueLooupId;
	}

	public String getArgumentName() {
		return argumentName;
	}

	public void setArgumentName(String argumentName) {
		this.argumentName = argumentName;
	}

	public String getArgumentvalue() {
		return argumentvalue;
	}

	public void setArgumentvalue(String argumentvalue) {
		this.argumentvalue = argumentvalue;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
	
}
