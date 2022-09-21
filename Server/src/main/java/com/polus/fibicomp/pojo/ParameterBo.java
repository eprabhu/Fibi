package com.polus.fibicomp.pojo;

import java.io.Serializable;

import javax.persistence.*;

@Entity
@Table(name = "PARAMETER")
public class ParameterBo implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PARAMETER_NAME")
	private String parameterName;

	@Column(name = "VALUE")
	private String value;

	public String getParameterName() {
		return parameterName;
	}

	public void setParameterName(String parameterName) {
		this.parameterName = parameterName;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
