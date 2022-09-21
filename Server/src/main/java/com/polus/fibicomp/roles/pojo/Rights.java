package com.polus.fibicomp.roles.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "RIGHTS")
public class Rights implements Serializable {


	private static final long serialVersionUID = 1L;

	@Id	
	@Column(name= "RIGHT_ID")
	private Integer rightId;
	
	@Column(name= "RIGHT_NAME")
	private String rightName;
	
	@Column(name= "DESCRIPTION")
	private String description;

	public Integer getRightId() {
		return rightId;
	}
	public void setRightId(Integer rightId) {
		this.rightId = rightId;
	}

	public String getRightName() {
		return rightName;
	}

	public void setRightName(String rightName) {
		this.rightName = rightName;
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
