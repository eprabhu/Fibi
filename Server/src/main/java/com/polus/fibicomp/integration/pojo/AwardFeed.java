package com.polus.fibicomp.integration.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "TEMP_USER")
public class AwardFeed implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ID")
	private Integer id;

	@Column(name = "USER_NAME")
	private String userName;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "IS_FO_USER")
	private String isFoUser;

	@Column(name = "CREATED_USER_NAME")
	private String createdUserName;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getIsFoUser() {
		return isFoUser;
	}

	public void setIsFoUser(String isFoUser) {
		this.isFoUser = isFoUser;
	}

	public String getCreatedUserName() {
		return createdUserName;
	}

	public void setCreatedUserName(String createdUserName) {
		this.createdUserName = createdUserName;
	}

}
