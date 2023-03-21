package com.polus.fibicomp.person.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "PERSON_PREFERENCE")
public class PersonPreference implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "PREFERENCE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer preferenceId;
	
	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "PREFERENCES_TYPE_CODE")
	private String preferencesTypeCode;
	
	@Column(name = "VALUE")
	private String value;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getPreferencesTypeCode() {
		return preferencesTypeCode;
	}

	public void setPreferencesTypeCode(String preferencesTypeCode) {
		this.preferencesTypeCode = preferencesTypeCode;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
	
	public Integer getPreferenceId() {
		return preferenceId;
	}

	public void setPreferenceId(Integer preferenceId) {
		this.preferenceId = preferenceId;
	}

}
