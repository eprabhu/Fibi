package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "MANPOWER_TEMP")
public class ManpowerTemp implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_ID")
	private String manpowerPersonId;

	@Column(name = "NATIONALITY")
	private String nationality;

	@Column(name = "CITIZENSHIP")
	private String citizenship;

	public String getManpowerPersonId() {
		return manpowerPersonId;
	}

	public void setManpowerPersonId(String manpowerPersonId) {
		this.manpowerPersonId = manpowerPersonId;
	}

	public String getNationality() {
		return nationality;
	}

	public void setNationality(String nationality) {
		this.nationality = nationality;
	}

	public String getCitizenship() {
		return citizenship;
	}

	public void setCitizenship(String citizenship) {
		this.citizenship = citizenship;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
