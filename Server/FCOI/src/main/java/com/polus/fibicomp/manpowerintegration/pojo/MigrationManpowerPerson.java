package com.polus.fibicomp.manpowerintegration.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "MIGRATION_MANPOWER_PERSON")
public class MigrationManpowerPerson implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "MIGRATION_MANPOWER_PERSON_ID")
	private String manpowerPersonId;

	@Column(name = "WORKERID")
	private String workerPersonId;

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

	public String getWorkerPersonId() {
		return workerPersonId;
	}

	public void setWorkerPersonId(String workerPersonId) {
		this.workerPersonId = workerPersonId;
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

}
