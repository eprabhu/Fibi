package com.polus.fibicomp.triagequestionnaire.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "TRIAGE_HEADER")
public class TriageHeader implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_TRIAGE_HEADER")
	@SequenceGenerator(name="SEQ_TRIAGE_HEADER", sequenceName = "SEQ_TRIAGE_HEADER", allocationSize=1)
	@Column(name = "TRIAGE_HEADER_ID")
	private Integer triageHeaderId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "SUB_MODULE_ITEM_KEY")
	private String subModuleItemKey;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "TRIAGE_STATUS")
	private String triageStatus;

	public Integer getTriageHeaderId() {
		return triageHeaderId;
	}

	public void setTriageHeaderId(Integer triageHeaderId) {
		this.triageHeaderId = triageHeaderId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
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

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public String getTriageStatus() {
		return triageStatus;
	}

	public void setTriageStatus(String triageStatus) {
		this.triageStatus = triageStatus;
	}

}
