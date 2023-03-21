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
@Table(name = "TRIAGE_TEMPLATE_MAPPING")
public class TriageTemplateMapping implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_TRIAGE_TEMPLATE")
	@SequenceGenerator(name="SEQ_TRIAGE_TEMPLATE", sequenceName = "SEQ_TRIAGE_TEMPLATE", allocationSize=1)
	@Column(name = "TRIAGE_TEMPLATE_ID")
	private Integer triageTemplateId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getTriageTemplateId() {
		return triageTemplateId;
	}

	public void setTriageTemplateId(Integer triageTemplateId) {
		this.triageTemplateId = triageTemplateId;
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

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
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

}
