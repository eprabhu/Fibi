package com.polus.fibicomp.sectionwiseedit.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "MODULE_VARIABLE_SECTION")
public class ModuleVariableSection implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "VARIABLE_SECTION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_MODULE_VARIABLE_SECTION")
	@SequenceGenerator(name="SEQ_MODULE_VARIABLE_SECTION)", sequenceName = "SEQ_MODULE_VARIABLE_SECTION", allocationSize=1)
	private Integer variableSectionId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "SECTION_CODE")
	private String sectionCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "MODULE_VARIABLE_SECTION_FK2"), name = "SECTION_CODE", referencedColumnName = "SECTION_CODE", insertable = false, updatable = false)
	private SectionType sectionType;

	@Column(name = "TYPE_CODE")
	private Integer typeCode;
	
	@Column(name = "VARIABLE_TYPE")
	private String variableType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "SUB_MODULE_ITEM_KEY")
	private String subModuleItemKey;

	@Column(name = "PERSON_ID")
	private String personId;

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

	public String getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(String sectionCode) {
		this.sectionCode = sectionCode;
	}

	public SectionType getSectionType() {
		return sectionType;
	}

	public void setSectionType(SectionType sectionType) {
		this.sectionType = sectionType;
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

	public Integer getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(Integer typeCode) {
		this.typeCode = typeCode;
	}

	public Integer getVariableSectionId() {
		return variableSectionId;
	}

	public void setVariableSectionId(Integer variableSectionId) {
		this.variableSectionId = variableSectionId;
	}

	public String getVariableType() {
		return variableType;
	}

	public void setVariableType(String variableType) {
		this.variableType = variableType;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

}
