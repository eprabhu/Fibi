package com.polus.fibicomp.general.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "HELP_TEXT")
public class HelpText implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "HELP_TEXT_ID")
	private Integer helpTextId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "SECTION_CODE")
	private Integer sectionCode;

	@Column(name = "CATEGORY")
	private String category;

	@Column(name = "HELP_TEXT")
	private String helpTextInfo;

	@Column(name = "PARENT_HELP_TEXT_ID")
	private Integer parentHelpTextId;
	
	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Transient
	private List<Map<String, HelpText>> parentHelpTexts;

	public Integer getHelpTextId() {
		return helpTextId;
	}

	public void setHelpTextId(Integer helpTextId) {
		this.helpTextId = helpTextId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(Integer sectionCode) {
		this.sectionCode = sectionCode;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	public Integer getParentHelpTextId() {
		return parentHelpTextId;
	}

	public void setParentHelpTextId(Integer parentHelpTextId) {
		this.parentHelpTextId = parentHelpTextId;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getHelpTextInfo() {
		return helpTextInfo;
	}

	public void setHelpTextInfo(String helpTextInfo) {
		this.helpTextInfo = helpTextInfo;
	}

	public List<Map<String, HelpText>> getParentHelpTexts() {
		return parentHelpTexts;
	}

	public void setParentHelpTexts(List<Map<String, HelpText>> parentHelpTexts) {
		this.parentHelpTexts = parentHelpTexts;
	}

}
