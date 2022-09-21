package com.polus.fibicomp.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "CUSTOM_DATA")
public class CustomData implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "CUSTOM_DATA_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer customDataId;
	
	@Column(name = "CUSTOM_DATA_ELEMENTS_ID")
	private Integer customDataElementsId;
	
	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;
	
	@Column(name = "MODULE_SUB_ITEM_CODE")
	private Integer moduleSubItemCode;
	
	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;
	
	@Column(name = "MODULE_SUB_ITEM_KEY")
	private String moduleSubItemKey;
	
	@Column(name = "VALUE")
	private String value;
	
	@Column(name = "COLUMN_ID")
	private Integer columnId;
	
	@Column(name = "COLUMN_VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public CustomData() {
		super();
	}

	public CustomData(Integer customDataId, String value, String description) {
		super();
		this.customDataId = customDataId;
		this.value = value;
		this.description = description;
	}

	public Integer getCustomDataId() {
		return customDataId;
	}

	public void setCustomDataId(Integer customDataId) {
		this.customDataId = customDataId;
	}

	public Integer getCustomDataElementsId() {
		return customDataElementsId;
	}

	public void setCustomDataElementsId(Integer customDataElementsId) {
		this.customDataElementsId = customDataElementsId;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public Integer getModuleSubItemCode() {
		return moduleSubItemCode;
	}

	public void setModuleSubItemCode(Integer moduleSubItemCode) {
		this.moduleSubItemCode = moduleSubItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleSubItemKey() {
		return moduleSubItemKey;
	}

	public void setModuleSubItemKey(String moduleSubItemKey) {
		this.moduleSubItemKey = moduleSubItemKey;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}
	
	public Integer getColumnId() {
		return columnId;
	}

	public void setColumnId(Integer columnId) {
		this.columnId = columnId;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

}
