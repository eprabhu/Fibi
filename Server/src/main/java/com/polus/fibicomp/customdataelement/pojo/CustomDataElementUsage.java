package com.polus.fibicomp.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.Module;

@Entity
@Table (name = "CUSTOM_DATA_ELEMENT_USAGE")
public class CustomDataElementUsage implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "customDataElementUsageIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "customDataElementUsageIdGenerator")
	@Column(name = "CUSTOM_DATA_ELEMENT_USAGE_ID")
	private Integer customElementUsageId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CUSTOM_DATA_ELEMENT_USAGE_FK1"), name = "CUSTOM_DATA_ELEMENTS_ID", referencedColumnName = "CUSTOM_DATA_ELEMENTS_ID")
	private CustomDataElements customDataElement;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "CUSTOM_DATA_ELEMENT_USAGE_FK2"), name = "MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module module;

	@Column(name = "IS_REQUIRED")
	private String isRequired;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "ORDER_NUMBER")
	private Integer orderNumber;

	@Transient
	private String acType;

	public Integer getCustomElementUsageId() {
		return customElementUsageId;
	}

	public void setCustomElementUsageId(Integer customElementUsageId) {
		this.customElementUsageId = customElementUsageId;
	}

	public CustomDataElements getCustomDataElement() {
		return customDataElement;
	}

	public void setCustomDataElement(CustomDataElements customDataElement) {
		this.customDataElement = customDataElement;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getIsRequired() {
		return isRequired;
	}

	public void setIsRequired(String isRequired) {
		this.isRequired = isRequired;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Module getModule() {
		return module;
	}

	public void setModule(Module module) {
		this.module = module;
	}

	public Integer getOrderNumber() {
		return orderNumber;
	}

	public void setOrderNumber(Integer orderNumber) {
		this.orderNumber = orderNumber;
	}
}
