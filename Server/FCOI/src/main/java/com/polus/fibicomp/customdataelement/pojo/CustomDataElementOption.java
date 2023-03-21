package com.polus.fibicomp.customdataelement.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

@Entity
@Table(name = "CUSTOM_DATA_ELEMENTS_OPTIONS")
public class CustomDataElementOption implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@Id
	@GenericGenerator(name = "customDataOptionIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "customDataOptionIdGenerator")
	@Column(name = "CUSTOM_DATA_OPTION_ID")
	private Integer customDataOptionId;
	
	@Column(name = "CUSTOM_DATA_ELEMENTS_ID")
	private Integer customDataElementsId;
	
	@Column(name = "OPTION_NAME")
	private String optionName;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String acType;

	public Integer getCustomDataOptionId() {
		return customDataOptionId;
	}

	public void setCustomDataOptionId(Integer customDataOptionId) {
		this.customDataOptionId = customDataOptionId;
	}

	public Integer getCustomDataElementsId() {
		return customDataElementsId;
	}

	public void setCustomDataElementsId(Integer customDataElementsId) {
		this.customDataElementsId = customDataElementsId;
	}

	public String getOptionName() {
		return optionName;
	}

	public void setOptionName(String optionName) {
		this.optionName = optionName;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

}
