package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "FREQUENCY_BASE")
public class FrequencyBase implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FREQUENCY_BASE_CODE")
	private String frequencyBaseCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "REGENERATION_TYPE_NAME")
	private String regenerationTypeName;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean active;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "INCLUDE_BASE_DATE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean includeBaseDate;

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getRegenerationTypeName() {
		return regenerationTypeName;
	}

	public void setRegenerationTypeName(String regenerationTypeName) {
		this.regenerationTypeName = regenerationTypeName;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getFrequencyBaseCode() {
		return frequencyBaseCode;
	}

	public void setFrequencyBaseCode(String frequencyBaseCode) {
		this.frequencyBaseCode = frequencyBaseCode;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Boolean getActive() {
		return active;
	}

	public void setActive(Boolean active) {
		this.active = active;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

	public Boolean getIncludeBaseDate() {
		return includeBaseDate;
	}

	public void setIncludeBaseDate(Boolean includeBaseDate) {
		this.includeBaseDate = includeBaseDate;
	}

}
