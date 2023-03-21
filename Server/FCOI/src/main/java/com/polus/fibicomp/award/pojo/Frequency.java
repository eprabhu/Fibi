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
@Table(name = "FREQUENCY")
public class Frequency implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FREQUENCY_CODE")
	private String frequencyCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "NUMBER_OF_DAYS")
	private Integer numberOfDays;

	@Column(name = "NUMBER_OF_MONTHS")
	private String numberOfMonths;

	@Column(name = "REPEAT_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean repeatFlag;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ADVANCE_NUMBER_OF_DAYS")
	private Integer advanceNumberOfDays;

	@Column(name = "ADVANCE_NUMBER_OF_MONTHS")
	private String advanceNumberOfMonths;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean active;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "IS_INVOICE_FREQUENCY")
	private String isInvoiceFrequency;

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getNumberOfDays() {
		return numberOfDays;
	}

	public void setNumberOfDays(Integer numberOfDays) {
		this.numberOfDays = numberOfDays;
	}

	public String getNumberOfMonths() {
		return numberOfMonths;
	}

	public void setNumberOfMonths(String numberOfMonths) {
		this.numberOfMonths = numberOfMonths;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getAdvanceNumberOfDays() {
		return advanceNumberOfDays;
	}

	public void setAdvanceNumberOfDays(Integer advanceNumberOfDays) {
		this.advanceNumberOfDays = advanceNumberOfDays;
	}

	public String getAdvanceNumberOfMonths() {
		return advanceNumberOfMonths;
	}

	public void setAdvanceNumberOfMonths(String advanceNumberOfMonths) {
		this.advanceNumberOfMonths = advanceNumberOfMonths;
	}

	
	public String getFrequencyCode() {
		return frequencyCode;
	}

	public void setFrequencyCode(String frequencyCode) {
		this.frequencyCode = frequencyCode;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Boolean getRepeatFlag() {
		return repeatFlag;
	}

	public void setRepeatFlag(Boolean repeatFlag) {
		this.repeatFlag = repeatFlag;
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

	public String getIsInvoiceFrequency() {
		return isInvoiceFrequency;
	}

	public void setIsInvoiceFrequency(String isInvoiceFrequency) {
		this.isInvoiceFrequency = isInvoiceFrequency;
	}

}

