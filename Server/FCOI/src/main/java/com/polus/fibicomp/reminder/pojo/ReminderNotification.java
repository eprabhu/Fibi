package com.polus.fibicomp.reminder.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "REMINDER_NOTIFICATION")
public class ReminderNotification implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REMINDER_NOTIFICATION_ID")
	private Integer id;

	@Column(name = "REMINDER_NAME")
	private String reminderName;

	@Column(name = "NOTIFICATION_TYPE_ID")
	private Integer notificationId;

	@Column(name = "DAYS_TO_DUE_DATE")
	private Integer daysToDueDate;

	@Column(name = "PROCEDURE_NAME")
	private String procedureName;

	@Column(name = "IS_ACTVE")
	private String isActive;

	@Column(name = "PLACEHOLDER_VALUES")
	private String placeHolderValues;

	@Column(name = "REMINDER_TYPE_FLAG")
	private String reminderTypeFlag;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getReminderName() {
		return reminderName;
	}

	public void setReminderName(String reminderName) {
		this.reminderName = reminderName;
	}

	public Integer getNotificationId() {
		return notificationId;
	}

	public void setNotificationId(Integer notificationId) {
		this.notificationId = notificationId;
	}

	public Integer getDaysToDueDate() {
		return daysToDueDate;
	}

	public void setDaysToDueDate(Integer daysToDueDate) {
		this.daysToDueDate = daysToDueDate;
	}

	public String getProcedureName() {
		return procedureName;
	}

	public void setProcedureName(String procedureName) {
		this.procedureName = procedureName;
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

	public String getPlaceHolderValues() {
		return placeHolderValues;
	}

	public void setPlaceHolderValues(String placeHolderValues) {
		this.placeHolderValues = placeHolderValues;
	}

	public String getReminderTypeFlag() {
		return reminderTypeFlag;
	}

	public void setReminderTypeFlag(String reminderTypeFlag) {
		this.reminderTypeFlag = reminderTypeFlag;
	}

}
