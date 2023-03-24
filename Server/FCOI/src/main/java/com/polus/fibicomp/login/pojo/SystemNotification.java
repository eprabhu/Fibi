package com.polus.fibicomp.login.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "SYSTEM_NOTIFICATION")
public class SystemNotification {

	@Id
	@Column(name = "SYSTEM_NOTIFICATION_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer systemNotificationId;

	@Column(name = "PUBLISHED_START_DATE")
	private Timestamp publishedStartDate;

	@Column(name = "PUBLISHED_END_DATE")
	private Timestamp publishedEndDate;

	@Column(name = "MESSAGE")
	private String message;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "IS_ACTIVE")
	private String isActive;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "PRIORITY")
	private String priority;

	@Transient
	private PersonSystemNotificationMapping personSystemNotificationMapping;

	public Integer getSystemNotificationId() {
		return systemNotificationId;
	}

	public void setSystemNotificationId(Integer systemNotificationId) {
		this.systemNotificationId = systemNotificationId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
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

	public PersonSystemNotificationMapping getPersonSystemNotificationMapping() {
		return personSystemNotificationMapping;
	}

	public void setPersonSystemNotificationMapping(PersonSystemNotificationMapping personSystemNotificationMapping) {
		this.personSystemNotificationMapping = personSystemNotificationMapping;
	}

	public Timestamp getPublishedStartDate() {
		return publishedStartDate;
	}

	public void setPublishedStartDate(Timestamp publishedStartDate) {
		this.publishedStartDate = publishedStartDate;
	}

	public Timestamp getPublishedEndDate() {
		return publishedEndDate;
	}

	public void setPublishedEndDate(Timestamp publishedEndDate) {
		this.publishedEndDate = publishedEndDate;
	}
	public String getPriority() {
		return priority;
	}
	
	public void setPriority(String priority) {
		this.priority = priority;
	}
	
}
