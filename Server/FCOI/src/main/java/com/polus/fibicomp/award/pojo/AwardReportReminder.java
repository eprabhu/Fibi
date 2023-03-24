package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_REPORT_REMINDER")
public class AwardReportReminder implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REMINDER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AWARD_REPORT_REMINDER")
	@SequenceGenerator(name="SEQ_AWARD_REPORT_REMINDER", sequenceName = "SEQ_AWARD_REPORT_REMINDER", allocationSize=1)
	private Integer remainderId;

	@Column(name = "REPORT_CLASS_CODE")
	private String reportClassCode;

	@Column(name = "REPORT_CODE")
	private String reportCode;

	@Column(name = "FREQUENCY_CODE")
	private String frequencyCode;

	@Column(name = "DAYS_TO_DUE_DATE")
	private Integer daysToDueDate;

	@Column(name = "NOTIFICATION_TYPE_ID")
	private Integer notificationTypeId;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive = false;

	public Integer getRemainderId() {
		return remainderId;
	}

	public void setRemainderId(Integer remainderId) {
		this.remainderId = remainderId;
	}

	public String getReportClassCode() {
		return reportClassCode;
	}

	public void setReportClassCode(String reportClassCode) {
		this.reportClassCode = reportClassCode;
	}

	public String getReportCode() {
		return reportCode;
	}

	public void setReportCode(String reportCode) {
		this.reportCode = reportCode;
	}

	public String getFrequencyCode() {
		return frequencyCode;
	}

	public void setFrequencyCode(String frequencyCode) {
		this.frequencyCode = frequencyCode;
	}

	public Integer getNotificationTypeId() {
		return notificationTypeId;
	}

	public void setNotificationTypeId(Integer notificationTypeId) {
		this.notificationTypeId = notificationTypeId;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public Integer getDaysToDueDate() {
		return daysToDueDate;
	}

	public void setDaysToDueDate(Integer daysToDueDate) {
		this.daysToDueDate = daysToDueDate;
	}

}
