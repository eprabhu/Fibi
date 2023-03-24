package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "AWARD_KEYPERSON_TIMESHEET")
@EntityListeners(AuditingEntityListener.class)
public class AwardKeyPersonTimesheet implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KEYPERSON_TIMESHEET_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer keypersonTimesheetId;

	@Column(name= "YEAR")
	private String year;

	@Column(name= "AWARD_NUMBER")
	private String awardNumber;

	@Column(name= "AWARD_ID")
	private Integer awardId;

	@Column(name= "AWARD_PERSON_ID")
	private Integer awardPersonId;

	@JsonIgnore
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_KEYPERSON_TIMESHEET_FK1"), name = "AWARD_PERSON_ID", referencedColumnName = "AWARD_PERSON_ID", insertable = false, updatable = false)
	private AwardPerson awardPerson;

	@Column(name= "VALUE", precision = 5, scale = 2)
	private BigDecimal value;

	@Column(name= "TIMESHEET_TYPE")
	private String timesheetType;

	@Column(name= "ORDER_NUMBER")
	private Integer orderNumber;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String createUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Integer getAwardPersonId() {
		return awardPersonId;
	}

	public void setAwardPersonId(Integer awardPersonId) {
		this.awardPersonId = awardPersonId;
	}

	public String getTimesheetType() {
		return timesheetType;
	}

	public void setTimesheetType(String timesheetType) {
		this.timesheetType = timesheetType;
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

	public AwardPerson getAwardPerson() {
		return awardPerson;
	}

	public void setAwardPerson(AwardPerson awardPerson) {
		this.awardPerson = awardPerson;
	}

	public Integer getOrderNumber() {
		return orderNumber;
	}

	public void setOrderNumber(Integer orderNumber) {
		this.orderNumber = orderNumber;
	}

	public Integer getKeypersonTimesheetId() {
		return keypersonTimesheetId;
	}

	public void setKeypersonTimesheetId(Integer keypersonTimesheetId) {
		this.keypersonTimesheetId = keypersonTimesheetId;
	}

	public BigDecimal getValue() {
		return value;
	}

	public void setValue(BigDecimal value) {
		this.value = value;
	}

}
