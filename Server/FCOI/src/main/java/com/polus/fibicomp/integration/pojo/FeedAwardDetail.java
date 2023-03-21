package com.polus.fibicomp.integration.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

@Entity
@Table(name = "FEED_AWARD_DETAILS")
public class FeedAwardDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "feedAwardDetailIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "feedAwardDetailIdGenerator")
	@Column(name = "ID")
	private Integer id;

	@Column(name = "PI_USER_NAME")
	private String piUserName;

	@Column(name = "PI_FULL_NAME")
	private String piFullName;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "SCENARIO")
	private String scenario;

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getPiUserName() {
		return piUserName;
	}

	public void setPiUserName(String piUserName) {
		this.piUserName = piUserName;
	}

	public String getPiFullName() {
		return piFullName;
	}

	public void setPiFullName(String piFullName) {
		this.piFullName = piFullName;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public String getScenario() {
		return scenario;
	}

	public void setScenario(String scenario) {
		this.scenario = scenario;
	}

}
