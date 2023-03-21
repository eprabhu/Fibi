package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "COUNTRY")
public class Country implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COUNTRY_CODE")
	private String countryCode;

	@Column(name = "COUNTRY_NAME")
	private String countryName;

	@Column(name = "CURRENCY_CODE")
	private String currencyCode;
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "COUNTRY_FK1"), name = "CURRENCY_CODE", referencedColumnName = "CURRENCY_CODE", insertable = false, updatable = false)
	private Currency currency;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "COUNTRY_CODE_ISO2")
	private String countryTwoCode;

	public Country() {
		super();
	}

	public Country(String countryCode, String countryName, String currencyCode, Currency currency) {
		super();
		this.countryCode = countryCode;
		this.countryName = countryName;
		this.currencyCode = currencyCode;
		this.currency = currency;
	}

	public Country(String countryCode, String countryName) {
		super();
		this.countryCode = countryCode;
		this.countryName = countryName;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public String getCountryName() {
		return countryName;
	}

	public void setCountryName(String countryName) {
		this.countryName = countryName;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public Currency getCurrency() {
		return currency;
	}

	public void setCurrency(Currency currency) {
		this.currency = currency;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getCountryTwoCode() {
		return countryTwoCode;
	}

	public void setCountryTwoCode(String countryTwoCode) {
		this.countryTwoCode = countryTwoCode;
	}

}
