package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "CURRENCY")
public class Currency implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "CURRENCY_CODE")
	private String currencyCode;
	
	@Column(name = "CURRENCY")
	private String currency;
	
	@Column(name= "CURRENCY_SYMBOL")
	private String currencySymbol;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name ="UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Currency() {
		super();
	}

	public Currency(String currencyCode, String currency, String currencySymbol) {
		super();
		this.currencyCode = currencyCode;
		this.currency = currency;
		this.currencySymbol = currencySymbol;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public String getCurrency() {
		return currency;
	}

	public void setCurrency(String currency) {
		this.currency = currency;
	}

	public String getCurrencySymbol() {
		return currencySymbol;
	}

	public void setCurrencySymbol(String currencySymbol) {
		this.currencySymbol = currencySymbol;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

}
