package com.polus.fibicomp.pojo;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Table(name = "CURRENCY_RATE")
public class CurrencyRate implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "COUNTRY_CODE")
    private String countryCode;

    @ManyToOne(optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "COUNTRY_FK1"), name = "COUNTRY_CODE", referencedColumnName = "CURRENCY_CODE", insertable = false, updatable = false)
    private Country country;

    @Column(name = "CURRENCY_RATE")
    private Float currencyRate;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name ="UPDATE_TIMESTAMP")
    private Timestamp updateTimeStamp;

    public String getCountryCode() {
        return countryCode;
    }

    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }

    public Country getCountry() {
        return country;
    }

    public void setCountry(Country country) {
        this.country = country;
    }

    public Float getCurrencyRate() {
        return currencyRate;
    }

    public void setCurrencyRate(Float currencyRate) {
        this.currencyRate = currencyRate;
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
