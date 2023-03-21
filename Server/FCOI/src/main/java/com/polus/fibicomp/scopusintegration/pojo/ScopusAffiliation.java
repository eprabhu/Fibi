package com.polus.fibicomp.scopusintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "SCOPUS_AFFILIATION")
@EntityListeners(AuditingEntityListener.class)
public class ScopusAffiliation implements Serializable {

	private static final long serialVersionUID = 1L;

//	@Id
//	@Column(name = "AFFILIATION_ID")
//	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AFFILIATION")
//	@SequenceGenerator(name = "SEQ_AFFILIATION", sequenceName = "SEQ_AFFILIATION", allocationSize = 1)
//	private Integer affiliationId;

	@Id
	@Column(name = "AFF_ID")
	private String scopusAffId;

	@Id
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "SCOPUS_AFFILIATION_FK1"), name = "SCOPUS_ID", referencedColumnName = "SCOPUS_ID")
	private Scopus scopus;

	@Column(name = "URL")
	private String url;

	@Column(name = "CITY")
	private String city;

	@Column(name = "COUNTRY")
	private String country;

	@Column(name = "NAME")
	private String name;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Scopus getScopus() {
		return scopus;
	}

	public void setScopus(Scopus scopus) {
		this.scopus = scopus;
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

	public String getScopusAffId() {
		return scopusAffId;
	}

	public void setScopusAffId(String scopusAffId) {
		this.scopusAffId = scopusAffId;
	}

}
