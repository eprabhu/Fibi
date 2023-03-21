package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "NEGOTIATION_PERSONNEL")
public class NegotiationsPersonnel implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "negotiationsPersonIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "negotiationsPersonIdGenerator")
	@Column(name = "NEGOTIATION_PERSONNEL_ID")
	private Integer negotiationPersonnelId;
	
	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;
	
	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_PERSONNEL_FK2"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;
	
	@Column(name = "PERSONNEL_TYPE_CODE")
	private String personnelTypeCode;
	
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_PERSONNEL_FK1"), name = "PERSONNEL_TYPE_CODE", referencedColumnName = "PERSONNEL_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsPersonnelType negotiationsPersonnelType;
	
	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "FULL_NAME")
	private String fullName;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String acType;
	
	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public String getPersonnelTypeCode() {
		return personnelTypeCode;
	}

	public void setPersonnelTypeCode(String personnelTypeCode) {
		this.personnelTypeCode = personnelTypeCode;
	}

	public NegotiationsPersonnelType getNegotiationsPersonnelType() {
		return negotiationsPersonnelType;
	}

	public void setNegotiationsPersonnelType(NegotiationsPersonnelType negotiationsPersonnelType) {
		this.negotiationsPersonnelType = negotiationsPersonnelType;
	}

	public String getAcType() {
		if(acType == null) {
			return "U";
		}
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getNegotiationPersonnelId() {
		return negotiationPersonnelId;
	}

	public void setNegotiationPersonnelId(Integer negotiationPersonnelId) {
		this.negotiationPersonnelId = negotiationPersonnelId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}
	
}
