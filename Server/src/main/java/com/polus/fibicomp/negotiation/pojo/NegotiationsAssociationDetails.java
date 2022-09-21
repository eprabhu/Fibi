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
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;

@Entity
@Table(name = "NEGOTIATION_ASSOC_DETAIL")
public class NegotiationsAssociationDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "negotiationAssocDetailIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "negotiationAssocDetailIdGenerator")
	@Column(name = "NEGOTIATIONS_ASSOC_DETAIL_ID")
	private Integer negotiationAssocDetailId;

	@Column(name = "NEGOTIATIONS_ASSOCIATION_ID")
	private Integer negotiationsAssociationId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK1"), name = "NEGOTIATIONS_ASSOCIATION_ID", referencedColumnName = "NEGOTIATIONS_ASSOCIATION_ID", insertable = false, updatable = false)
	private NegotiationsAssociation negotiationsAssociation;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK2"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "PI_PERSON_ID")
	private String personId;

	@Column(name = "PI_NAME")
	private String piName;

	@Column(name = "PI_ROLODEX_ID")
	private String rolodexId;

	@Column(name = "LEAD_UNIT")
	private String leadUnit;

	@JsonProperty(access = Access.AUTO)
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK3"), name = "LEAD_UNIT", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@JsonProperty(access = Access.AUTO)
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK4"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "PRIME_SPONSOR_CODE")
	private String primeSponsorCode;

	@JsonProperty(access = Access.AUTO)
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK5"), name = "PRIME_SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor primeSponsor;

	@Column(name = "SPONSOR_AWARD_NUMBER")
	private String sponsorAwardNumber;

	@Column(name = "CONTACT_ADMIN_PERSON_ID")
	private String contactAdminPersonId;

//	@JsonProperty(access = Access.READ_ONLY)
//	@ManyToOne(cascade = { CascadeType.ALL })
//	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK6"), name = "CONTACT_ADMIN_PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
//	private Person contactAdminPerson;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK6"), name = "CONTACT_ADMIN_PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person contactAdminPerson;

	@Column(name = "SUBAWARD_ORG")
	private String subAwardOrg;

	@JsonProperty(access = Access.AUTO)
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOC_DETAIL_FK7"), name = "SUBAWARD_ORG", referencedColumnName = "ORGANIZATION_ID", insertable = false, updatable = false)
	private Organization organization;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String acType;

	@Transient
	private String associationTypeCode;

	@Transient
	private String updateUserFullName;

	public Integer getNegotiationAssocDetailId() {
		return negotiationAssocDetailId;
	}

	public void setNegotiationAssocDetailId(Integer negotiationAssocDetailId) {
		this.negotiationAssocDetailId = negotiationAssocDetailId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(String rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getLeadUnit() {
		return leadUnit;
	}

	public void setLeadUnit(String leadUnit) {
		this.leadUnit = leadUnit;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public String getContactAdminPersonId() {
		return contactAdminPersonId;
	}

	public void setContactAdminPersonId(String contactAdminPersonId) {
		this.contactAdminPersonId = contactAdminPersonId;
	}

	public String getSubAwardOrg() {
		return subAwardOrg;
	}

	public void setSubAwardOrg(String subAwardOrg) {
		this.subAwardOrg = subAwardOrg;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public NegotiationsAssociation getNegotiationsAssociation() {
		return negotiationsAssociation;
	}

	public void setNegotiationsAssociation(NegotiationsAssociation negotiationsAssociation) {
		this.negotiationsAssociation = negotiationsAssociation;
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

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public Organization getOrganization() {
		return organization;
	}

	public void setOrganization(Organization organization) {
		this.organization = organization;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getNegotiationsAssociationId() {
		return negotiationsAssociationId;
	}

	public void setNegotiationsAssociationId(Integer negotiationsAssociationId) {
		this.negotiationsAssociationId = negotiationsAssociationId;
	}

	public String getPrimeSponsorCode() {
		return primeSponsorCode;
	}

	public void setPrimeSponsorCode(String primeSponsorCode) {
		this.primeSponsorCode = primeSponsorCode;
	}

	public Sponsor getPrimeSponsor() {
		return primeSponsor;
	}

	public void setPrimeSponsor(Sponsor primeSponsor) {
		this.primeSponsor = primeSponsor;
	}

	public String getAssociationTypeCode() {
		return "3";
	}

	public void setAssociationTypeCode(String associationTypeCode) {
		this.associationTypeCode = associationTypeCode;
	}

	public Person getContactAdminPerson() {
		return contactAdminPerson;
	}

	public void setContactAdminPerson(Person contactAdminPerson) {
		this.contactAdminPerson = contactAdminPerson;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}