package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.FundingScheme;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;

@Entity
@Table(name = "AWARD_ASSOC_DETAIL")
public class AwardAssociationDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_ASSOC_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardAssocDetailId;

	@JsonBackReference
	@OneToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK1"), name = "AWARD_ASSOCIATION_ID", referencedColumnName = "AWARD_ASSOCIATION_ID")
	private AwardAssociation awardAssociation;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "PI_PERSON_ID")
	private String personId;

	@Column(name = "PI_ROLODEX_ID")
	private String rolodexId;

	@Column(name = "PI_NAME")
	private String piName;

	@Column(name = "LEAD_UNIT")
	private String leadUnit;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK2"), name = "LEAD_UNIT", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK3"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "PRIME_SPONSOR_CODE")
	private String primeSponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK4"), name = "PRIME_SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor primeSponsor;

	@Column(name = "SPONSOR_AWARD_NUMBER")
	private String sponsorAwardNumber;

	@Column(name = "CONTACT_ADMIN_PERSON_ID")
	private String contactAdminPersonId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK5"), name = "CONTACT_ADMIN_PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person contactAdminPerson;

	@Column(name = "SUBAWARD_ORG")
	private String subAwardOrg;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK6"), name = "SUBAWARD_ORG", referencedColumnName = "ORGANIZATION_ID", insertable = false, updatable = false)
	private Organization organization;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "FUNDING_SCHEME_CODE")
	private String fundingSchemeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOC_DETAIL_FK7"), name = "FUNDING_SCHEME_CODE", referencedColumnName = "FUNDING_SCHEME_CODE", insertable = false, updatable = false)
	private FundingScheme fundingScheme;

	@Column(name = "STATUS_DESCRIPTION")
	private String statusDescription;

	@Column(name = "TOTAL_PROJECT_COST")
	private BigDecimal totalProjectCost = BigDecimal.ZERO;

	@Transient
	private String updateUserFullName;

	private transient String sponsorName;

	private transient String primeSponsorName;

	public Integer getAwardAssocDetailId() {
		return awardAssocDetailId;
	}

	public void setAwardAssocDetailId(Integer awardAssocDetailId) {
		this.awardAssocDetailId = awardAssocDetailId;
	}

	public AwardAssociation getAwardAssociation() {
		return awardAssociation;
	}

	public void setAwardAssociation(AwardAssociation awardAssociation) {
		this.awardAssociation = awardAssociation;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(String rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
	}

	public String getLeadUnit() {
		return leadUnit;
	}

	public void setLeadUnit(String leadUnit) {
		this.leadUnit = leadUnit;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
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

	public Person getContactAdminPerson() {
		return contactAdminPerson;
	}

	public void setContactAdminPerson(Person contactAdminPerson) {
		this.contactAdminPerson = contactAdminPerson;
	}

	public String getSubAwardOrg() {
		return subAwardOrg;
	}

	public void setSubAwardOrg(String subAwardOrg) {
		this.subAwardOrg = subAwardOrg;
	}

	public Organization getOrganization() {
		return organization;
	}

	public void setOrganization(Organization organization) {
		this.organization = organization;
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

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getFundingSchemeCode() {
		return fundingSchemeCode;
	}

	public void setFundingSchemeCode(String fundingSchemeCode) {
		this.fundingSchemeCode = fundingSchemeCode;
	}

	public FundingScheme getFundingScheme() {
		return fundingScheme;
	}

	public void setFundingScheme(FundingScheme fundingScheme) {
		this.fundingScheme = fundingScheme;
	}

	public String getStatusDescription() {
		return statusDescription;
	}

	public void setStatusDescription(String statusDescription) {
		this.statusDescription = statusDescription;
	}

	public BigDecimal getTotalProjectCost() {
		return totalProjectCost;
	}

	public void setTotalProjectCost(BigDecimal totalProjectCost) {
		this.totalProjectCost = totalProjectCost;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public String getPrimeSponsorName() {
		return primeSponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public void setPrimeSponsorName(String primeSponsorName) {
		this.primeSponsorName = primeSponsorName;
	}

}
