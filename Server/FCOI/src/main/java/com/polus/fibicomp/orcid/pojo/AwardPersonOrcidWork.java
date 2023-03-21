package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;

@Entity
@Table(name = "AWARD_PERSON_ORCID_WORK")
public class AwardPersonOrcidWork implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_PERSON_ORCID_WORK_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_PERSON_ORCID_WORK_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_PERSON_ORCID_WORK_ID_GENERATOR", sequenceName = "AWARD_PERSON_ORCID_WORK_ID_GENERATOR", allocationSize=1)
	private Integer awardPersonOrcidWorkId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "PERSON_ORCID_WORK_ID")
	private Integer personOrcidWorkId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PERSON_ORCID_WORK_FK1"), name = "PERSON_ORCID_WORK_ID", referencedColumnName = "PERSON_ORCID_WORK_ID", insertable = false, updatable = false)
	private PersonOrcidWork personOrcidWork;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private ModuleDetails awardDetail;

	public Integer getAwardPersonOrcidWorkId() {
		return awardPersonOrcidWorkId;
	}

	public void setAwardPersonOrcidWorkId(Integer awardPersonOrcidWorkId) {
		this.awardPersonOrcidWorkId = awardPersonOrcidWorkId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getPersonOrcidWorkId() {
		return personOrcidWorkId;
	}

	public void setPersonOrcidWorkId(Integer personOrcidWorkId) {
		this.personOrcidWorkId = personOrcidWorkId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
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

	public PersonOrcidWork getPersonOrcidWork() {
		return personOrcidWork;
	}

	public void setPersonOrcidWork(PersonOrcidWork personOrcidWork) {
		this.personOrcidWork = personOrcidWork;
	}

	public ModuleDetails getAwardDetail() {
		return awardDetail;
	}

	public void setAwardDetail(ModuleDetails awardDetail) {
		this.awardDetail = awardDetail;
	}

}
